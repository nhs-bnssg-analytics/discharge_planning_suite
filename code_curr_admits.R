df_curr_admits <- local({
    if(seed) set.seed(123)
    require(tidyverse)
    require(workflows)
    require(recipes)
    
    # 1. INITIAL DATA PREP
    # Get only the patients we need and their attributes
    los_df <- nctr_sum %>%
      filter(ctr) %>%
      left_join(
        dplyr::select(attr_df, -sex, -age) %>% mutate(nhs_number = as.character(nhs_number)),
        by = join_by(nhs_number == nhs_number)
      ) %>%
      dplyr::select(age, sex, cambridge_score, bed_type, grp, spec, los_current = los)
    
    # 2. LOAD MODELS
    # Extract workflows early to keep objects clean
    rf_models <- readRDS("data/rf_fit_props_grp.RDS") %>% 
      mutate(fit = map(fit, "fit")) %>%
      mutate(wf = map(fit, extract_workflow)) %>%
      dplyr::select(grp, wf)
    
    los_wf_grp <- readRDS("data/los_wf_grp.RDS")
    los_dist <- readRDS("data/fit_dists_grp.RDS") %>%
      mutate(leaf = as.character(leaf)) %>%
      rename(los_hist = los)
    
    # 3. ASSIGN LEAVES (Done once per patient)
    # Pre-bake the data through the recipe
    los_df_baked <- recipes::bake(workflows::extract_recipe(los_wf_grp), los_df)
    
    los_df <- los_df %>%
      mutate(leaf = as.character(treeClust::rpart.predict.leaves(
        workflows::extract_fit_engine(los_wf_grp), 
        los_df_baked
      ))) %>%
      left_join(los_dist, by = "leaf")
    
    # 4. SIMULATION LOOP (Processing by Group to save RAM)
    # We process site-by-site so we don't have 2 million rows in RAM at once
    grps_to_process <- unique(los_df$grp)
    
    results <- map(grps_to_process, function(current_grp) {
      
      # Filter for this specific group
      site_data <- los_df %>% filter(grp == current_grp)
      if(nrow(site_data) == 0) return(NULL)
      
      # A. Sample LOS 'n_rep' times for every patient in this site
      site_sim <- site_data %>%
        mutate(
          sampled_los_tot = map2(los_hist, los_current, function(h, curr) {
            valid <- h[h >= curr]
            if(length(valid) < 5) return(rep(999, n_rep)) # Handle edge cases
            sample(valid, n_rep, replace = TRUE)
          }),
          rep = list(seq_len(n_rep))
        ) %>%
        dplyr::select(-los_hist) %>% # Drop historic vectors immediately to free GBs
        unnest(cols = c(sampled_los_tot, rep))
      
      # B. Format 'los' as expected by the RF model
      # We rename sampled_los_tot to 'los' because the RF expects that name
      site_sim <- site_sim %>%
        mutate(los = factor(map_chr(sampled_los_tot, \(x) cut(
          x,
          breaks = c(0, 3, 4, 5, 6, 7, 8, 9, 10, Inf),
          include.lowest = TRUE
        ))))
      
      # C. Run Pathway Prediction
      # Pull the specific model for this group
      current_wf <- rf_models %>% filter(grp == current_grp) %>% pull(wf) %>% pluck(1)
      
      # Predict probabilities
      preds <- predict(current_wf, site_sim, type = "prob")
      
      # D. Sample Pathways (Vectorized strategy)
      # Instead of pmap/sample, we use matrix math for speed and memory
      prob_mat <- as.matrix(preds)
      r_vals <- runif(nrow(site_sim))
      cum_probs <- matrixStats::rowCumsums(prob_mat)
      path_indices <- rowSums(r_vals > cum_probs) + 1
      
      site_sim$pathway <- factor(
        colnames(prob_mat)[path_indices], 
        levels = c(".pred_Other", ".pred_P1", ".pred_P2", ".pred_P3")
      )
      
      # E. Aggregate Site Results
      # Collapse to counts immediately to shrink the object
      site_summary <- site_sim %>%
        mutate(
          los_remaining = pmax(0, (sampled_los_tot - los_current) %/% 1),
          day = pmin(15, los_remaining + 1), # Cap day at simulation horizon
          pathway = str_remove(as.character(pathway), "\\.pred_") # Clean factor names
        ) %>%
        group_by(rep, grp, day, pathway) %>%
        summarise(count = n(), .groups = "drop")
      
      # Clean up memory within the loop
      rm(site_sim, preds, prob_mat, cum_probs)
      gc()
      
      return(site_summary)
    })
    
    # 5. COMBINE AND FINALIZE
    df_pred <- bind_rows(results) %>%
      mutate(pathway = factor(pathway, levels = c("Other", "P1", "P2", "P3"))) %>%
      complete(rep, grp, day = 1:11, pathway, fill = list(count = 0)) %>%
      filter(day <= 10) %>% # Keep only relevant horizon
      mutate(source = "current_admits")
    
    df_pred
  })