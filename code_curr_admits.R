df_curr_admits <- local({
  
  if(seed) set.seed(123)
  require(tidyverse)
  # LOS predictions
  los_df <- nctr_sum %>%
    # filter for our main sites / perhaps I shouldn't do this?
    # filter(Organisation_Site_Code %in% c('RVJ01', 'RA701', 'RA301', 'RA7C2')) %>%
    # filter for CTR, we wont predict the NCTR outcome for those already NCTR/on a queue
    filter(ctr)
  
  
  los_df <- los_df %>%
    left_join(
      dplyr::select(attr_df, -sex, -age) %>%
        mutate(nhs_number = as.character(nhs_number)),
      by = join_by(nhs_number == nhs_number)
    ) %>%
    dplyr::select(age, sex, cambridge_score, bed_type, site, los) #%>%
  #na.omit()
  
  # pathway model
  
  rf_wf <- readRDS("data/rf_wf.RDS")
  
  # los_tree
  
  los_wf <- readRDS("data/los_wf.RDS")
  
  los_df <- los_df %>%
    recipes::bake(workflows::extract_recipe(los_wf), .) %>%
    mutate(leaf = as.character(treeClust::rpart.predict.leaves(workflows::extract_fit_engine(los_wf), .))) %>%
    # bind RF pathway predicted probabilities
    bind_cols(predict(rf_wf, los_df, type = "prob")) %>%
    mutate(site = fct_drop(site))
  
  # los distributions
  
  los_dist <- readRDS("data/fit_dists.RDS")  %>%
    mutate(leaf = as.character(leaf)) %>%
    rename(los_hist = los)
  
  
  
  df_pred <- los_df %>%
    mutate(id = 1:n()) %>%
    left_join(los_dist, by = join_by(leaf == leaf)) %>%
    # filter out any LOS less than current stay
    mutate(los_hist = map2(los_hist, los, \(x, y) x[x >= y])) %>%
    # If los history is now zero we add Inf
    mutate(los_hist = map(los_hist, \(x) ifelse(length(x) < 10, list(Inf), list(x)))) %>%
    mutate(los_hist = map(los_hist, pluck, 1)) %>%
    # mutate(los_dist = map(los_hist, ~partial(EnvStats::remp, obs = .x))) %>%
    # mutate(los_tot = map(los_dist,  ~.x(n_rep)))
    mutate(los_tot = map(los_hist,  \(x) sample(x, size = n_rep, replace = TRUE))) %>%
    mutate(los_remaining = map2(los_tot, los, \(x, y) x - y)) %>%
    mutate( pathways = pmap(list(.pred_Other,
                         .pred_P1,
                         .pred_P2,
                         .pred_P3), 
                    ~factor(sample(c("Other", "P1", "P2", "P3"),
                            n_rep,
                            prob = c(..1, ..2, ..3, ..4),
                            replace = TRUE)),
                    levels = c("Other", "P1", "P2", "P3"))) %>%
    dplyr::select(id, site, los_remaining, pathways) %>%
    unnest(cols = c(los_remaining, pathways)) %>%
    mutate(los_remaining = ifelse(los_remaining < 0, 0, los_remaining)) %>%
    mutate(los_remaining = los_remaining %/% 1) %>%
    group_by(site, id) %>%
    mutate(rep = 1:n()) %>%
    # group_by(rep, site, day = los_remaining + 1, pathway = pathways) %>% # (DEPRECATED - this was when I considered sim to start on 'day zero')
    group_by(rep, site, day = los_remaining + 1, pathway = pathways) %>% # day is los_remaining + 1 as we start sim on 'day one' so zero day los on first day should be day = 1
    count(name = "count") %>%
    ungroup() %>%
    complete(rep, site, day, pathway, fill =list(count = 0)) %>%
    mutate(source = "current_admits")
  df_pred
})
    