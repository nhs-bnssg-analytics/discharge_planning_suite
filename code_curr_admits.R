df_curr_admits <- local({

  # LOS predictions
  
  los_df <- nctr_sum %>%
    # filter for our main sites / perhaps I shouldn't do this?
    # filter(Organisation_Site_Code %in% c('RVJ01', 'RA701', 'RA301', 'RA7C2')) %>%
    # filter for CTR, we wont predict the NCTR outcome for those already NCTR/on a queue
    filter(ctr == "Y")
  
  
  # attributes
  
  attr_df <-
    RODBC::sqlQuery(
      con,
      "select * from (
select a.*, ROW_NUMBER() over (partition by nhs_number order by attribute_period desc) rn from
[MODELLING_SQL_AREA].[dbo].[New_Cambridge_Score] a) b where b.rn = 1"
    )
  
  los_df <- los_df %>%
    left_join(attr_df, by = join_by(nhs_number == nhs_number)) %>%
    dplyr::select(age, sex, cambridge_score, bed_type, site, los) %>%
    na.omit()
  
  # pathway model
  
  rf_wf <- readRDS("data/rf_wf.RDS")
  
  # los_tree
  
  los_wf <- readRDS("data/los_wf.RDS")
  
  los_df <- los_df %>%
    bake(extract_recipe(los_wf), .) %>%
    mutate(leaf = as.character(treeClust::rpart.predict.leaves(extract_fit_engine(los_wf), .))) %>%
    # bind RF pathway predicted probabilities
    bind_cols(predict(rf_wf, los_df, type = "prob"))
  
  # los distributions
  
  los_dist <- readRDS("data/dist_split.RDS") %>%
    enframe() %>%
    unnest_wider(value)
  
  
  df_pred <- los_df %>%
    mutate(id = 1:n()) %>%
    left_join(los_dist, by = join_by(leaf == name)) %>%
    mutate(los_remaining = pmap(
      list(los, meanlog, sdlog),
      ~
        rlnormt(
          n_rep,
          meanlog = ..2,
          sdlog = ..3,
          range = c(..1, Inf)
        ) - ..1
    )) %>%
    dplyr::select(id, site, los_remaining, starts_with(".pred")) %>%
    unnest(los_remaining) %>%
    mutate(los_remaining = ifelse(los_remaining < 0, 0, los_remaining)) %>%
    mutate(los_remaining = los_remaining %/% 1) %>%
    group_by(site, id) %>%
    mutate(rep = 1:n()) %>%
    group_by(rep, site, day = los_remaining) %>%
    summarise(across(starts_with(".pred"), list(count = {
      \(x) sum(x)
    }))) %>%
    rename_with(.fn = \(x) str_replace_all(x, pattern = ".pred_|_count", "")) %>%
    pivot_longer(
      cols = -c(site, day, rep),
      names_to = "pathway",
      values_to = "count"
    ) %>%
    mutate(source = "current_admits")
  df_pred
  
})