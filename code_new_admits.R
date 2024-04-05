df_new_admit <- local({
  rdist <- readRDS("data/fit_dists.RDS") %>%
    filter(leaf == -1) %>%
  pull(rdist) %>%
  `[[`(1)
  
  props <- readRDS("data/pathway_prop.RDS") %>%
    set_names(c("Other", "P1", "P2", "P3"))
  
  # daily A&E forecasts to be used later
  df_admit_fcast_flt <- df_admit_fcast %>%
    pivot_wider(names_from = metric, values_from = value) %>%
    # filter(!is.na(fcast)) %>%
    dplyr::select(site, date, fcast, u_95, l_95) %>%
    filter(date >= report_start,
           date <= report_end)
  
  sites <- unique(df_admit_fcast_flt$site)
  dates <- sort(unique(df_admit_fcast_flt$date))
  
  sim <- expand_grid(site = sites,
                     rep = seq_len(n_rep),
                     date = dates) %>%
    left_join(df_admit_fcast_flt, join_by(site, date == date)) %>%
    rowwise() %>%
    mutate(fcast_samp = rnorm(1, mean = fcast, sd = get_sd_from_ci(ci = c(l_95, u_95)))) %>%
    ungroup() %>%
    mutate(arrivals = coalesce(map_dbl(fcast_samp, rpois, n = 1), 0),
           # coalesce in case we sample below zero
           day = rep(0:n_days, length(sites) * n_rep)) %>% # adding hour column, for grouping/stats later (3 sites x 1000 reps)
    filter(arrivals > 0) %>%
    mutate(los = map(arrivals, function(arr) rdist(arr))) %>%
    unnest(los) %>%
    mutate(date_end = date + ddays(los)) %>%
    group_by(site, day = (date_end - (report_start)) / ddays(1), rep) %>%
    count() %>%
    ungroup() %>%
    mutate(pathways = map(n, ~ factor(sample(names(props), size = .x, prob = props, replace = TRUE), levels = names(props))))  %>%
    mutate(pathways = map(pathways, table)) %>%
    unnest_wider(pathways) %>%
    select(-n) %>%
    pivot_longer(
      cols = -c(site, day, rep),
      names_to = "pathway",
      values_to = "count"
    ) %>%
    mutate(count = as.numeric(count), source = "new_admits")
  sim
})