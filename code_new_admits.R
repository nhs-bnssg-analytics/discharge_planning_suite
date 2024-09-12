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
    dplyr::select(site, date, fcast, u_85, l_85) %>%
    # - 1 day to include day 0 arivals
    filter(date >= report_start -ddays(1),
           date < report_end)
  
  sites <- unique(df_admit_fcast_flt$site)
  dates <- sort(unique(df_admit_fcast_flt$date))
  
  sim <- expand_grid(site = sites,
                     rep = seq_len(n_rep),
                     date = dates) %>%
    left_join(df_admit_fcast_flt, join_by(site, date == date)) %>%
    rowwise() %>%
    mutate(fcast_samp = rnorm(1, mean = fcast, sd = get_sd_from_ci(ci = c(l_85, u_85)))) %>%
    ungroup() %>%
    mutate(arrivals = coalesce(map_dbl(fcast_samp, rpois, n = 1), 0),
           # coalesce in case we sample below zero
           day = rep(0:n_days, length(sites) * n_rep)) %>% 
    filter(arrivals > 0) %>%
    # los plus one because it is stored as the left boundary of the interval censor (i.e. 0-2 should los 1)
    mutate(los = map(arrivals, function(arr) rdist(arr) + 1)) %>%
    unnest(los) %>%
    mutate(date_end = date + ddays(los)) %>%
    # day is + 1 to shift all predicted discharges to the next snapshot
    group_by(site, day = lubridate::interval(report_date, date_end)/ddays(1), rep) %>%
    count() %>%
    ungroup() %>%
    mutate(pathways = map(n, ~ factor(sample(names(props), size = .x, prob = props, replace = TRUE), levels = names(props))))  %>%
    mutate(pathways = map(pathways, table)) %>%
    unnest_wider(pathways) %>%
    dplyr::select(-n) %>%
    pivot_longer(
      cols = -c(site, day, rep),
      names_to = "pathway",
      values_to = "count"
    ) %>%
    mutate(count = as.numeric(count), source = "new_admits")
  sim
})