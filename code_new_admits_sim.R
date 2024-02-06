df_new_admit <- local({

  # daily A&E forecasts to be used later
  df_admit_fcast_flt <- df_admit_fcast %>%
    pivot_wider(names_from = metric, values_from = value) %>%
    # filter(!is.na(fcast)) %>%
    dplyr::select(site, date, fcast, u_95, l_95) %>%
    filter(date > report_start,
           date <= report_end)
  
  sites <- unique(df_admit_fcast_flt$site)
  
  sim <- expand_grid(site = sites,
                     rep = seq_len(n_rep),
                     date = df_admit_fcast_flt$date[1:10]) %>%
    left_join(df_admit_fcast_flt, join_by(site, date == date)) %>%
    rowwise() %>%
    mutate(fcast_samp = rnorm(1, mean = fcast, sd = get_sd_from_ci(ci = c(l_95, u_95)))) %>%
    # duplicate each row 24 times (each day is broken in to 24 hours and then
    # scaled by the appropriate proportion)
    ungroup() %>%
    mutate(arrivals = map_dbl(fcast_samp, rpois, n = 1),
           day = rep(1:n_days, length(sites)*n_rep)) %>% # adding hour column, for grouping/stats later (3 sites x 1000 reps)
    
    mutate(los = pmap(list(arrivals, site), ~round(rlnorm(..1, meanlog = sample(params[[..2]]$meanlog, 1), sdlog = sample(params[[..2]]$sdlog, 1))))) %>%
    unnest(los) %>%
    filter(los > 24) %>%
    mutate(los_end = hour + los) %>%
    dplyr::select(site, rep, hour, los_end) %>%
    group_by(site, rep) %>%
    summarise(count = list(matrix(sweep_fn(sim_length, hour, los_end), nrow = 1)))
  tictoc::toc()
  saveRDS(sim, "data/new_ae.RDS")
  if(save_int) {
    sim %>%
      group_by(site) %>%
      summarise(count = list(reduce(count, rbind))) %>%
      mutate(
        mean = map(count, colMeans),
        u_80 = map(count, ~ map_dbl(array_branch(.x, margin = 2), ~ quantile(.x, .9))),
        l_80 = map(count, ~ map_dbl(array_branch(.x, margin = 2), ~ quantile(.x, .1))),
        u_95 = map(count, ~ map_dbl(array_branch(.x, margin = 2), ~ quantile(.x, .975))),
        l_95 = map(count, ~ map_dbl(array_branch(.x, margin = 2), ~ quantile(.x, .025)))
      ) %>%
      mutate(frame = pmap(list(date = list(seq(report_start + dhours(1), report_end, by = "hours")),
                               mean = mean,
                               u_80 = u_80,
                               l_80 = l_80,
                               u_95 = u_95,
                               l_95 = l_95
      ),
      bind_cols)) %>%
      dplyr::select(site, frame) %>%
      unnest(cols = c(frame)) %>%
      pivot_longer(cols = -c(site, date),
                   names_to = "metric",
                   values_to = "value") %>%
      mutate(tag = "occ_ae_adm",
             report_date = report_start) %>% 
      saveRDS("data/df_new_ae_sql.RDS")
  }
  sim
})