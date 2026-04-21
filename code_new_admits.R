df_new_admit <- local({ 
  
  if(seed) set.seed(123)
  # report_date <- report_start -ddays(1) # (DEPRECATED)
  report_date <- report_start
  

  rdist <- readRDS("data/fit_dists_grp.RDS") %>%
    filter(leaf == -1) %>%
    pull(los) %>% 
    pluck(1) %>% 
    {
      targets <- .[names(.) %in% c("weston", "bri")]
      c(., set_names(targets, rep("nbt", length(targets))))
    } %>% 
    split(names(.)) %>%
    map(~partial(EnvStats::remp, obs = .x)) #%>%
    # enframe(name = "grp", value = "rdist")
  
  
  
  props_grp <- readRDS("data/rf_fit_props_grp.RDS") %>%
    mutate(props = map(fit, "props")) %>%
    mutate(props = map(props, \(prop) map(prop, ~ set_names(
      .x, c("Other", "P1", "P2", "P3")
    )))) %>%
    dplyr::select(grp, props)  %>%
    unnest_wider(props) %>%
    pivot_longer(-grp, names_to = "los") %>%
    unnest_wider(value) %>%
    pivot_longer(-c(grp, los), names_to = "pathway") %>%
    nest(.by = c(grp, los), .key = "props") %>%
    mutate(props = map(props, \(data) mutate(data, value = set_names(value, pathway)) %>% pull(value)))

  
  # daily A&E forecasts to be used later
  df_admit_fcast_flt <- df_admit_fcast %>%
    pivot_wider(names_from = metric, values_from = value) %>%
    # filter(!is.na(fcast)) %>%
    dplyr::select(grp, date, fcast, u_85, l_85) %>%
    # minus 1 day to include day 0 arrivals
    filter(date >= report_start -ddays(1),
           date < report_end)
  
  grps <- unique(df_admit_fcast_flt$grp)
  dates <- sort(unique(df_admit_fcast_flt$date))
  
  
  sim <- expand_grid(
    grp = grps,
    rep = seq_len(n_rep),
    date = dates
  ) %>%
    left_join(df_admit_fcast_flt, by = c("grp", "date")) %>%
    mutate(
      calc_sd = pmax((u_85 - l_85) / 2.88, 0.001, na.rm = TRUE), 
      arrivals = pmax(0, round(extraDistr::rdnorm(n(), mean = fcast + 0.5, sd = calc_sd))),
      day = as.numeric(date - report_date, units = "days")
    ) %>%
    filter(arrivals > 0) %>%
    mutate(los_list = map2(arrivals, grp, function(arr, g) {
      dist_fn <- rdist[[g]] 
      if(is.null(dist_fn)) return(rep(1, arr)) # Fallback
      dist_fn(arr)
    })) %>%
    unnest(los_list) %>%
    mutate(
      date_end = date + ddays(los_list),
      sim_day = pmax(1, as.numeric(date_end - report_date, units = "days") + 1)
    ) %>%
    filter(sim_day <= n_days) %>% # Drop anything beyond our forecast window early
    left_join(props_grp, by = "grp") %>% 
    group_by(grp, sim_day, rep, props) %>%
    summarise(total_arrivals = n(), .groups = "drop") %>%
    mutate(pathway_counts = map2(total_arrivals, props, function(n_total, p_vec) {
      counts <- rmultinom(1, size = n_total, prob = p_vec)
      tibble(
        pathway = names(p_vec),
        count = as.vector(counts)
      )
    })) %>%
    select(grp, day = sim_day, rep, pathway_counts) %>%
    unnest(pathway_counts) %>%
    complete(grp, day, rep, pathway, fill = list(count = 0)) %>%
    mutate(source = "new_admits")
  sim
})
