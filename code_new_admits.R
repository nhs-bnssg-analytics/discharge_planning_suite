df_new_admit <- local({ 
  
  if(seed) set.seed(123)
  # report_date <- report_start -ddays(1) # (DEPRECATED)
  report_date <- report_start
  
  rdist <- readRDS("data/fit_dists.RDS") %>%
    filter(leaf == -1) %>%
    pull(los) %>% 
    pluck(1) %>%
    c(., set_names(., rep("nbt", times = length(.)))) %>% # bind all rows together to make global dist for nbt
    split(names(.)) %>%
    map(~partial(EnvStats::remp, obs = .x)) %>%
    enframe(name = "site", value = "rdist")
    
  
  # props <- readRDS("data/pathway_prop.RDS") %>%
  #   set_names(c("Other", "P1", "P2", "P3"))
  
   props_site <- readRDS("data/rf_fit_props_site.RDS") %>% 
     mutate(props = map(fit, "props")) %>%
     mutate(props = map(props, \(prop) map(prop, ~set_names(.x, c("Other", "P1", "P2", "P3"))))) %>%
     dplyr::select(site, props)  %>%
     unnest_wider(props) %>%
     pivot_longer(-site, names_to = "los") %>%
     unnest_wider(value) %>%
     pivot_longer(-c(site, los), names_to = "pathway") %>%
     nest(.by = c(site, los), .key = "props") %>%
     mutate(props = map(props, \(data) mutate(data, value = set_names(value, pathway)) %>% pull(value)))
   
   
  # daily A&E forecasts to be used later
  df_admit_fcast_flt <- df_admit_fcast %>%
    pivot_wider(names_from = metric, values_from = value) %>%
    # filter(!is.na(fcast)) %>%
    dplyr::select(site, date, fcast, u_85, l_85) %>%
    # minus 1 day to include day 0 arrivals
    filter(date >= report_start -ddays(1),
           date < report_end)
  
  sites <- unique(df_admit_fcast_flt$site)
  dates <- sort(unique(df_admit_fcast_flt$date))
  
  sim <- expand_grid(site = sites,
                     rep = seq_len(n_rep),
                     date = dates) %>%
    left_join(df_admit_fcast_flt, join_by(site, date == date)) %>%
    rowwise() %>%
    mutate(#fcast_samp = rnorm(1, mean = fcast, sd = get_sd_from_ci(ci = c(l_85, u_85))),
           # arrivals = pmax(extraDistr::rdnorm(1, mean = (1.16*fcast)+0.5),0)
           arrivals = pmax(extraDistr::rdnorm(1, mean = (fcast)+0.5, sd = get_sd_from_ci(ci = c(l_85, u_85))))
           ) %>%
    ungroup() %>%
    mutate(
     # arrivals = coalesce(map_dbl(fcast_samp, rpois, n = 1), 0),
           # coalesce in case we sample below zero
           day = rep(0:n_days, length(sites) * n_rep)) %>% 
    filter(arrivals > 0) %>%
    left_join(rdist) %>%
    # (DEPRECATED) los plus one because it is stored as the left boundary of the interval censor (i.e. 0-2 should los 1)
    # mutate(los = map(arrivals, function(arr) rdist(arr) + 1)) %>%
    mutate(los = map2(arrivals, rdist, function(arr, dist_fn) dist_fn(arr))) %>%
    # mutate(los = map(arrivals, function(arr) rdist(arr)) %>% map(\(x) map_dbl(x, ~sample(seq(.x, .x + 2), 1)))) %>%
    unnest(los) %>%
    mutate(date_end = date + ddays(los)) %>%
    mutate(los = cut(los, breaks = c(0, 3, 4, 5, 6, 7,  8, 9, 10, Inf), include.lowest = TRUE)) %>%
    left_join(props_site) %>%
    # DEPRECATED CHUNK #
    # # mutate(date_end = date + ddays(pmax(los-1, 0))) %>%
    # #(DEPRECATED) day is + 1 to shift all predicted discharges to the next snapshot
    # # group_by(site, day = lubridate::interval(report_date, date_end)/ddays(1), rep) %>%
    # # day is + 1 as an interval of zero for new arrivals on day x that are NCTR on x should counted on should be on day x + 1 (day 1 events are on day 1 etc)
    # # group_by(site, day = lubridate::interval(report_date, date_end)/ddays(1) + 1, rep) %>%
    
    # pmax here to keep the (small) numbers discharged on day zero in the sim
    # and we add one because interval of zero corresponds to day 'one' of the
    # simulation
    group_by(site, day = pmax(1, lubridate::interval(report_date, date_end)/ddays(1) + 1), rep, los, props) %>%
    count() %>%
    ungroup() %>%
    mutate(pathways = map2(n, props, ~ factor(sample(names(.y),
                                                     size = .x,
                                                     prob = .y,
                                                     replace = TRUE),
                                              levels = names(.y)))) %>%
    unnest(pathways) %>%
    group_by(site, day, rep, pathway = pathways) %>%
    count(name = "count") %>%
    ungroup() %>%
    complete(site, day, rep, pathway, fill = list(count = 0)) %>%
    mutate(source = "new_admits")
  sim
})
