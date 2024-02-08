df_new_admit <- local({
  params <- readRDS("data/dist_split.RDS") %>%
    `[[`("-1")
  
  props <- readRDS("data/pathway_prop.RDS") %>%
    set_names(c("Other", "P1", "P2", "P3"))
  
  # daily A&E forecasts to be used later
  df_admit_fcast_flt <- df_admit_fcast %>%
    pivot_wider(names_from = metric, values_from = value) %>%
    # filter(!is.na(fcast)) %>%
    dplyr::select(site, date, fcast, u_95, l_95) %>%
    filter(date > report_start,
           date <= report_end)
  
  sites <- unique(df_admit_fcast_flt$site)
  dates <- sort(unique(df_admit_fcast_flt$date))
  
  sim <- expand_grid(site = sites,
                     rep = seq_len(n_rep),
                     date = dates) %>%
    left_join(df_admit_fcast_flt, join_by(site, date == date)) %>%
    rowwise() %>%
    mutate(fcast_samp = rnorm(1, mean = fcast, sd = get_sd_from_ci(ci = c(l_95, u_95)))) %>%
    # duplicate each row 24 times (each day is broken in to 24 hours and then
    # scaled by the appropriate proportion)
    ungroup() %>%
    mutate(arrivals = coalesce(map_dbl(fcast_samp, rpois, n = 1), 0),
           # coalesce in case we sample below zero
           day = rep(1:n_days, length(sites) * n_rep)) %>% # adding hour column, for grouping/stats later (3 sites x 1000 reps)
    
    mutate(los = map(arrivals, ~ round(
      rlnorm(..1, meanlog = params[["meanlog"]], sdlog = params[["sdlog"]])
    ))) %>%
    unnest(los) %>%
    mutate(date_end = date + ddays(los)) %>%
    group_by(site, day = (date_end - report_start) / ddays(1), rep) %>%
    count() %>%
    ungroup() %>%
    mutate(pathways = map(n, ~ .x * props) %>% bind_rows())  %>%
    unnest(pathways) %>%
    select(-n) %>%
    pivot_longer(
      cols = -c(site, day, rep),
      names_to = "pathway",
      values_to = "count"
    ) %>%
    mutate(source = "new_admits")
  
  
  sim
})
  
  # complete(site, day, rep, fill = list(n = 0)) %>%
  # mutate(pathway = map(n,
  #                      ~sample(size = .x, names(props), prob = props, replace = TRUE) #%>%
  #                       # table() %>%
  #                       # rbind() %>%
  #                       # as_tibble()
  #                      )
  #        
  #        )


# samp <- sample(size = sum(foo$n), names(props), prob = props, replace = TRUE)
# samp <- split(samp, rep(seq_along(foo$n), times = foo$n))
# 
# bar <- map(samp, ~table(.x)) %>%
#   bind_rows()
# 
# 
# map(samp, ~table(.x) %>% rbind() %>% as_tibble())
# 
# %>%
#   group_by(site, date) %>%
#   summarise(count = mean(n),
#             u95 = quantile(n, 0.95),
#             l95 = quantile(n, 0.05),
#             pathways = pmap(list(count, u95, l95), ~list(count = ..1*props, u95 = ..2*props, l95 = ..3*props)),
#             pathways = map(pathways, function(x) map(x, ~bind_rows(.x)) %>%
#                              bind_rows() %>%
#                              mutate(stat = c("mean", "u95", "l95")) %>%
#                              pivot_wider(names_from = stat, values_from = -c(stat))
#             )
#             ) %>%
#   unnest_wider(pathways)
# 
# 
# 
# map(foo$pathways[1], ~bind_rows(.x) %>%
#       mutate(stat = c("mean", "u95", "l95")) %>%
#       pivot_wider(names_from = stat, values_from = -c(stat))
#     )
# 
# map(foo$pathways, function(x) map(x, ~bind_rows(.x)) %>%
#   bind_rows() %>%
#   mutate(stat = c("mean", "u95", "l95")) %>%
#   pivot_wider(names_from = stat, values_from = -c(stat))
# ) %>%
#   bind_rows()
# 
# 


  
  # %>%
  #   mutate(los_end = day + los) %>%
  #   dplyr::select(site, rep, los_end, day) %>%
  #   group_by(site, rep) %>%
  #   summarise(n = list(matrix(sweep_fn(n_days, day, los_end), nrow = 1)))
  
#   
#   tictoc::toc()
#   saveRDS(sim, "data/new_ae.RDS")
#   if(save_int) {
#     sim %>%
#       group_by(site) %>%
#       summarise(n = list(reduce(n, rbind))) %>%
#       mutate(
#         mean = map(n, colMeans),
#         u_80 = map(n, ~ map_dbl(array_branch(.x, margin = 2), ~ quantile(.x, .9))),
#         l_80 = map(n, ~ map_dbl(array_branch(.x, margin = 2), ~ quantile(.x, .1))),
#         u_95 = map(n, ~ map_dbl(array_branch(.x, margin = 2), ~ quantile(.x, .975))),
#         l_95 = map(n, ~ map_dbl(array_branch(.x, margin = 2), ~ quantile(.x, .025)))
#       ) %>%
#       mutate(frame = pmap(list(date = list(seq(report_start + ddays(1), report_end, by = "days")),
#                                mean = mean,
#                                u_80 = u_80,
#                                l_80 = l_80,
#                                u_95 = u_95,
#                                l_95 = l_95
#       ),
#       bind_cols)) %>%
#       dplyr::select(site, frame) %>%
#       unnest(cols = c(frame)) %>%
#       pivot_longer(cols = -c(site, date),
#                    names_to = "metric",
#                    values_to = "value") %>%
#       mutate(tag = "occ_ae_adm",
#              report_date = report_start) %>% 
#       saveRDS("data/df_new_ae_sql.RDS")
#   }
#   sim
# })