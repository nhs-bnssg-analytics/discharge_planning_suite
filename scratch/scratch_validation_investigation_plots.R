library(tidyverse)

out <- readRDS("data/final_validation_full_out_1e1_newpwmodel2.RDS")

bind_rows(
  out %>%
    map("result") %>%
    map("na_out_df") %>%
    bind_rows(.id = "id") %>%
    filter(site != "nbt") %>%
    complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
    select(id, site, day, pathway, source, n) %>%
    pivot_wider(values_from = n, names_from = source) %>%
    mutate(diff = observed - simulated, metric = "new_admits"),
  out %>%
    map("result") %>%
    map("ca_out_df") %>%
    bind_rows(.id = "id") %>%
    filter(site != "NBT") %>%
    complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
    select(id, site, day, pathway, source, n) %>%
    pivot_wider(values_from = n, names_from = source) %>%
    mutate(diff = observed - simulated, metric = "curr_admits")
) %>%
  summarise(
    simulated = sum(simulated),
    observed = sum(observed),
    .by = c(id, site, day)
  ) %>%
  mutate(diff = observed - simulated) %>%
  summarise(mean_diff = mean(diff),
            u95 = quantile(diff, 0.975),
            l95 = quantile(diff, 0.025), .by = c(site, day)) %>%
  filter(site != "system") %>%
  ggplot(aes(x = day, y = mean_diff)) +
  geom_point() +
  geom_errorbar(aes(ymin = l95, ymax = u95)) +
  facet_grid(site~.) 



  summarise(across(c(simulated, baseline), \(x) yardstick::rmse_vec(observed, x)), .by = c(site, day)) %>%
  mutate(ratio = baseline/simulated)%>%
  filter(site != "system") %>%
  ggplot(aes(x = as.numeric(day), y = ratio)) +
  geom_hline(yintercept = 1, linetype = 2) +
  scale_x_continuous(breaks = 1:10) +
  scale_colour_manual(values = c("green3", "red2")) +
  theme_minimal() +
  # geom_line() +
  ggforce::geom_link2(aes(colour = after_stat(
    ifelse(
      y > 1,
      "Model outperforms baseline",
      "Model underperforms baseline"
    )
  ))) +
  facet_grid(~site) +
  # ggh4x::facet_grid2(site ~ pathway, scales = "free_y", independent = "y") +
  labs(x = "Day",
       colour = "",
       y = str_wrap("RMSE ratio between baseline and simulation models", 50)) +
  theme(legend.position = "bottom")


bind_rows(
  out %>%
    map("result") %>%
    map("na_out_df") %>%
    bind_rows(.id = "id") %>%
    filter(site != "nbt") %>%
    complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
    select(id, site, day, pathway, source, n) %>%
    pivot_wider(values_from = n, names_from = source) %>%
    mutate(diff = observed - simulated, metric = "new_admits"),
  out %>%
    map("result") %>%
    map("ca_out_df") %>%
    bind_rows(.id = "id") %>%
    filter(site != "NBT") %>%
    complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
    select(id, site, day, pathway, source, n) %>%
    pivot_wider(values_from = n, names_from = source) %>%
    mutate(diff = observed - simulated, metric = "curr_admits")
) %>%
  summarise(
    simulated = sum(simulated),
    observed = sum(observed),
    .by = c(id, site, day)
  ) %>%
  left_join(
    out %>%
      map("result") %>%
      map("bl_out_df") %>%
      bind_rows(.id = "id") %>%
      mutate(day = factor(day, levels = 1:10)) %>%
      filter(site != "nbt") %>%
      complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
      rename(baseline = n) %>%
      summarise(baseline = sum(baseline), .by = c(id, site, day))
  ) %>%
  summarise(across(c(simulated, baseline), \(x) yardstick::rmse_vec(observed, x)), .by = c(site, day)) %>%
  mutate(ratio = baseline/simulated)%>%
  filter(site != "system") %>%
  ggplot(aes(x = as.numeric(day), y = ratio)) +
  geom_hline(yintercept = 1, linetype = 2) +
  scale_x_continuous(breaks = 1:10) +
  scale_colour_manual(values = c("green3", "red2")) +
  theme_minimal() +
  # geom_line() +
  ggforce::geom_link2(aes(colour = after_stat(
    ifelse(
      y > 1,
      "Model outperforms baseline",
      "Model underperforms baseline"
    )
  ))) +
  facet_grid(~site) +
  # ggh4x::facet_grid2(site ~ pathway, scales = "free_y", independent = "y") +
  labs(x = "Day",
       colour = "",
       y = str_wrap("RMSE ratio between baseline and simulation models", 50)) +
  theme(legend.position = "bottom")



bind_rows(
  out %>%
    map("result") %>%
    map("na_out_df") %>%
    bind_rows(.id = "id") %>%
    filter(site != "nbt") %>%
    complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
    select(id, site, day, pathway, source, n) %>%
    pivot_wider(values_from = n, names_from = source) %>%
    mutate(diff = observed - simulated, metric = "new_admits"),
  out %>%
    map("result") %>%
    map("ca_out_df") %>%
    bind_rows(.id = "id") %>%
    filter(site != "NBT") %>%
    complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
    select(id, site, day, pathway, source, n) %>%
    pivot_wider(values_from = n, names_from = source) %>%
    mutate(diff = observed - simulated, metric = "curr_admits")
) %>%
  summarise(
    simulated = sum(simulated),
    observed = sum(observed),
    .by = c(id, site, day, pathway)
  ) %>%
  left_join(
    out %>%
      map("result") %>%
      map("bl_out_df") %>%
      bind_rows(.id = "id") %>%
      mutate(day = factor(day, levels = 1:10)) %>%
      filter(site != "nbt") %>%
      complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
      rename(baseline = n)
  ) %>%
  mutate(date = min(date), .by = id) %>%
  summarise(across(c(simulated, baseline), \(x) yardstick::rmse_vec(observed, x)), .by = c(site, date, pathway)) %>%
  mutate(ratio = baseline/simulated)%>%
  filter(site != "system") %>%
  ggplot(aes(x = date, y = ratio)) +
  geom_hline(yintercept = 1, linetype = 2) +
  # scale_x_continuous(breaks = 1:10) +
  scale_colour_manual(values = c("green3", "red2")) +
  theme_minimal() +
  geom_line() +
  # ggforce::geom_link2(aes(colour = after_stat(
  #   ifelse(
  #     y > 1,
  #     "Model outperforms baseline",
  #     "Model underperforms baseline"
  #   )
  # ))) +
  facet_grid(site ~ pathway) +
  # ggh4x::facet_grid2(site ~ pathway, scales = "free_y", independent = "y") +
  labs(x = "Day",
       colour = "",
       y = str_wrap("RMSE ratio between baseline and simulation models", 50)) +
  theme(legend.position = "bottom")

bind_rows(
  out %>%
    map("result") %>%
    map("na_out_df") %>%
    bind_rows(.id = "id") %>%
    filter(site != "nbt") %>%
    complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
    select(id, site, day, pathway, source, n) %>%
    pivot_wider(values_from = n, names_from = source) %>%
    mutate(diff = observed - simulated, metric = "new_admits"),
  out %>%
    map("result") %>%
    map("ca_out_df") %>%
    bind_rows(.id = "id") %>%
    filter(site != "NBT") %>%
    complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
    select(id, site, day, pathway, source, n) %>%
    pivot_wider(values_from = n, names_from = source) %>%
    mutate(diff = observed - simulated, metric = "curr_admits")
) %>%
  summarise(
    simulated = sum(simulated),
    observed = sum(observed),
    .by = c(id, site, day, pathway)
  ) %>%
  left_join(
    out %>%
      map("result") %>%
      map("bl_out_df") %>%
      bind_rows(.id = "id") %>%
      mutate(day = factor(day, levels = 1:10)) %>%
      filter(site != "nbt") %>%
      complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
      rename(baseline = n)
  ) %>%
  mutate(date = min(date), .by = id) %>%
  summarise(across(c(simulated, baseline), \(x) yardstick::rmse_vec(observed, x)), .by = c(site, date)) %>%
  mutate(ratio = baseline/simulated)%>%
  filter(site != "system") %>%
  arrange(date) %>%
  ggplot(aes(x = date, y = ratio)) +
  geom_hline(yintercept = 1, linetype = 2) +
  # scale_x_continuous(breaks = 1:10) +
  scale_colour_manual(values = c("green3", "red2")) +
  theme_minimal() +
  # geom_line() +
  ggforce::geom_link2(aes(colour = after_stat(
    ifelse(
      y > 1,
      "Model outperforms baseline",
      "Model underperforms baseline"
    )
  ))) +
  facet_grid(~site) +
  # ggh4x::facet_grid2(site ~ pathway, scales = "free_y", independent = "y") +
  labs(x = "Day",
       colour = "",
       y = str_wrap("RMSE ratio between baseline and simulation models", 50)) +
  theme(legend.position = "bottom")


bind_rows(
  out %>%
    map("result") %>%
    map("na_out_df") %>%
    bind_rows(.id = "id") %>%
    filter(site != "nbt") %>%
    complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
    select(id, site, day, pathway, source, n) %>%
    pivot_wider(values_from = n, names_from = source) %>%
    mutate(diff = observed - simulated, metric = "new_admits"),
  out %>%
    map("result") %>%
    map("ca_out_df") %>%
    bind_rows(.id = "id") %>%
    filter(site != "NBT") %>%
    complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
    select(id, site, day, pathway, source, n) %>%
    pivot_wider(values_from = n, names_from = source) %>%
    mutate(diff = observed - simulated, metric = "curr_admits")
) %>%
  left_join(
    out %>%
      map("result") %>%
      map("na_out_df") %>%
      bind_rows(.id = "id") %>%
      mutate(date = min(date), .by = id) %>%
      select(id, date) %>%
      distinct()
  ) %>%
  summarise(
    simulated = sum(simulated),
    observed = sum(observed),
    .by = c(id, site, metric, date)
  ) %>%
  summarise(across(c(simulated), \(x) yardstick::rmse_vec(observed, x)), .by = c(site, metric, date)) %>%
  filter(site != "system") %>%
  arrange(date) %>%
  ggplot(aes(x = date, y = simulated)) +
  theme_minimal() +
  geom_line() +
  facet_grid(site~metric)
  
  

bind_rows(
  out %>%
    map("result") %>%
    map("na_out_df") %>%
    bind_rows(.id = "id") %>%
    filter(site != "nbt") %>%
    complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
    select(id, site, day, pathway, source, n) %>%
    pivot_wider(values_from = n, names_from = source) %>%
    mutate(diff = observed - simulated, metric = "new_admits"),
  out %>%
    map("result") %>%
    map("ca_out_df") %>%
    bind_rows(.id = "id") %>%
    filter(site != "NBT") %>%
    complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
    select(id, site, day, pathway, source, n) %>%
    pivot_wider(values_from = n, names_from = source) %>%
    mutate(diff = observed - simulated, metric = "curr_admits")
) %>%
  summarise(
    simulated = sum(simulated),
    observed = sum(observed),
    .by = c(id, site, day, pathway, metric)
  ) %>%
  mutate(diff = observed - simulated) %>%
  filter(site != "system") %>%
  summarise(mean_diff = mean(diff), u95 = quantile(diff, 0.975), l95 = quantile(diff, 0.025), .by = c(site, metric, day)) %>%
  ggplot(aes(x = day, y = mean_diff)) +
  geom_point() +
  geom_errorbar(aes(ymin = l95, ymax = u95)) +
  facet_grid(site~metric)
  
bind_rows(
  out %>%
    map("result") %>%
    map("na_out_df") %>%
    bind_rows(.id = "id") %>%
    filter(site != "nbt") %>%
    complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
    select(id, site, day, pathway, source, n) %>%
    pivot_wider(values_from = n, names_from = source) %>%
    mutate(diff = observed - simulated, metric = "new_admits"),
  out %>%
    map("result") %>%
    map("ca_out_df") %>%
    bind_rows(.id = "id") %>%
    filter(site != "NBT") %>%
    complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
    select(id, site, day, pathway, source, n) %>%
    pivot_wider(values_from = n, names_from = source) %>%
    mutate(diff = observed - simulated, metric = "curr_admits")
) %>%
  summarise(
    simulated = sum(simulated),
    observed = sum(observed),
    .by = c(id, site, day, pathway, metric)
  ) %>%
  mutate(diff = observed - simulated) %>%
  filter(site != "system") %>%
  summarise(mean_diff = mean(diff),
            u95 = quantile(diff, 0.975),
            l95 = quantile(diff, 0.025), .by = c(site, metric, pathway)) %>%
  ggplot(aes(x = pathway, y = mean_diff)) +
  geom_point() +
  geom_errorbar(aes(ymin = l95, ymax = u95)) +
  facet_grid(site~metric)
  

summarise(across(c(simulated), \(x) yardstick::rmse_vec(observed, x)), .by = c(site, day))





day_cap <- 3

bind_rows(
  out %>%
    map("result") %>%
    map("na_out_df") %>%
    bind_rows(.id = "id") %>%
    filter(site != "nbt") %>%
    complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
    select(id, site, day, pathway, source, n) %>%
    pivot_wider(values_from = n, names_from = source) %>%
    mutate(diff = observed - simulated, metric = "new_admits"),
  out %>%
    map("result") %>%
    map("ca_out_df") %>%
    bind_rows(.id = "id") %>%
    filter(site != "NBT") %>%
    complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
    select(id, site, day, pathway, source, n) %>%
    pivot_wider(values_from = n, names_from = source) %>%
    mutate(diff = observed - simulated, metric = "curr_admits")
) %>%
  filter(as.numeric(as.character(day)) <= day_cap) %>%
  summarise(
    simulated = sum(simulated),
    observed = sum(observed),
    .by = c(id, site, day, pathway)
  ) %>%
  left_join(
    out %>%
      map("result") %>%
      map("bl_out_df") %>%
      bind_rows(.id = "id") %>%
      mutate(day = factor(day, levels = 1:10)) %>%
      filter(site != "nbt") %>%
      complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
      rename(baseline = n)  %>%
      filter(as.numeric(as.character(day)) <= day_cap) %>%
      summarise(
        baseline = sum(baseline),
        .by = c(id, site, day, pathway)
      ) 
  ) %>%
  
  # summarise(across(c(simulated, baseline), \(x) yardstick::rmse_vec(observed, x)), .by = c(site, pathway)) %>%
  # mutate(ratio = baseline/simulated) %>%
  # filter(site != "system") %>%
  # ggplot(aes(x = pathway, y = ratio)) +
  # geom_hline(yintercept = 1, linetype = 2) +
  # geom_col() +
  # facet_wrap(vars(site)) 

  summarise(across(c(simulated, baseline), \(x) yardstick::rmse_vec(observed, x)), .by = c(site, day, pathway)) %>%
  mutate(ratio = baseline/simulated) %>%
  filter(site != "system") %>%
  ggplot(aes(x = day, y = ratio)) +
  geom_hline(yintercept = 1, linetype = 2) +
  # geom_col() +
  facet_wrap(vars(site)) +
  # scale_x_continuous(breaks = 1:10) +
  scale_colour_manual(values = c("green3", "red2")) +
  theme_minimal() +
  geom_line() +
  ggforce::geom_link2(aes(colour = after_stat(
    ifelse(
      y > 1,
      "Model outperforms baseline",
      "Model underperforms baseline"
    )
  ))) +
  facet_grid(site ~ pathway) +
  # ggh4x::facet_grid2(site ~ pathway, scales = "free_y", independent = "y") +
  labs(x = "Day",
       colour = "",
       y = str_wrap("RMSE ratio between baseline and simulation models", 50)) +
  theme(legend.position = "bottom")


bind_rows(
  out %>%
    map("result") %>%
    map("na_out_df") %>%
    bind_rows(.id = "id") %>%
    filter(site != "nbt") %>%
    complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
    select(id, site, day, pathway, source, n) %>%
    pivot_wider(values_from = n, names_from = source) %>%
    mutate(diff = observed - simulated, metric = "new_admits"),
  out %>%
    map("result") %>%
    map("ca_out_df") %>%
    bind_rows(.id = "id") %>%
    filter(site != "NBT") %>%
    complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
    select(id, site, day, pathway, source, n) %>%
    pivot_wider(values_from = n, names_from = source) %>%
    mutate(diff = observed - simulated, metric = "curr_admits")
) %>%
  filter(site != "system") %>%
  nest(.by = metric) %>%
  mutate(plot = map(data, \(x) x %>%
  summarise(
    simulated = sum(simulated),
    observed = sum(observed),
    .by = c(id, site, day)
  ) %>%
  summarise(
    sim_mean = mean(simulated),
    sim_u95 = quantile(simulated, 0.975),
    sim_l95 = quantile(simulated, 0.025),
    obs_mean = mean(observed),
    obs_u95 = quantile(observed, 0.975),
    obs_l95 = quantile(observed, 0.025),
    .by = c(site, day)) %>%
  ggplot(aes(x = as.numeric(day))) +
  geom_ribbon(aes(ymin = sim_l95, ymax = sim_u95, fill = "simulated"), alpha = 0.1) +
  geom_line(aes(y = sim_mean, col = "simulated")) +
  geom_ribbon(aes(ymin = obs_l95, ymax = obs_u95, fill = "observed"), alpha = 0.1) +
  geom_line(aes(y = obs_mean, col = "observed")) +
  facet_wrap(site~., nrow = 2, scales = "free"))) %>%
  mutate(plot = map2(plot, metric, \(x, y) x + labs(title = glue::glue("{y}")))) %>%
  pull(plot) %>%
  patchwork::wrap_plots(ncol = 2, axes = "collect", guides = "collect")
