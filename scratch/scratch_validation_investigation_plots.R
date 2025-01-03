library(tidyverse)

out <- readRDS("data/final_validation_full_out_1e1_newpwmodel_hackednewadmits_newvalidlogic.RDS")

discharge_plots <- local({
  out_df <- bind_rows(
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
  )
  
  p1 <- out_df %>%
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
    annotate(geom = "rect", xmin = -Inf, xmax = Inf, ymin = -2.5, ymax = 2.5, alpha = 0.25) + 
    geom_hline(yintercept = 0, linetype = 2) +
    geom_point() +
    geom_errorbar(aes(ymin = l95, ymax = u95)) +
    facet_grid(site~.) +
    labs(title = "Discharge residuals per site", y = "Observed minus simulated discharges")
  
  p2 <- out_df %>%
  summarise(
    simulated = sum(simulated),
    observed = sum(observed),
    .by = c(id, site, pathway, day)
  ) %>%
    mutate(diff = observed - simulated) %>%
    summarise(mean_diff = mean(diff),
              u95 = quantile(diff, 0.975),
              l95 = quantile(diff, 0.025), .by = c(site, pathway, day)) %>%
    filter(site != "system") %>%
    ggplot(aes(x = day, y = mean_diff)) +
    annotate(geom = "rect", xmin = -Inf, xmax = Inf, ymin = -2.5, ymax = 2.5, alpha = 0.25) +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_point() +
    geom_errorbar(aes(ymin = l95, ymax = u95)) +
    ggh4x::facet_grid2(site~pathway, independent = "y", scales = "free") +
    labs(title = "Discharges per site/pathway", y = "Observed minus simulated discharges")
  
  p3 <- out_df %>%
  summarise(
    simulated = sum(simulated),
    observed = sum(observed),
    .by = c(id, site, metric, day)
  ) %>%
    mutate(diff = observed - simulated) %>%
    summarise(mean_diff = mean(diff),
              u95 = quantile(diff, 0.975),
              l95 = quantile(diff, 0.025), .by = c(site, metric, day)) %>%
    filter(site != "system") %>%
    ggplot(aes(x = day, y = mean_diff)) +
    annotate(geom = "rect", xmin = -Inf, xmax = Inf, ymin = -2.5, ymax = 2.5, alpha = 0.25) +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_point() +
    geom_errorbar(aes(ymin = l95, ymax = u95)) +
    facet_grid(site~metric) +
    labs(title = "Discharges per site vs source", y = "Observed minus simulated discharges")
  
  p4 <- out_df %>%
  summarise(
    simulated = sum(simulated),
    observed = sum(observed),
    .by = c(id, site, pathway, metric, day)
  ) %>%
    mutate(diff = observed - simulated) %>%
    summarise(mean_diff = mean(diff),
              u95 = quantile(diff, 0.975),
              l95 = quantile(diff, 0.025), .by = c(site, pathway, metric, day)) %>%
    filter(site != "system") %>%
    nest(.by = metric) %>%
    mutate(plot = map2(data, metric, ~{ .x %>%
      ggplot(aes(x = day, y = mean_diff)) +
      annotate(geom = "rect", xmin = -Inf, xmax = Inf, ymin = -2.5, ymax = 2.5, alpha = 0.25) +
      geom_hline(yintercept = 0, linetype = 2) +
      geom_point() +
      geom_errorbar(aes(ymin = l95, ymax = u95)) +
      ggh4x::facet_grid2(site~pathway, scales = "free_y", independent = "y") +
      labs(title = glue::glue("Discharges per site/pathway, {.y}"), y = "Observed minus simulated discharges")
    })) %>%
    pull(plot) %>%
    patchwork::wrap_plots(ncol = 1, axes = "collect")
  
  
  p5 <- out_df %>%
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
    mutate(plot = map2(plot, metric, \(x, y) x + labs(title = glue::glue("{y}"), y = "Discharges", x = "Day"))) %>%
    pull(plot) %>%
    patchwork::wrap_plots(ncol = 2, axes = "collect", guides = "collect")
  
  list(p1, p2, p3, p4, p5)
  })

discharge_plots

pathway_plots <- local({
  out_df <- bind_rows(
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
  )
  
  p1 <- out_df %>%
    summarise(
      simulated = sum(simulated),
      observed = sum(observed),
      .by = c(id, site, day, pathway)
    ) %>%
    mutate(diff = observed - simulated) %>%
    filter(site != "system") %>%
    summarise(
      mean_diff = mean(diff),
      u95 = quantile(diff, 0.975),
      l95 = quantile(diff, 0.025),
      .by = c(site, pathway)
    ) %>%
    ggplot(aes(x = pathway, y = mean_diff)) +
    annotate(geom = "rect", xmin = -Inf, xmax = Inf, ymin = -2.5, ymax = 2.5, alpha = 0.25) +
    geom_point() +
    geom_hline(aes(yintercept = 0), linetype = 2) +
    geom_errorbar(aes(ymin = l95, ymax = u95)) +
    ggh4x::facet_grid2(site ~ pathway, scales = "free", independent = "y") +
    labs(title = glue::glue("Pathway residuals per site"),
         y = "Observed pathway count minus simulated pathway count")
  
  p2 <- out_df %>%
    summarise(
      simulated = sum(simulated),
      observed = sum(observed),
      .by = c(id, site, pathway)
    ) %>%
    mutate(across(c(simulated, observed), ~ .x / sum(.x)), .by = c(id, site)) %>%
    mutate(diff = observed - simulated) %>%
    filter(site != "system") %>%
    summarise(
      mean_diff = mean(diff),
      u95 = quantile(diff, 0.975),
      l95 = quantile(diff, 0.025),
      .by = c(site, pathway)
    ) %>%
    ggplot(aes(x = pathway, y = mean_diff)) +
    geom_point() +
    geom_hline(aes(yintercept = 0), linetype = 2) +
    geom_errorbar(aes(ymin = l95, ymax = u95)) +
    ggh4x::facet_grid2(site ~ pathway, scales = "free", independent = "y") +
    labs(title = glue::glue("Pathway proportion residuals per site"),
         y = "Observed pathway proportion minus simulated pathway proportion")
  
   p3 <- out_df %>%
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
    nest(.by = metric) %>%
    mutate(plot = map2(data, metric, ~{.x %>%
        ggplot(aes(x = pathway, y = mean_diff)) +
        annotate(geom = "rect", xmin = -Inf, xmax = Inf, ymin = -2.5, ymax = 2.5, alpha = 0.25) +
        geom_point() +
        geom_hline(aes(yintercept = 0), linetype = 2) +
        geom_errorbar(aes(ymin = l95, ymax = u95)) +
        ggh4x::facet_grid2(site~pathway, scales = "free", independent = "y") +
        labs(title = glue::glue("Pathway residuals per site, {.y}"),
             y = "Observed pathway count minus simulated pathway count")
    })) %>%
    pull(plot) %>%
    patchwork::wrap_plots(ncol = 1, axes = "collect")
  
 p4 <- out_df %>%
    summarise(
      simulated = sum(simulated),
      observed = sum(observed),
      .by = c(id, site, pathway, metric)
    ) %>%
    mutate(across(c(simulated, observed), ~ .x/sum(.x)), .by = c(id, site, metric)) %>%
    mutate(diff = observed - simulated) %>%
    filter(site != "system") %>%
    summarise(mean_diff = mean(diff),
              u95 = quantile(diff, 0.975),
              l95 = quantile(diff, 0.025), .by = c(site, metric, pathway)) %>%
    nest(.by = metric) %>%
    mutate(plot = map2(data, metric, ~{.x %>%
        ggplot(aes(x = pathway, y = mean_diff)) +
        geom_point() +
        geom_hline(aes(yintercept = 0), linetype = 2) +
        geom_errorbar(aes(ymin = l95, ymax = u95)) +
        ggh4x::facet_grid2(site~pathway, scales = "free", independent = "y") +
        labs(title = glue::glue("Pathway proportion residuals per site, {.y}"),
             y = "Observed pathway proportion minus simulated pathway proportion")
    })) %>%
    pull(plot) %>%
    patchwork::wrap_plots(ncol = 1, axes = "collect")
  
 list(p1, p2, p3, p4)
  })

pathway_plots


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
