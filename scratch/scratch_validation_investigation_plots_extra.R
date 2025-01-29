census_log <- nctr_df_full %>%
  filter(Person_Stated_Gender_Code %in% 1:2) %>%
  mutate(nhs_number = as.character(NHS_Number),
         nhs_number = if_else(is.na(nhs_number), CDS_Unique_Identifier, nhs_number),
         sex = if_else(Person_Stated_Gender_Code == 1, "Male", "Female")) %>%
  filter(Organisation_Site_Code %in% c('RVJ01', 'RA701', 'RA301', 'RA7C2')) %>%
  mutate(
    site = case_when(
      Organisation_Site_Code == 'RVJ01' ~ 'nbt',
      Organisation_Site_Code == 'RA701' ~ 'bri',
      Organisation_Site_Code %in% c('RA301', 'RA7C2') ~ 'weston',
      TRUE ~ 'other'
    ),
    Date_Of_Admission = as.Date(Date_Of_Admission)
  ) %>%
  filter(site != "nbt") %>%
  ungroup() %>%
  mutate(
    der_los = (as.Date(Census_Date) - as.Date(Date_Of_Admission))/ddays(1),
    der_ctr = case_when(
      Criteria_To_Reside == "Y" | is.na(Criteria_To_Reside) ~ TRUE,
      !is.na(Days_NCTR) ~ FALSE,
      !is.na(Date_NCTR) ~ FALSE,
      Criteria_To_Reside == "N" ~ FALSE
    )) %>%
  mutate(report_date = max(Census_Date)) %>%
  mutate(los = (report_date - Date_Of_Admission) / ddays(1)) %>%
  mutate(
    pathway = recode(
      Current_Delay_Code_Standard,
      !!!pathway_recodes
    ),
    pathway = coalesce(pathway, "Other")
  ) %>%
  mutate(pathway = if_else(
    !pathway %in% c("P1", "P2", "P3", "P3" , "Other"),
    "Other",
    pathway
  )) %>%
  group_by(CDS_Unique_Identifier) %>%
  mutate(pathway = ifelse(length(pathway[pathway != "Other"]) > 0, head(pathway[pathway != "Other"], 1), "Other")) %>%
  mutate(start_date = min(Census_Date)) %>%
  mutate(end_date = as.Date(if_else(any(!der_ctr),min(Census_Date[!der_ctr]) - ddays(1),max(Census_Date)))) %>%
  dplyr::select(
    nhs_number,
    id = CDS_Unique_Identifier,
    start_date,
    end_date,
    site,
    pathway
  ) %>%
  ungroup() %>%
  distinct()



calc_new_admit_disch_rdy <- function(d, h = 10, census_log) {
  census_log %>%
    filter(start_date > d, between(end_date, d, d+ddays(h-1))) %>%
    group_by(site, pathway, date = end_date) %>%
    count(name = "observed") %>%
    ungroup() %>%
    complete(site, pathway, date = seq.Date(from = d, length.out = h, by = "day"), fill = list(observed = 0)) %>%
    mutate(day = factor(seq_len(h)), .by = c(site, pathway))
  }

calc_cur_admit_disch_rdy <- function(d, h = 10, census_log) {
  census_log %>%
    filter(start_date <= d, between(end_date, d, d+ddays(h-1))) %>%
    group_by(site, pathway, date = end_date) %>%
    count(name = "observed") %>%
    ungroup() %>%
    complete(site, pathway, date = seq.Date(from = d, length.out = h, by = "day"), fill = list(observed = 0)) %>% 
    mutate(day = factor(seq_len(h)), .by = c(site, pathway))
  }

calc_new_admit_disch_rdy(dates[5], census_log = census_log)
calc_cur_admit_disch_rdy(dates[5], census_log = census_log)

observed <- map(dates, \(d) list(
  new_admits = calc_new_admit_disch_rdy(d, census_log = census_log),
  cur_admits = calc_cur_admit_disch_rdy(d, census_log = census_log)
  )) %>%
map(bind_rows, .id = "source") %>%
bind_rows(.id = "id") 

observed %>%
  summarise(observed = sum(observed), .by = c(id, source, site, pathway, day)) %>% 
  summarise(across(observed, list(mean = mean,
                           uq = \(x) quantile(x, 0.975), 
                           lq = \(x) quantile(x, 0.025))),
            .by = c(site, source, pathway, day)) %>%
  ggplot(aes(x = day, y = observed_mean, ymin = observed_lq, ymax = observed_uq)) +
  geom_pointrange() +
  ggh4x::facet_grid2(pathway~site+source, scales = "free", independent = "y")


bind_rows(
  out %>%
    map("result") %>%
    map("na_out_df") %>%
    bind_rows(.id = "id") %>%
    filter(site != "nbt") %>%
    complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
    dplyr::select(id, site, day, pathway, source, n) %>%
    pivot_wider(values_from = n, names_from = source) %>%
    mutate(diff = observed - simulated, metric = "new_admits"),
  out %>%
    map("result") %>%
    map("ca_out_df") %>%
    bind_rows(.id = "id") %>%
    filter(site != "NBT") %>%
    complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
    dplyr::select(id, site, day, pathway, source, n) %>%
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
  summarise(across(c(simulated, baseline), \(x) yardstick::rmse_vec(observed, x)), .by = c(id, site, day, pathway)) %>%
  mutate(ratio = baseline/simulated) %>%
  summarise(median_ratio = quantile(ratio, 0.5),
            u95_ratio = quantile(ratio, 0.75),
            l95_ratio = quantile(ratio, 0.25),
            .by = c(site, day, pathway))%>%
  filter(site != "system", pathway != "Other") %>%
  ggplot(aes(x = as.numeric(day), y = median_ratio)) +
  geom_hline(yintercept = 1, linetype = 2) +
  geom_ribbon(aes(ymin = l95_ratio, ymax = u95_ratio), alpha = 0.15) +
  # geom_path() +
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
    dplyr::select(id, site, day, pathway, source, n) %>%
    pivot_wider(values_from = n, names_from = source) %>%
    mutate(source = "new_admits"),
  out %>%
    map("result") %>%
    map("ca_out_df") %>%
    bind_rows(.id = "id") %>%
    filter(site != "NBT") %>%
    complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
    dplyr::select(id, site, day, pathway, source, n) %>%
    pivot_wider(values_from = n, names_from = source) %>%
    mutate(source = "cur_admits")
) %>%
  dplyr::select(-observed) %>%
  left_join(observed) %>%
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
  mutate(across(c(simulated, baseline), list(error = \(x) observed - x))) %>%
  mutate(across(matches("_error"), list(sq = \(x) x^2))) %>%
  summarise(across(matches("error_sq"), list(mean = \(x) mean(x, na.rm = TRUE),
                                             uq = \(x) quantile(x, 0.975, na.rm = TRUE),
                                             lq = \(x) quantile(x, 0.025, na.rm = TRUE)
                                             )), .by = c(site, day, pathway)) %>%
  mutate(across(matches("error_sq_"), list(root = sqrt))) %>%
  mutate(
    ratio = baseline_error_sq_mean_root/simulated_error_sq_mean_root,
    ratio_uq = baseline_error_sq_uq_root/simulated_error_sq_uq_root,
    ratio_lq = baseline_error_sq_lq_root/simulated_error_sq_lq_root
    ) %>%
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
    dplyr::select(id, site, day, pathway, source, n) %>%
    pivot_wider(values_from = n, names_from = source) %>%
    mutate(diff = observed - simulated, metric = "new_admits"),
  out %>%
    map("result") %>%
    map("ca_out_df") %>%
    bind_rows(.id = "id") %>%
    filter(site != "NBT") %>%
    complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
    dplyr::select(id, site, day, pathway, source, n) %>%
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
  mutate(across(c(simulated, baseline), list(error = \(x) observed - x))) %>%
  mutate(across(matches("_error"), list(sq = \(x) x^2))) %>%
  summarise(across(matches("error_sq"), list(mean = mean,
                                             uq = \(x) quantile(x, 0.975),
                                             lq = \(x) quantile(x, 0.025)
                                             )), .by = c(site, day, pathway))%>%
  mutate(across(matches("error_sq_"), list(root = sqrt))) %>%
  mutate(
    ratio = baseline_error_sq_mean_root/simulated_error_sq_mean_root,
    ratio_uq = baseline_error_sq_uq_root/simulated_error_sq_uq_root,
    ratio_lq = baseline_error_sq_lq_root/simulated_error_sq_lq_root
    ) %>%
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
  facet_grid(site ~ pathway) +
  # ggh4x::facet_grid2(site ~ pathway, scales = "free_y", independent = "y") +
  labs(x = "Day",
       colour = "",
       y = str_wrap("RMSE ratio between baseline and simulation models", 50)) +
  theme(legend.position = "bottom")
