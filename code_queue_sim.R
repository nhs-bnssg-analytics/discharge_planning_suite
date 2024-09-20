


plot_df_queue_sim <- local({

# calculate the number of discharges made by observing patients on a queue
# leaving the census on the following day
discharges_ts <- nctr_df %>%
  filter(!is.na(NHS_Number)) %>%
  filter(Organisation_Site_Code %in% c('RVJ01', 'RA701', 'RA301', 'RA7C2')) %>%
  # filter for CTR, we wont predict the NCTR outcome for those already NCTR/on a queue
  # filter(Criteria_To_Reside == "N") %>%
  mutate(
    site = case_when(
      Organisation_Site_Code == 'RVJ01' ~ 'nbt',
      Organisation_Site_Code == 'RA701' ~ 'bri',
      Organisation_Site_Code %in% c('RA301', 'RA7C2') ~ 'weston',
      TRUE ~ 'other'
    )) %>%
  mutate(pathway = recode(Current_Delay_Code_Standard,
                          !!!pathway_recodes),
         pathway = coalesce(pathway, "Other")) %>%
  mutate(pathway = if_else(
    !pathway %in% c("P1", "P2", "P3", "P3" , "Other"),
    "Other",
    pathway
  )) %>%
  dplyr::select(nhs_number = NHS_Number, site, pathway, date = Census_Date, Date_Of_Admission) %>%
  distinct() %>%
  filter(pathway != "Other") %>%
  group_by(nhs_number, Date_Of_Admission) %>%
  mutate(id = cur_group_id()) %>%
  group_by(id) %>%
  # # (DEPRECATED) take the first non-other pathway
  # filter(date == min(date)) %>%
  # Find the last date for each ID
  filter(date == max(date)) %>%
  group_by(site, pathway, date = date + ddays(1)) %>%
  count() %>%
  group_by(site, pathway) %>%
  filter(date < max(date)) %>%
  arrange(date) %>%
  slice(-1)


# mean number of discharges per day/site
discharge_sum <- discharges_ts %>%
  filter(date >= (max(date) - dweeks(4))) %>%
  group_by(site, pathway) %>% 
  summarise(mean = mean(n),
            sd = sd(n))


discharges_ts %>%
ggplot(aes(x = date, y = n)) +
  geom_step() +
  geom_hline(data = discharge_sum,
      aes(yintercept = mean), col = "blue",
      linetype = 2) +
  facet_grid(site ~ pathway, scales = "free")


# current queue

pathway_queue <- nctr_sum %>%
  filter(!is.na(nhs_number), !is.na(ctr)) %>%
  filter(pathway != "Other", !ctr) %>% 
  dplyr::select(-ctr) %>%
  group_by(site, pathway) %>%
  count() %>%
  mutate(source = "current_ctr_data",
         report_date = max_date,
         day = 1) %>%
  pivot_longer(cols = c(n),
               names_to = "metric",
               values_to = "value")

t_sim <- n_days

sim_fn <- function(q, d, c) {
  n_arrs <-
    round(pmap_dbl(
      list(d$n, d$l85, d$u85),
      ~ rnorm(1, mean = ..1, sd = get_sd_from_ci(ci = c(..2, ..3)))
    ))
  n_cap <-  pmax(0, round(rnorm(length(d$day), mean = c[["av"]], sd = c[["sd"]])))
  # n_cap <-  round(rpois(length(d$day), lambda = c))
  q_out <- vector(mode = "numeric", length = length(n_cap))
  diff <- n_arrs - n_cap
  q_out[1] <- max(0, q + diff[1])
  for (i in seq_along(n_cap)[-1]) {
    q_out[i] <- max(0, q_out[i - 1]  + (diff)[i])
  }
  q_out
}

df_sim <- pathway_queue %>%
  dplyr::select(site, pathway, value) %>%
  left_join({
    df_pred %>%
      group_by(site, pathway) %>%
      nest(.key = "demand_fc")
  }, by = join_by(site, pathway)) %>%
  # join on daily discharage average
  left_join(discharge_sum, by = join_by(site, pathway)) %>%
  rename(initial_queue = value) %>%
  mutate(cap = map2(mean, sd, ~c(av = ..1, sd = ..2))) %>%
  mutate(cap_u = map(cap, ~c(av = .x[["av"]]*1.1, sd = .x[["sd"]])),
         cap_l = map(cap, ~c(av = .x[["av"]]*0.9, sd = .x[["sd"]]))) %>%
  mutate(sim = list(map(seq_len(n_rep),
                        function(rep)
                          reduce(
                          pmap(
                            list(initial_queue,
                                 demand_fc,
                                 cap), ~ {
                                   sim_fn(..1, ..2, ..3)
                                 }
                          ), rbind)))) %>%
  mutate(sim = map(sim, reduce, rbind)) %>%
  mutate(sim_u = list(map(seq_len(n_rep),
                        function(rep)
                          reduce(
                          pmap(
                            list(initial_queue,
                                 demand_fc,
                                 cap_u), ~ {
                                   sim_fn(..1, ..2, ..3)
                                 }
                          ), rbind)))) %>%
  mutate(sim_u = map(sim_u, reduce, rbind)) %>%
  mutate(sim_l = list(map(seq_len(n_rep),
                        function(rep)
                          reduce(
                          pmap(
                            list(initial_queue,
                                 demand_fc,
                                 cap_l), ~ {
                                   sim_fn(..1, ..2, ..3)
                                 }
                          ), rbind)))) %>%
  mutate(sim_l = map(sim_l, reduce, rbind)) %>%
  mutate(n = map(sim, colMeans),
         n_u85 = map(sim, ~ map_dbl(array_branch(.x, margin = 2), ~ quantile(.x, .925))),
         n_l85 = map(sim, ~ map_dbl(array_branch(.x, margin = 2), ~ quantile(.x, .075)))) %>%
  mutate(n_u = map(sim_u, colMeans),
         n_u_u85 = map(sim_u, ~ map_dbl(array_branch(.x, margin = 2), ~ quantile(.x, .925))),
         n_u_l85 = map(sim_u, ~ map_dbl(array_branch(.x, margin = 2), ~ quantile(.x, .075)))) %>%
  mutate(n_l = map(sim_l, colMeans),
         n_l_u85 = map(sim_l, ~ map_dbl(array_branch(.x, margin = 2), ~ quantile(.x, .925))),
         n_l_l85 = map(sim_l, ~ map_dbl(array_branch(.x, margin = 2), ~ quantile(.x, .075)))) %>%
  dplyr::select(site, pathway, n, n_u85, n_l85, n_u, n_u_u85, n_u_l85, n_l, n_l_u85, n_l_l85) %>%
  unnest(c(n, n, n_u85, n_l85, n_u, n_u_u85, n_u_l85, n_l, n_l_u85, n_l_l85)) %>%
  mutate(day = rep(c(1:n_days), times = n_distinct(site)*n_distinct(pathway))) %>%
  mutate(ctr = "N",
         source = "queue_sim",
         report_date = max_date) %>%
  pivot_longer(cols = c(n, n, n_u85, n_l85, n_u, n_u_u85, n_u_l85, n_l, n_l_u85, n_l_l85),
               names_to = "metric",
               values_to = "value")


df_sim <- df_sim %>%
  bind_rows(discharge_sum %>%
              dplyr::select(-sd) %>%
              pivot_longer(cols = -c(site, pathway),
                           names_to = "metric", 
                           values_to = "value") %>%
              mutate(ctr = "N",
                     metric = "slot_avg",
                     source = "queue_sim",
                     report_date = max_date)
  )
df_sim
})