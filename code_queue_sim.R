


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
                          "P3 / Other Complex Discharge" = "P3",
                          "Uncoded" = "Other",
                          "Repatriation" = "Other",
                          "NCTR Null" = "Other",
                          "Not Set" = "Other",
                          "18a  Infection  bxviii  Standard" = "Other",
                          "xviii. Awaiting discharge to a care home but have not had a COVID 19 test (in 48 hrs preceding discharge)." = "Other",
                          "15b  Repat  bxv  WGH" = "Other"),
         pathway = coalesce(pathway, "Other")) %>%
  mutate(pathway = if_else(
    !pathway %in% c("P1", "P2", "P3", "P3" , "Other"),
    "Other",
    pathway
  )) %>%
  select(nhs_number = NHS_Number, site, pathway, date = Census_Date, Date_Of_Admission) %>%
  distinct() %>%
  filter(pathway != "Other") %>%
  group_by(nhs_number, Date_Of_Admission) %>%
  mutate(id = cur_group_id()) %>%
  group_by(id) %>%
  filter(date == max(date)) %>%
  group_by(site, pathway, date = date + ddays(1)) %>%
  count() %>%
  group_by(site, pathway) %>%
  filter(date < max(date))


# mean number of discharges per day/site
discharge_sum <- discharges_ts %>%
  group_by(site, pathway) %>%
  summarise(mean = mean(n))

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
  filter(pathway != "Other", ctr == "N") %>% 
  select(-ctr) %>%
  group_by(site, pathway) %>%
  count() %>%
  mutate(source = "current_ctr_data",
         report_date = max_date,
         day = 0) %>%
  pivot_longer(cols = c(n),
               names_to = "metric",
               values_to = "value")

t_sim <- n_days
n_rep <- 1E4

sim_fn <- function(q, d, c) {
  n_arrs <-
    round(pmap_dbl(
      list(d$n, d$l95, d$u95),
      ~ rnorm(1, mean = ..1, sd = get_sd_from_ci(ci = c(..2, ..3)))
    ))
  n_cap <-  round(rpois(length(d$day), lambda = c))
  q_out <- vector(mode = "numeric", length = length(n_cap))
  diff <- n_arrs - n_cap
  q_out[1] <- max(0, q + diff[1])
  for (i in seq_along(n_cap)[-1]) {
    q_out[i] <- max(0, q_out[i - 1]  + (diff)[i])
  }
  q_out
}

df_sim <- pathway_queue %>%
  select(site, pathway, value) %>%
  left_join({
    df_pred %>%
      group_by(site, pathway) %>%
      nest(.key = "demand_fc")
  }, by = join_by(site, pathway)) %>%
  # join on daily discharage mean
  left_join(discharge_sum, by = join_by(site, pathway)) %>%
  rename(initial_queue = value, cap_mean = mean) %>%
  mutate(sim = list(map(seq_len(n_rep),
                        function(rep)
                          reduce(
                          pmap(
                            list(initial_queue,
                                 demand_fc,
                                 cap_mean), ~ {
                                   sim_fn(..1, ..2, ..3)
                                 }
                          ), rbind)))) %>%
  mutate(sim = map(sim, reduce, rbind)) %>%
  mutate(n = map(sim, colMeans),
         u95 = map(sim, ~ map_dbl(array_branch(.x, margin = 2), ~ quantile(.x, .975))),
         l95 = map(sim, ~ map_dbl(array_branch(.x, margin = 2), ~ quantile(.x, .025)))) %>%
  select(site, pathway, n, u95, l95) %>%
  unnest(c(n, u95, l95)) %>%
  mutate(day = rep(c(0:n_days), times = n_distinct(site)*n_distinct(pathway))) %>%
  mutate(ctr = "N",
         source = "queue_sim",
         report_date = max_date) %>%
  pivot_longer(cols = c(n, u95, l95),
               names_to = "metric",
               values_to = "value")
df_sim
})

# %>%
#   ggplot(aes(x = day, y = n)) +
#   geom_ribbon(aes(ymin = l95, ymax = u95), alpha = 0.25) +
#   geom_step() +
#   facet_grid(site ~ pathway)
#   

# # derive pathway timeseries
# pathway_ts <- nctr_df %>%
#   filter(!is.na(NHS_Number)) %>%
#   filter(Organisation_Site_Code %in% c('RVJ01', 'RA701', 'RA301', 'RA7C2')) %>%
#   # filter for CTR, we wont predict the NCTR outcome for those already NCTR/on a queue
#   # filter(Criteria_To_Reside == "N") %>%
#   mutate(
#     site = case_when(
#       Organisation_Site_Code == 'RVJ01' ~ 'nbt',
#       Organisation_Site_Code == 'RA701' ~ 'bri',
#       Organisation_Site_Code %in% c('RA301', 'RA7C2') ~ 'weston',
#       TRUE ~ 'other'
#     )) %>%
#   mutate(pathway = recode(Current_Delay_Code_Standard,
#                           "P3 / Other Complex Discharge" = "P3",
#                           "Uncoded" = "Other",
#                           "Repatriation" = "Other",
#                           "NCTR Null" = "Other",
#                           "Not Set" = "Other",
#                           "18a  Infection  bxviii  Standard" = "Other",
#                           "xviii. Awaiting discharge to a care home but have not had a COVID 19 test (in 48 hrs preceding discharge)." = "Other",
#                           "15b  Repat  bxv  WGH" = "Other"),
#          pathway = coalesce(pathway, "Other")) %>%
#   mutate(pathway = if_else(
#     !pathway %in% c("P1", "P2", "P3", "P3" , "Other"),
#     "Other",
#     pathway
#   )) %>%
#   select(nhs_number = NHS_Number, site, pathway, date = Census_Date, Date_Of_Admission) %>%
#   group_by(nhs_number, Date_Of_Admission) %>%
#   mutate(id = cur_group_id()) %>%
#   group_by(nhs_number) %>%
#   arrange(date) %>%
#   filter(pathway != lag(pathway, default = "blank")) %>%
#   group_by(id) %>%
#   mutate(first_pathway_date = min(date[pathway != "Other"])) %>% 
#   filter(is.finite(first_pathway_date)) %>%
#   group_by(date = first_pathway_date, site, pathway) %>%
#   count() %>%
#   filter(pathway != "Other",
#          date > ymd("2023-07-15"))
# 
# 
# pathway_queue_ts <- nctr_df %>%
#   filter(!is.na(NHS_Number)) %>%
#   filter(Organisation_Site_Code %in% c('RVJ01', 'RA701', 'RA301', 'RA7C2')) %>%
#   # filter for CTR, we wont predict the NCTR outcome for those already NCTR/on a queue
#   # filter(Criteria_To_Reside == "N") %>%
#   mutate(
#     site = case_when(
#       Organisation_Site_Code == 'RVJ01' ~ 'nbt',
#       Organisation_Site_Code == 'RA701' ~ 'bri',
#       Organisation_Site_Code %in% c('RA301', 'RA7C2') ~ 'weston',
#       TRUE ~ 'other'
#     )) %>%
#   mutate(pathway = recode(Current_Delay_Code_Standard,
#                           "P3 / Other Complex Discharge" = "P3",
#                           "Uncoded" = "Other",
#                           "Repatriation" = "Other",
#                           "NCTR Null" = "Other",
#                           "Not Set" = "Other",
#                           "18a  Infection  bxviii  Standard" = "Other",
#                           "xviii. Awaiting discharge to a care home but have not had a COVID 19 test (in 48 hrs preceding discharge)." = "Other",
#                           "15b  Repat  bxv  WGH" = "Other"),
#          pathway = coalesce(pathway, "Other")) %>%
#   mutate(pathway = if_else(
#     !pathway %in% c("P1", "P2", "P3", "P3" , "Other"),
#     "Other",
#     pathway
#   )) %>%
#   select(nhs_number = NHS_Number, site, pathway, date = Census_Date, Date_Of_Admission) %>%
#   distinct() %>%
#   filter(pathway != "Other") %>%
#   group_by(site, pathway, date) %>%
#   count() 