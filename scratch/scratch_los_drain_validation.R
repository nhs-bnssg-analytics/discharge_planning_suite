source("utils.R")
n_rep <- 1E3

nctr_sum <- nctr_df %>%
  filter(Person_Stated_Gender_Code %in% 1:2) %>%
  mutate(nhs_number = as.character(NHS_Number),
         nhs_number = if_else(is.na(nhs_number), glue::glue("unknown_{1:n()}"), nhs_number),
         sex = if_else(Person_Stated_Gender_Code == 1, "Male", "Female"))  %>%
  filter(Census_Date < max(Census_Date) -ddays(10)) %>%
  # mutate(nhs_number[is.na(nhs_number)] = glue::glue("unknown_{seq_along(nhs_number[is.na(nhs_number)])}"))
  mutate(Organisation_Site_Code = case_when(Organisation_Site_Code == 'RVJ01' ~ 'nbt',
                                            Organisation_Site_Code == 'RA701' ~ 'bri',
                                            Organisation_Site_Code %in% c('RA301', 'RA7C2') ~ 'weston',
                                            TRUE ~ '')) %>%
  group_by(nhs_number, Date_Of_Admission) %>%
  mutate(spell_id = cur_group_id()) %>%
  ungroup() %>%
    mutate(
         der_los = (as.Date(Census_Date) - as.Date(Date_Of_Admission))/ddays(1),
         der_ctr = case_when(
           Criteria_To_Reside == "Y" | is.na(Criteria_To_Reside) ~ TRUE,
           !is.na(Days_NCTR) ~ FALSE,
           !is.na(Date_NCTR) ~ FALSE,
           Criteria_To_Reside == "N" ~ FALSE
         )) 


dates_spells <- nctr_sum %>%
  group_by(Census_Date, Criteria_To_Reside, Days_NCTR, spell_id) %>%
  distinct()


dates <- nctr_df %>%
  filter(Census_Date > ymd("2023-07-01"),
         Census_Date < max(Census_Date) - ddays(50)) %>%
  pull(Census_Date) %>%
  unique()

los_wf <- readRDS("data/los_wf.RDS")

los_dist <- readRDS("data/dist_split.RDS") %>%
  enframe() %>%
  unnest_wider(value)

# take sample of dates
d_i <- sample(dates, 9)

out <- map(d_i, ~{

# spell ids with CTR from this date:
sid_i <- dates_spells %>%
  filter(Census_Date ==.x & der_ctr) %>%
  pull(spell_id) %>% 
  unique()

# Calculate the drain for these patients

emp_drain <- nctr_sum %>%
  #first, only filter spells we are interested in
  filter(spell_id %in% sid_i) %>%
  filter(Census_Date >=.x) %>%
  arrange(Census_Date) %>%
  group_by(spell_id) %>%
  mutate(los = min(der_los), # los on index date is the minimum LOS
         los_dis_rdy = case_when(
           !any(der_ctr) ~ max(der_los), # if never NCTR, take max LOS
           der_ctr ~ tail(der_los, 1),  # where CTR take last LOS
           length(der_los) == 1 ~ der_los, # if only 1 LOS record, take that
           )
         ) %>%
select(nhs_number = NHS_Number,
       spell_id,
       bed_type = Bed_Type,
       los,
       los_dis_rdy) %>%
  # take first active LOS value for each spell
  group_by(spell_id) %>%
  arrange(los_dis_rdy) %>%
  slice(1) %>%
  mutate(days_until_rdy = los_dis_rdy - los) %>%
  group_by(day = days_until_rdy) %>%
  count(name = "value")

sim_drain <- nctr_sum %>%
  # filter(Criteria_To_Reside == "Y" & (is.na(Days_NCTR) | Days_NCTR == 0)) %>%
  filter(Census_Date == .x,
         spell_id %in% sid_i) %>%
  group_by(spell_id) %>%
  mutate(los = der_los) %>%
  select(nhs_number,
         age = Person_Age,
         sex = Person_Stated_Gender_Code,
         bed_type = Bed_Type,
         los) %>%
  left_join(
    select(attr_df,-sex,-age) %>% mutate(nhs_number = as.character(nhs_number)),
    by = join_by(nhs_number == nhs_number)
  ) %>%
  bake(extract_recipe(los_wf) %>% update_role_requirements(role = "site id", bake = FALSE),
       .) %>%
  ungroup() %>%
  mutate(id = 1:n(),
         leaf = as.character(treeClust::rpart.predict.leaves(extract_fit_engine(los_wf), .))) %>%
  left_join(los_dist, by = join_by(leaf == name)) %>%
  mutate(los_remaining = pmap(list(los, meanlog, sdlog),
                              ~ rlnormt(n_rep, ..2, ..3, range = c(..1, Inf)) - ..1) %>%
           reduce(rbind)) %>%
  pull(los_remaining) %>%
  `%/%`(., 1) %>%
  apply(
    MARGIN = 2,
    FUN = function(x)
      table(factor(x, levels = 0:max(.)))
  ) %>%
  apply(
    MARGIN = 1,
    FUN = function(x)
      list(
        mean = mean(x),
        u95 = quantile(x, 0.975),
        l95 = quantile(x, 0.225)
      )
  ) %>%
  enframe() %>%
  unnest_wider(value) %>%
  rename(day = name) %>%
  pivot_longer(cols = -day, names_to = "metric") %>%
  mutate(source = "simulated",
         day = as.numeric(day))

out_df <- emp_drain %>%
  mutate(source = "empirical", 
         metric = "mean") %>%
  bind_rows(sim_drain) %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  mutate(date = .x + ddays(day))
  # pivot_longer(cols = -day, names_to = "metric", values_to = "n") %>%
  # mutate(metric = recode(metric, n = "empirical", n_sim = "simulated")) %>%
  # group_by(metric) %>%
  # mutate(prop = n/sum(n)) %>%

 out_df %>%
   mutate(id = which(d_i == .x))}
)
  

out %>%
  reduce(bind_rows) %>%
  pivot_longer(cols = -c(id, day, date, source), names_to = "metric", values_to = "value") %>%
  group_by(source, id, metric) %>%
  mutate(prop = value/sum(value)) %>%
  mutate(cum_prop = cumsum(prop)) %>%
  pivot_longer(cols = -c(day, date, source, id, metric), names_to = "calc", values_to = "value") %>%
  unite("metric", metric, calc, sep = "_") %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  filter(day <= 50) %>%
  ggplot(aes(x = day, y = mean_cum_prop, fill = source)) +
  # geom_col(position = "dodge") +
  geom_line(aes(col = source)) +
  # geom_errorbar(aes(ymin = l95_cum_prop, ymax = u95_cum_prop), position = "dodge") +
  facet_wrap(vars(id), scales = "free")


out %>%
  reduce(bind_rows) %>%
  pivot_longer(cols = -c(id, day, date, source), names_to = "metric", values_to = "value") %>%
  group_by(source, id, metric) %>%
  mutate(prop = value/sum(value)) %>%
  mutate(cum_prop = cumsum(prop)) %>%
  pivot_longer(cols = -c(day, date, source, id, metric), names_to = "calc", values_to = "value") %>%
  unite("metric", metric, calc, sep = "_") %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  filter(day <= 20) %>%
  ggplot(aes(x = date, y = mean_value, fill = source)) +
  geom_col(position = "dodge") +
  scale_x_date(labels = date_format(format = "%a"), date_breaks = "2 days") +
  # geom_line(aes(col = source)) +
  geom_errorbar(aes(ymin = l95_value, ymax = u95_value), position = "dodge") +
  facet_wrap(vars(id), scales = "free")
 # foo <- with(sim_drain,
 #      replicate(n_rep, pmap_dbl(list(los, meanlog, sdlog), ~rlnormt(1, ..2, ..3, range = c(..1, Inf)) - ..1))
 # )
 # 
 # foo <- with(sim_drain,
 #      pmap(list(los, meanlog, sdlog), ~rlnormt(n_rep, ..2, ..3, range = c(..1, Inf)) - ..1) %>%
 #      reduce(rbind)
 # )

# bar <-
#   apply(sim_drain$los_remaining %/% 1,
#         MARGIN = 2,
#         FUN = \(x) table(factor(x, levels = 0:max(
#           sim_drain$los_remaining
#         ))))
# 
# 
# 
# 
# 
# 
# 
# foo
# 
# foo %>%
#   as_tibble() %>%
#   pivot_longer(cols = everything(),
#                names_to = "sim",
#                values_to = "day") %>%
#   pivot_wider(names_from = day, values_from = sim)
# 
# 
# 
# model.matrix( ~ 0 + bar)
# 
# %>%
#   pivot_wider(names_from = day, values_fill = 1)
# 
# 
# %>%
#   mutate(los_remaining = pmap(
#     list(los, meanlog, sdlog),
#     ~
#       rlnormt(
#         n_rep,
#         meanlog = ..2,
#         sdlog = ..3,
#         range = c(..1, Inf)
#       ) - ..1
#   )) %>%
#   dplyr::select(id, los, los_remaining) %>%
#   unnest(los_remaining) %>%
#   mutate(los_remaining = ifelse(los_remaining < 0, 0, los_remaining)) %>%
#   mutate(los_remaining = los_remaining %/% 1) %>%
#   group_by(id) %>%
#   mutate(rep = 1:n()) %>%
#   group_by(rep, day = los_remaining) %>%
#   count() %>% # compute CIs/mean over reps
#   group_by(day) %>%
#   summarise(across(n, list(
#     mean = mean,
#     u95 = {
#       \(x) quantile(x, 0.975)
#     },
#     l95 = {
#       \(x) quantile(x, 0.025)
#     }
#   )))
# 
# 
# 
# 
# # take maximum date we have data for each patient and admission
# filter(Census_Date == max(Census_Date)) %>%
#   ungroup() %>%
#   # keep only patients with either NCTR (we know they are ready for discharge)
#   # OR whos max date recorded is before the latest data (have been discharged)
#   filter(!is.na(Date_NCTR) | Census_Date != max(Census_Date)) %>%
#   # Compute the fit for discharge LOS
#   mutate(discharge_rdy_los = ifelse(!is.na(Date_NCTR), Current_LOS - Days_NCTR, Current_LOS + 1))  %>%
#   # remove negative LOS (wrong end timestamps?)
#   filter(discharge_rdy_los > 0) %>%
#   filter(Organisation_Site_Code %in% c('RVJ01', 'RA701', 'RA301', 'RA7C2')) %>%
#   dplyr::select(
#     nhs_number = nhs_number,
#     site = Organisation_Site_Code,
#     sex,
#     age = Person_Age,
#     spec = Specialty_Code,
#     bed_type = Bed_Type,
#     los = discharge_rdy_los
#   )
