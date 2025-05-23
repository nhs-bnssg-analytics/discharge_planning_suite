source("utils/utils.R")
n_rep <- 1E2
validation_end <- ymd("2024-09-01")
validation_start <- ymd("2023-07-01")
start_date <- validation_end - dweeks(13) 
nctr_df <- nctr_df %>% filter(between(Census_Date, validation_start, validation_end-ddays(1)))


nctr_sum <- nctr_df %>%
  filter(Person_Stated_Gender_Code %in% 1:2) %>%
  mutate(nhs_number = as.character(NHS_Number),
         nhs_number = if_else(is.na(nhs_number), glue::glue("unknown_{1:n()}"), nhs_number),
         sex = if_else(Person_Stated_Gender_Code == 1, "Male", "Female"))  %>%
  filter(Census_Date < max(Census_Date) -ddays(10)) %>%
  # mutate(nhs_number[is.na(nhs_number)] = glue::glue("unknown_{seq_along(nhs_number[is.na(nhs_number)])}"))
  mutate(site = case_when(Organisation_Site_Code == 'RVJ01' ~ 'nbt',
                          Organisation_Site_Code == 'RA701' ~ 'bri',
                          Organisation_Site_Code %in% c('RA301', 'RA7C2') ~ 'weston',
                          TRUE ~ NA_character_)) %>%
  filter(!is.na(site)) %>%
  filter(site != "nbt") %>% # remove nbt to better compare with the accum plot
  group_by(nhs_number, Date_Of_Admission) %>%
  mutate(spell_id = as.character(cur_group_id())) %>%
  ungroup() %>%
  mutate(
    der_los = (as.Date(Census_Date) - as.Date(Date_Of_Admission))/ddays(1),
    der_ctr = case_when(
      Criteria_To_Reside == "Y" | is.na(Criteria_To_Reside) ~ TRUE,
      !is.na(Days_NCTR) ~ FALSE,
      !is.na(Date_NCTR) ~ FALSE,
      Criteria_To_Reside == "N" ~ FALSE
    )) 


# attr_df <-
#   RODBC::sqlQuery(
#     con,
#     "select * from (
# select a.*, ROW_NUMBER() over (partition by nhs_number order by attribute_period desc) rn from
# [MODELLING_SQL_AREA].[dbo].[New_Cambridge_Score] a) b where b.rn = 1"
#   )

attr_df <- readRDS("data/attr_df.RDS")

dates_spells <- nctr_sum %>%
  group_by(Census_Date, Criteria_To_Reside, Days_NCTR, spell_id) %>%
  distinct() 

dates_spells <- dates_spells %>% 
  bind_rows(dates_spells %>% mutate(spell_id = paste0(spell_id, "_sys", sep = "")))


dates <- nctr_df %>%
  filter(Census_Date >= start_date,
         Census_Date < validation_end,
         Census_Date > ymd("2023-07-01"),
         Census_Date < max(Census_Date) - ddays(n_days),
         # remove dates near Christmas
         abs(lubridate::interval(Census_Date, ymd("2023-12-25"))/ddays(1)) > 15
  ) %>%
  pull(Census_Date) %>%
  unique()

# # take only Mondays
# 
# dates <- dates[weekdays(dates) == "Monday"] 

los_wf <- readRDS("data/los_wf.RDS")

los_rec <- hardhat::extract_recipe(los_wf)
los_engine <- hardhat::extract_recipe(los_wf)


fit_dists <- readRDS("data/fit_dists.RDS") %>%
  mutate(leaf = as.character(leaf))

# hack in system total dists for joining later

fit_dists <-
  fit_dists %>%
  expand_grid(site = unique(nctr_sum$site)) %>%
  bind_rows(
    fit_dists,
    mutate(fit_dists, site = "system")
  ) %>%
  group_by(site, leaf) %>%
  slice(1) %>%
  na.omit()

# # take sample of dates
d_i <- sample(dates, 100)
# d_i <- dates

nctr_sum <- nctr_sum %>%
  bind_rows(nctr_sum %>% mutate(site = "system", spell_id = paste0(spell_id, "_sys", sep = "")))


# fn 

los_rec <- hardhat::extract_recipe(los_wf)
los_engine <- hardhat::extract_fit_engine(los_wf)

drain_fn <- function(date_i) {
  
  sid_i <- dates_spells %>%
    filter(Census_Date == date_i & der_ctr) %>%
    pull(spell_id) %>% 
    unique()
  
  # Calculate the drain for these patients
  emp_drain <- nctr_sum %>%
    #first, only filter spells we are interested in
    filter(spell_id %in% sid_i) %>%
    filter(Census_Date >=date_i) %>%
    arrange(Census_Date) %>%
    group_by(spell_id) %>%
    mutate(los = min(der_los), # los on index date is the minimum LOS
           der_date_nctr = as.Date(if_else(any(!der_ctr),min(Census_Date[!der_ctr]) - ddays(1),max(Census_Date)))) %>%
    ungroup() %>%
    mutate(der_date_nctr = pmin(der_date_nctr, Date_NCTR, na.rm = TRUE)) %>% 
    group_by(spell_id) %>%
    mutate(discharge_rdy_los = (der_date_nctr - as.Date(Date_Of_Admission))/ddays(1)) %>%
    select(nhs_number = NHS_Number,
           site,
           spell_id,
           bed_type = Bed_Type,
           los,
           discharge_rdy_los) %>%
    # take first active LOS value for each spell
    group_by(spell_id) %>%
    arrange(discharge_rdy_los) %>%
    slice(1) %>%
    mutate(days_until_rdy = discharge_rdy_los - los) %>%
    group_by(day = days_until_rdy, site) %>%
    count(name = "value") %>%
    filter(day >= 0)
  
  sim_drain <- nctr_sum %>%
    # filter(Criteria_To_Reside == "Y" & (is.na(Days_NCTR) | Days_NCTR == 0)) %>%
    filter(Census_Date == date_i,
           spell_id %in% sid_i) %>%
    group_by(spell_id) %>%
    mutate(los = der_los) %>%
    select(nhs_number,
           site,
           age = Person_Age,
           sex = Person_Stated_Gender_Code,
           bed_type = Bed_Type,
           los) %>%
    left_join(
      select(attr_df,-sex,-age) %>% mutate(nhs_number = as.character(nhs_number)),
      by = join_by(nhs_number == nhs_number)
    ) %>%
    bake(los_rec, .) %>%
    mutate(site = if_else(is.na(site), "system", site)) %>%
    ungroup() %>%
    arrange(site) %>%
    mutate(id = 1:n(),
           leaf = as.character(treeClust::rpart.predict.leaves(los_engine, .))) %>%
    left_join(fit_dists, by = join_by(site == site, leaf == leaf)) %>%
    mutate(los_remaining = pmap(list(los, tdist),
                                function(los, trunc_dist)
                                  trunc_dist(
                                    n_rep,
                                    range = c(los, Inf)
                                  ) - los) %>%
             reduce(rbind)) %>%
    select(site, los_remaining)  %>%
    group_by(site) %>%
    nest(.key = "los_remaining") %>%
    mutate(los_remaining = map(los_remaining, ~
                                 `%/%`(.x, 1) %>%
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
                                 ))) %>% 
    mutate(los_remaining = map(los_remaining, enframe)) %>%
    mutate(los_remaining = map(los_remaining, ~unnest_wider(.x, value))) %>%
    unnest(cols = los_remaining) %>%
    rename(day = name) %>%
    pivot_longer(cols = -c(day, site), names_to = "metric") %>%
    mutate(source = "simulated",
           day = as.numeric(day))
  
  
  out_df <- emp_drain %>%
    mutate(source = "empirical", 
           metric = "mean") %>%
    bind_rows(sim_drain) %>%
    pivot_wider(names_from = metric, values_from = value) %>%
    mutate(date = date_i + ddays(day))
  # pivot_longer(cols = -day, names_to = "metric", values_to = "n") %>%
  # mutate(metric = recode(metric, n = "empirical", n_sim = "simulated")) %>%
  # group_by(metric) %>%
  # mutate(prop = n/sum(n)) %>%
  
  out_df %>%
    mutate(id = which(dates == date_i))
}

drain_fn_safe <- safely(drain_fn)

options(future.globals.maxSize = 16000 * 1024^2)
future::plan(future::multisession, workers = parallel::detectCores() - 6)
# future::plan(future::multisession, workers = 4)
out <- furrr::future_map(dates, ~drain_fn_safe(.x))
out <- furrr::future_map(dates[1:50], ~drain_fn_safe(.x))

map(out, "result") %>%
  map_lgl(is.null) %>%
  sum()

map(out, "result") %>%
  map_lgl(is.null) %>%
  which()

saveRDS(out, "data/drain_out_new.RDS")

map(out, "result") %>% bind_rows()

n_days <- 10

# out %>% 
#   map("result") %>%
#   reduce(bind_rows) %>%
#   mutate(wday = weekdays(date)) %>%
#   group_by(id, site) %>%
#   mutate(index_day = wday[which.min(date)]) %>%
#   select(-wday) %>%
#   # filter(day <= 10, !wday %in% c("Saturday", "Sunday")) %>%
#   # select(-wday) %>%
#   filter(site == "system") %>%
#   pivot_longer(cols = -c(id, site, day, date, index_day, source), names_to = "metric", values_to = "value") %>%
#   filter(metric == "mean") %>%
#   group_by(source, id, metric, index_day) %>%
#   mutate(prop = value/sum(value)) %>%
#   mutate(cum_prop = cumsum(prop)) %>% 
#   pivot_longer(cols = -c(day, date, site, source, id, index_day, metric), names_to = "calc", values_to = "value") %>%
#   unite("metric", metric, calc, sep = "_") %>%
#   pivot_wider(names_from = metric, values_from = value) %>%
#   filter(day <= n_days) %>%
#   group_by(index_day, day, source) %>%
#   summarise(mean_cum_prop = mean(mean_cum_prop)) %>%
#   mutate(source = recode(source, "empirical" = "Observed", "simulated" = "Simulated")) %>%
#   ggplot(aes(x = day, y = mean_cum_prop, fill = source)) +
#   # geom_col(position = "dodge") +
#   geom_line(aes(col = source)) +
#   # geom_errorbar(aes(ymin = l95_cum_prop, ymax = u95_cum_prop), position = "dodge") +
#   # facet_wrap(vars(id), scales = "free")  +
#   # bnssgtheme() +
#   theme_minimal() +
#   theme(legend.position = "bottom", legend.justification = "center") +
#   # scale_fill_manual(values = unname(bnssgcols[c(3, 7)])) +
#   scale_colour_manual(values = c("#8c96c6", "#88419d")) +
#   scale_y_continuous(labels = scales::percent) +
#   # scale_x_continuous(breaks = 0:n_days) +
#   labs(y = "Cumulative occupancy drain",
#        x = "Day",
#        col = "") +
#   facet_wrap(vars(index_day))

out %>% 
  map("result") %>%
  reduce(bind_rows) %>%
  group_by(id, site) %>%
  filter(site == "system") %>%
  pivot_longer(cols = -c(id, site, day, date, source), names_to = "metric", values_to = "value") %>%
  filter(metric == "mean") %>%
  group_by(source, id, metric) %>%
  mutate(prop = value/sum(value)) %>%
  mutate(cum_prop = cumsum(prop)) %>% 
  pivot_longer(cols = -c(day, date, site, source, id, metric), names_to = "calc", values_to = "value") %>%
  unite("metric", metric, calc, sep = "_") %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  filter(day <= n_days) %>%
  group_by(day, source) %>%
  summarise(mean_cum_prop = mean(mean_cum_prop)) %>%
  mutate(source = recode(source, "empirical" = "Observed", "simulated" = "Simulated")) %>%
  ggplot(aes(x = day, y = mean_cum_prop, fill = source)) +
  # geom_col(position = "dodge") +
  geom_line(aes(col = source)) +
  # geom_errorbar(aes(ymin = l95_cum_prop, ymax = u95_cum_prop), position = "dodge") +
  # facet_wrap(vars(id), scales = "free")  +
  # bnssgtheme() +
  theme_minimal() +
  theme(legend.position = "bottom", legend.justification = "center") +
  # scale_fill_manual(values = unname(bnssgcols[c(3, 7)])) +
  scale_colour_manual(values = c("#8c96c6", "#88419d")) +
  scale_y_continuous(labels = scales::percent) +
  # scale_x_continuous(breaks = 0:n_days) +
  labs(y = "Cumulative Occupancy Drain",
       x = "Day",
       col = "")


ggsave(
  last_plot(),
  filename = "./validation/validation_plot_los_drain_meancumulative.png",
  bg = "white",
  height = 7.5,
  width = 7.5, 
  scale = 0.6)

out %>%
  map("result") %>%
  bind_rows() %>%
  mutate(wday = weekdays(date)) %>%
  filter(id == 69, day <= 10, !wday %in% c("Saturday", "Sunday")) %>%
  filter(site == "system") %>%
  ggplot(aes(x = day, y = mean, fill = source)) + geom_col(position = "dodge")


(drain_plot_cum <- out %>% 
    map("result") %>%
    reduce(bind_rows) %>%
    filter(site == "system") %>%
    pivot_longer(cols = -c(id, site, day, date, source), names_to = "metric", values_to = "value") %>%
    group_by(source, id, metric) %>%
    mutate(prop = value/sum(value)) %>%
    mutate(cum_prop = cumsum(prop)) %>%
    pivot_longer(cols = -c(day, date, site, source, id, metric), names_to = "calc", values_to = "value") %>%
    unite("metric", metric, calc, sep = "_") %>%
    pivot_wider(names_from = metric, values_from = value) %>%
    filter(day <= 50) %>%
    ggplot(aes(x = day, y = mean_cum_prop, fill = source)) +
    # geom_col(position = "dodge") +
    geom_line(aes(col = source)) +
    # geom_errorbar(aes(ymin = l95_cum_prop, ymax = u95_cum_prop), position = "dodge") +
    facet_wrap(vars(id), scales = "free") +
    labs(y = "Cumulative occupancy drain") +
    theme_bw())


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