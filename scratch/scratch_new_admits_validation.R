n_rep <- 100
start_date <- min(nctr_df$Census_Date) 
start_date <- ymd("2024-01-01")

admits_ts <- nctr_df %>%
  ungroup() %>%
  mutate(nhs_number = as.character(NHS_Number),
         nhs_number = if_else(is.na(nhs_number), glue::glue("unknown_{1:n()}"), nhs_number),
         sex = if_else(Person_Stated_Gender_Code == 1, "Male", "Female")) %>%
  filter(Census_Date > start_date) %>%
  filter(Census_Date > start_date, Date_Of_Admission > start_date) %>% # data before this are spurious
  #TODO: use the fix for nhs numbers we dont know
  filter(!is.na(nhs_number)) %>%
  filter(Organisation_Site_Code %in% c('RVJ01', 'RA701', 'RA301', 'RA7C2')) %>%
  mutate(site = case_when(Organisation_Site_Code == 'RVJ01' ~ 'nbt',
                          Organisation_Site_Code == 'RA701' ~ 'bri',
                          Organisation_Site_Code %in% c('RA301', 'RA7C2') ~ 'weston',
                          TRUE ~ '')) %>%
  group_by(nhs_number) %>%
  distinct(Date_Of_Admission, .keep_all = TRUE) %>%
  group_by(site , date = as.Date(Date_Of_Admission)) %>%
  count() 


rdist <- readRDS("data/fit_dists.RDS") %>%
  filter(leaf == -1) %>%
  pull(rdist) %>%
  `[[`(1)

# rdist <- partial(rlnorm, meanlog = 1.3, sdlog = 0.99)

sites <- unique(admits_ts$site)
dates <- sort(unique(admits_ts$date))

sim <- expand_grid(site = sites,
                   rep = seq_len(n_rep),
                   date = dates) %>%
  left_join(admits_ts, join_by(site, date == date)) %>%
  mutate(arrivals = coalesce(n, 0),
         # coalesce in case we sample below zero
         day = rep(1:length(dates), length(sites) * n_rep)) %>% 
  mutate(los = map(arrivals, ~ round(
    rdist(..1)))) %>%
  unnest(los) %>%
  mutate(date_end = date + ddays(los)) #%>%


sim_out_df <- map(sites,
                  function(s)
                    map_dbl(dates,
                            ~ nrow(
                              sim %>% filter(site == s, date <= .x, date_end > .x)
                            ))) %>%
  
  enframe() %>%
  mutate(site = sites) %>%
  unnest(value) %>%
  mutate(value = value / n_rep) %>%
  group_by(site) %>%
  mutate(day = 1:n()) %>%
  select(site, day, value) %>%
  mutate(source = "sim")

# empirical accumulation

act <- nctr_df %>%
  ungroup() %>%
  filter(Census_Date > start_date) %>%
  filter(Person_Stated_Gender_Code %in% 1:2) %>%
  filter(Date_Of_Admission > min(Census_Date)) %>%
  mutate(nhs_number = as.character(NHS_Number),
         nhs_number = if_else(is.na(nhs_number), glue::glue("unknown_{1:n()}"), nhs_number),
         sex = if_else(Person_Stated_Gender_Code == 1, "Male", "Female")) %>%
  # filter for our main sites / perhaps I shouldn't do this?
  filter(Organisation_Site_Code %in% c('RVJ01', 'RA701', 'RA301', 'RA7C2')) %>%
  # filter for CTR, we wont predict the NCTR outcome for those already NCTR/on a queue
  # filter(Criteria_To_Reside == "N") %>%
  mutate(
    site = case_when(
      Organisation_Site_Code == 'RVJ01' ~ 'nbt',
      Organisation_Site_Code == 'RA701' ~ 'bri',
      Organisation_Site_Code %in% c('RA301', 'RA7C2') ~ 'weston',
      TRUE ~ 'other'
    ),
    Date_Of_Admission = as.Date(Date_Of_Admission),
    Date_NCTR = as.Date(Date_NCTR)
  ) %>%
  mutate(
    der_los = (as.Date(Census_Date) - as.Date(Date_Of_Admission))/ddays(1),
    der_ctr = case_when(
      Criteria_To_Reside == "Y" | is.na(Criteria_To_Reside) ~ TRUE,
      !is.na(Days_NCTR) ~ FALSE,
      !is.na(Date_NCTR) ~ FALSE,
      Criteria_To_Reside == "N" ~ FALSE
    )) %>%
  group_by(nhs_number, Date_Of_Admission) %>%
  mutate(spell_id = cur_group_id(),
         der_date_nctr = as.Date(if_else(any(!der_ctr),min(Census_Date[!der_ctr]) - ddays(1),max(Census_Date)))) %>%
  # rowwise() %>%
  # mutate(der_date_nctr = min(der_date_nctr, Date_NCTR, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(der_date_nctr = pmin(der_date_nctr, Date_NCTR, na.rm = TRUE)) %>%
  select(nhs_number, spell_id, site, Date_Of_Admission, der_date_nctr) %>%
  distinct()


act_out_df <- map(sites,
                  function(s) map_dbl(dates,
                                      ~nrow(act %>% filter(site == s,
                                                           Date_Of_Admission <= .x,
                                                           der_date_nctr > .x)))) %>%
  enframe() %>%
  mutate(site = sites) %>%
  unnest(value) %>%
  mutate(value = value) %>%
  group_by(site) %>%
  mutate(day = 1:n()) %>%
  select(site, day, value) %>%
  mutate(source = "empirical")


p <- bind_rows(sim_out_df, act_out_df) %>%
  ggplot(aes(x = day, y = value, col = source)) +
  geom_step() +
  facet_wrap(vars(site)) +
  theme_bw()


ggsave(
  p,
  filename = "./validation/validation_plot_new_admits.png",
  scale = 0.55,
  width = 30,
  height = 15
)
