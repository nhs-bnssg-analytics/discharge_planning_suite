admits_ts <- nctr_df %>%
  filter(!is.na(NHS_Number)) %>%
  filter(Organisation_Site_Code %in% c('RVJ01', 'RA701', 'RA301', 'RA7C2')) %>%
  mutate(site = case_when(Organisation_Site_Code == 'RVJ01' ~ 'nbt',
                          Organisation_Site_Code == 'RA701' ~ 'bri',
                          Organisation_Site_Code %in% c('RA301', 'RA7C2') ~ 'weston',
                          TRUE ~ '')) %>%
  group_by(nhs_number = NHS_Number) %>%
  distinct(Date_Of_Admission, .keep_all = TRUE) %>%
  group_by(site , date = as.Date(Date_Of_Admission)) %>%
  count() %>%
  filter(date > ymd("2023-07-01"))


rdist <- readRDS("data/fit_dists.RDS") %>%
  filter(leaf == -1) %>%
  pull(rdist) %>%
  `[[`(1)

props <- readRDS("data/pathway_prop.RDS") %>%
  set_names(c("Other", "P1", "P2", "P3"))


sites <- unique(admits_ts$site)
dates <- sort(unique(admits_ts$date))

sim <- expand_grid(site = sites,
                   rep = seq_len(n_rep),
                   date = dates) %>%
  left_join(admits_ts, join_by(site, date == date)) %>%
  mutate(arrivals = coalesce(n, 0),
         # coalesce in case we sample below zero
         day = rep(1:length(dates), length(sites) * n_rep)) %>% # adding hour column, for grouping/stats later (3 sites x 1000 reps)
  mutate(los = map(arrivals, ~ round(
    rdist(..1)))) %>%
  unnest(los) %>%
  mutate(date_end = date + ddays(los)) #%>%
  #group_by(site, day = (date_end - min(dates)) / ddays(1), rep) %>%
  #mutate(bla = 1)
  #count() %>%
  #ungroup() 
sim

foo <- map(sites,
           function(s) map_dbl(dates,
                                  ~nrow(sim %>% filter(site == s, date <= .x, date_end > .x))))


