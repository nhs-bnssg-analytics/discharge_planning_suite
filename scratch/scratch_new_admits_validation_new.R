n_rep <- 1E2
validation_end <- ymd("2024-09-01")
validation_start <- ymd("2023-07-01")
start_date <- validation_end - dweeks(26) 
nctr_df <- nctr_df %>% filter(between(Census_Date, validation_start, validation_end-ddays(1)))

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
  pull("rdist") %>%
  `[[`(1)

# rdist <- tibble(site = c("bri", "nbt", "weston"),
#                 rdist = list(
#                   partial(rlnorm, meanlog = 1.3, sdlog = 0.99),
#                   partial(rlnorm, meanlog = 1.47, sdlog = 0.99),
#                   partial(rlnorm, meanlog = 1.3, sdlog = 0.99)
#                   )
# )


sites <- unique(admits_ts$site)
dates <- sort(unique(admits_ts$date))

n_days <- 10

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


na_sim_fn <- function(d){
  
  sim <- expand_grid(site = sites,
                     rep = seq_len(n_rep),
                     date = seq.Date(d, d + ddays(n_days - 1), by = "1 day")) %>%
    left_join(admits_ts, join_by(site, date == date)) %>% 
    # bind_cols(rdist) %>%
    # left_join(rdist, join_by(site)) %>%
    # filter(date >= d, date < d+ddays(10)) %>%
    mutate(arrivals = coalesce(n, 0),
           # coalesce in case we sample below zero
           day = rep(1:10, length(sites) * n_rep)) %>%
    filter(arrivals > 0) %>%
    mutate(los = map(arrivals, function(arr) round(
      rdist(arr)))) %>%
    # EnvStats::remp(arr, obs = los_df$los)))) %>%
    # mutate(los = map2(rdist, arrivals, function(dist, arr) round(
    #   dist(arr)))) %>%
    unnest(los) %>%
    mutate(date_end = date + ddays(los),
           day_end = day + los) %>%
    filter(day_end <= 10) %>%
    mutate(day_end = factor(day_end, levels = 1:10)) %>%
    group_by(rep, site, day_end) %>%
    count()
  
  
  sim_out_df <- sim %>% 
    # filter(day_end <= 10) %>%
    group_by(site, day_end) %>%
    summarise(
      u95 = quantile(n, 0.975),
      l95 = quantile(n, 0.025),
      n = mean(n)
    ) %>%
    mutate(source = "sim")
  
  
  # empirical accumulation
  
  act <- nctr_df %>%
    ungroup() %>%
    filter(Date_Of_Admission >= d) %>%
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
    # filter(Census_Date >= d, Census_Date < d + ddays(10)) %>%
    mutate(der_date_nctr = pmin(der_date_nctr, Date_NCTR, na.rm = TRUE)) %>%
    select(nhs_number, spell_id, site, Date_Of_Admission, der_date_nctr) %>%
    distinct() 
  
  
  act_out_df <- act %>%
    mutate(day_end = (der_date_nctr - d)/ddays(1)) %>%
    filter(day_end <= 10) %>%
    mutate(day_end = factor(day_end, levels = 1:10)) %>%
    group_by(site, day_end, .drop = FALSE) %>%
    count() %>%
    mutate(source = "actual") 
  
  
  plot_df <- bind_rows(sim_out_df, act_out_df) 
  
  p <- ggplot() +
    geom_errorbar(data = na.omit(plot_df), aes(x = day_end, ymin = l95, ymax = u95)) +
    geom_col(data = plot_df, aes(x = day_end, y = n, fill = source), position = "dodge") +
    facet_wrap(vars(site)) +
    theme_bw()
  
  res_out <- plot_df %>%
    select(site, day_end, n, source) %>%
    pivot_wider(names_from = source, values_from = n) %>% 
    mutate(residual = actual - sim) 
  
  # summarise(residual = mean(residual)) %>%
  # ggplot(aes(x = day_end, y = residual)) +
  # geom_col() +
  # facet_wrap(vars(site))
  
  list(p = p, res_out = res_out)
  
  }

sim_safe <- safely(na_sim_fn)
sim_safe(sample(dates, 1))


d_i <- sample(dates, 50)


options(future.globals.maxSize = 16000 * 1024^2)
future::plan(future::multisession, workers = parallel::detectCores() - 6)


out <- furrr::future_map(dates, sim_safe)

map(out, "result") %>%
  map("res_out") %>%
  map_lgl(is.null) %>%
  sum()

map(out, "result") %>%
  map("res_out") %>%
  map_lgl(is.null) %>%
  which()

na_sim_fn(d_i[26])


map(out, "result") %>%
  map("res_out") %>%
  bind_rows(.id = "rep") %>%
  ungroup() %>%
  group_by(rep, site, day_end) %>%
  mutate(smpe = smpe_custom(actual, sim)) %>%
  group_by(site, day_end) %>%
  summarise(smpe = mean(smpe, na.rm = TRUE))  %>%
  filter(site != "nbt") %>%
  ggplot(aes(x = day_end, y = smpe)) +
  geom_col() +
  facet_wrap(vars(site)) +
  theme_minimal() +
  labs(
    y = str_wrap(
      "Symmetric percentage error of numbers of new admissions becoming ready for discharge",
      50
    ),
    x = "Day"
  )


ggsave(
  last_plot(),
  filename = "./validation/validation_plot_new_admits_accumulation.png",
  bg = "white",
  height = 7.5,
  width = 10, 
  scale = 0.6)


(new_admits_plot <- map(out, "res_out") %>%
    bind_rows(.id = "rep") %>%
    group_by(site, day_end) %>%
    summarise(residual = mean(residual, na.rm = TRUE))  %>%
    ggplot(aes(x = day_end, y = residual)) +
    geom_col() +
    
    facet_wrap(vars(site)) + 
    theme_bw() +
    labs(y = str_wrap("Residual numbers of new patients becoming ready for discharge between actual and simulation.", 50))
  
  ggsave(
    new_admits_plot,
    filename = "./validation/validation_plot_new_admits.png",
    scale = 0.3,
    width = 20,
    height = 10
  )
  
  
  
  patchwork::wrap_plots(map(out[1:9], "p"), axes = "collect", guides = "collect") 
  
  ggsave(
    p,
    filename = "./validation/validation_plot_new_admits.png",
    scale = 0.55,
    width = 30,
    height = 15
  )
  