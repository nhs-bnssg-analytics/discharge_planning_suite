
na_sim_fn <- function(d){
  
  require(tidyverse)
  require(tidymodels)
  # n_rep <- 1E2
  # plot_int <- FALSE
  nctr_df_full <- nctr_df
  nctr_df <- nctr_df %>% filter(Census_Date <= d)
  run_date <- d
  report_start <- d #+ ddays(1)
  report_end <- report_start + ddays(n_days)
  source("code_admits_fcast.R", local = TRUE)
  source("code_new_admits.R", local = TRUE)
  
  sim_out_df <- df_new_admit %>%
  group_by(rep, site, day, source) %>%
  summarise(count = sum(count)) %>%
  group_by(site, day, source) %>%
  summarise(across(count, list(
    mean = mean,
    u95 = {\(x) quantile(x, 0.925)},
    l95 = {\(x) quantile(x, 0.075)}
  ))) %>% 
  filter(day <= n_days) %>%
  mutate(source = "sim")%>%
  filter(day <= n_days) %>%
  mutate(day = factor(day, levels = 1:10)) %>%
  dplyr::select(site, day, source, n = count_mean, u95 = count_u95, l95 = count_l95)
  
  
  # empirical accumulation
  
  act <- nctr_df_full %>%
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
    mutate(der_date_nctr = pmin(der_date_nctr, Date_NCTR, na.rm = TRUE)) %>%
    dplyr::select(nhs_number, spell_id, site, Date_Of_Admission, der_date_nctr) %>%
    distinct() 
  
  
  act_out_df <- act %>%
    mutate(day = (der_date_nctr - d)/ddays(1)) %>%
    filter(day <= 10) %>%
    mutate(day = factor(day, levels = 1:10)) %>%
    group_by(site, day, .drop = FALSE) %>%
    count() %>%
    mutate(source = "actual") 
  
  
  plot_df <- bind_rows(sim_out_df, act_out_df) 
  
  # p <- ggplot() +
  #   geom_col(data = plot_df, aes(x = day, y = n, fill = source), position = "dodge") +
  #   geom_errorbar(data = na.omit(plot_df), aes(x = day, ymin = l95, ymax = u95)) +
  #   facet_wrap(vars(site)) +
  #   theme_bw()
  
  res_out <- plot_df %>%
    dplyr::select(site, day, n, source) %>%
    pivot_wider(names_from = source, values_from = n) %>% 
    mutate(residual = actual - sim) 
  
  admissions <- nctr_df_full %>%
    filter(!is.na(NHS_Number)) %>%
    filter(Organisation_Site_Code %in% c('RVJ01', 'RA701', 'RA301', 'RA7C2')) %>%
    mutate(site = case_when(Organisation_Site_Code == 'RVJ01' ~ 'nbt',
                            Organisation_Site_Code == 'RA701' ~ 'bri',
                            Organisation_Site_Code %in% c('RA301', 'RA7C2') ~ 'weston',
                            TRUE ~ '')) %>%
    group_by(nhs_number = NHS_Number) %>%
    distinct(Date_Of_Admission, .keep_all = TRUE) %>%
    group_by(site , date = as.Date(Date_Of_Admission)) %>%
    count()
  
  fcast_diff_out <- df_admit_fcast %>%
    filter(day > 0) %>%
    pivot_wider(names_from = metric, values_from = value) %>%
    left_join(admissions) %>%
    mutate(diff = n - fcast)
  
  
  list(
    # p = p,
    res_out = res_out,
    fcast_diff_out = fcast_diff_out
    )
  
}

sim_safe <- safely(na_sim_fn)

options(future.globals.maxSize = 16000 * 1024^2)
future::plan(future::multisession, workers = parallel::detectCores() - 8)
out <- furrr::future_map(dates, sim_safe, .options = furrr::furrr_options(
# out <- furrr::future_map(dates[1:100], sim_safe, .options = furrr::furrr_options(
  seed = TRUE,
  globals = c(
    "get_sd_from_ci",
    "n_days",
    "n_rep",
    "nctr_df",
    "plot_int",
    "rdist",
    "seed"
  )
))


saveRDS(out, "data/validation_na_fcast_out.RDS")

map(out, "result") %>%
  map("res_out") %>%
  map_lgl(is.null) %>%
  sum()

map(out, "result") %>%
  map("res_out") %>%
  map_lgl(is.null) %>%
  which()




map(out, "result") %>%
  map("res_out") %>%
  bind_rows(.id = "rep") %>%
  ungroup() %>%
  group_by(rep, site, day) %>%
  mutate(smpe = smpe_custom(actual, sim)) %>%
  # group_by(site, day) %>%
  # summarise(smpe = mean(smpe, na.rm = TRUE))  %>%
  filter(site != "nbt") %>%
  ggplot(aes(x = day, y = smpe)) +
  geom_boxplot() +
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

map(out, "result") %>%
  map("res_out") %>%
  bind_rows(.id = "rep") %>%
  ungroup() %>%
  # summarise(residual = mean(residual, na.rm = TRUE),
  #           .by = c(site, day))  %>%
  filter(site != "nbt") %>%
  ggplot(aes(x = day, y = residual)) +
  geom_boxplot() +
  facet_wrap(vars(site)) +
  theme_minimal() +
  labs(
    y = str_wrap(
      "Difference between observed and simulated patients becoming ready for discharge",
      50
    ),
    x = "Forecast day"
  )

ggsave(
  last_plot(),
  filename = "./validation/validation_plot_new_admits_accumulation_obsdiff.png",
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
  