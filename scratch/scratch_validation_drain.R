source("utils/utils.R")
n_rep <- 1E2
validation_end <- ymd("2024-09-01")
validation_start <- ymd("2023-07-01")
start_date <- validation_end - dweeks(13) 

nctr_df_full <- nctr_df %>% filter(between(Census_Date, validation_start, validation_end-ddays(1))) %>%
  ungroup()
rm(nctr_df)

attr_df <- readRDS("data/attr_df.RDS")

dates <- nctr_df_full %>%
  filter(Census_Date >= start_date,
         Census_Date < validation_end,
         Census_Date > ymd("2023-07-01"),
         Census_Date < max(Census_Date) - ddays(n_days),
         # remove dates near Christmas
         abs(lubridate::interval(Census_Date, ymd("2023-12-25"))/ddays(1)) > 15
  ) %>%
  pull(Census_Date) %>%
  unique()


drain_fn <- function(d) {
  
  require(tidyverse)
  # n_rep <- 1E2
  # plot_int <- FALSE
  nctr_df <- nctr_df_full %>% filter(Census_Date == d)
  run_date <- d
  report_start <- d + ddays(1)
  report_end <- report_start + ddays(n_days)
  
  nctr_sum <- nctr_df %>%
    filter(Person_Stated_Gender_Code %in% 1:2) %>%
    mutate(nhs_number = as.character(NHS_Number),
           nhs_number = if_else(is.na(nhs_number), glue::glue("unknown_{1:n()}"), nhs_number),
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
    group_by(Organisation_Site_Code) %>%
    filter(Census_Date == max(Census_Date)) %>%
    filter(site != "nbt") %>%
    mutate(site = fct_drop(site)) %>%
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
    dplyr::select(
      report_date,
      nhs_number,
      sex,
      age = Person_Age,
      ctr = der_ctr,
      site,
      bed_type = Bed_Type,
      los = der_los,
      pathway
    ) %>%
    ungroup()
  
  nctr_sum_full <- nctr_df_full %>%
    filter(Person_Stated_Gender_Code %in% 1:2) %>%
    mutate(nhs_number = as.character(NHS_Number),
           nhs_number = if_else(is.na(nhs_number), glue::glue("unknown_{1:n()}"), nhs_number),
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
    group_by(Organisation_Site_Code) %>%
    filter(site != "nbt") %>%
    mutate(site = fct_drop(site)) %>%
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
    dplyr::select(
      report_date,
      nhs_number,
      spell_id,
      Census_Date,
      Date_Of_Admission,
      Date_NCTR,
      Days_NCTR, 
      sex,
      age = Person_Age,
      ctr = der_ctr,
      site,
      bed_type = Bed_Type,
      los = der_los,
      pathway
    ) %>%
    ungroup()
  
  dates_spells <- nctr_sum_full %>%
    group_by(Census_Date, ctr, Days_NCTR, spell_id) %>%
    distinct() 
  
  dates_spells <- dates_spells %>% 
    bind_rows(dates_spells %>% mutate(spell_id = paste0(spell_id, "_sys", sep = "")))
  
  source("code_curr_admits.R", local = TRUE)
  
  
  sid_i <- dates_spells %>%
    filter(Census_Date == d & ctr) %>%
    pull(spell_id) %>% 
    unique()
  
  
  # Calculate the drain for these patients
  emp_drain <- nctr_sum_full %>%
    #first, only filter spells we are interested in
    filter(spell_id %in% sid_i) %>%
    filter(Census_Date >=d) %>%
    arrange(Census_Date) %>%
    group_by(spell_id) %>%
    mutate(los = min(los), # los on index date is the minimum LOS
           der_date_nctr = as.Date(if_else(any(!ctr),min(Census_Date[!ctr]) - ddays(1),max(Census_Date)))) %>%
    ungroup() %>%
    mutate(der_date_nctr = pmin(der_date_nctr, Date_NCTR, na.rm = TRUE)) %>% 
    group_by(spell_id) %>%
    mutate(discharge_rdy_los = (der_date_nctr - as.Date(Date_Of_Admission))/ddays(1)) %>%
    select(nhs_number,
           site,
           spell_id,
           bed_type,
           los,
           discharge_rdy_los) %>%
    # take first active LOS value for each spell
    group_by(spell_id) %>%
    arrange(discharge_rdy_los) %>%
    slice(1) %>%
    mutate(days_until_rdy = (discharge_rdy_los - los)+1) %>%
    group_by(day = days_until_rdy, site) %>%
    count(name = "value") %>%
    filter(between(day, 1, 10))
  
  df_curr_admits %>%
    group_by(rep, site, day) %>%
    summarise(count = sum(count)) %>%
    group_by(site, day) %>% 
    summarise(
              value = mean(count),
              count_u95 = quantile(count, 0.975),
              count_l95 = quantile(count, 0.025)
              ) %>%
    filter(between(day, 1, 10)) %>%
    mutate(source = "Simulated") %>%
    bind_rows(mutate(emp_drain, source = "Observed"))
  
}


foo <- drain_fn(dates[10])
