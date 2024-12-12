library(fitdistrplus)
library(tidyverse)
library(tidymodels)
library(lubridate)

con <- switch(.Platform$OS.type,
              windows = RODBC::odbcConnect(dsn = "xsw"),
              unix = {"/root/sql/sql_connect_string_linux" |>
                  readr::read_lines() |>
                  RODBC::odbcDriverConnect()}
)

# con <- RODBC::odbcDriverConnect(readr::read_lines("/root/sql/sql_connect_string_linux"))

source("utils/utils.R")
source("utils/theme.R")
source("utils/colour_functions.R")

validation_end <- ymd("2024-09-01")
validation_start <- ymd("2023-07-01")
start_date <- validation_end - dweeks(13) 

seed <- FALSE
plot_int <- FALSE

n_rep <- 5E2

run_date <- today()
n_days <- 10

# latest nctr data
nctr_df_full <-
  RODBC::sqlQuery(
    con,
    glue::glue("SELECT
       [RN]
      ,[Organisation_Code_Provider]
      ,[Organisation_Code_Commissioner]
      ,[Census_Date]
      ,[Month_Date]
      ,[Month]
      ,[Day_Of_Week]
      ,[Week_Start_Date]
      ,[NHS_Number]
      ,[Person_Stated_Gender_Code]
      ,[Person_Age]
      ,[CDS_Unique_Identifier]
      ,[Sub_ICB_Location]
      ,[Organisation_Site_Code]
      ,[Current_Ward]
      ,[Specialty_Code]
      ,[Bed_Type]
      ,[Date_Of_Admission]
      ,[BNSSG]
      ,[Local_Authority]
      ,[Criteria_To_Reside]
      ,[Date_NCTR]
      ,[Current_LOS]
      ,[Days_NCTR]
      ,[Days_NCTR_On_Current_Code]
      ,[Current_Delay_Code]
      ,[Local_Authority_grouped]
      ,[Site_Name]
      ,[Current_Delay_Code_Standard]
      ,[Current_Delay_Code_Detailed]
      ,[Acute Community split]
      ,[Current_Covid_Status]
      ,[Planned_Date_Of_Discharge]
      ,[Date_Toc_Form_Completed]
      ,[Toc_Form_Status]
      ,[Discharge_Pathway]
      ,[DER_File_Name]
      ,[DER_Load_Timestamp]
  FROM Analyst_SQL_Area.dbo.vw_NCTR_Status_Report_Daily_JI
  Where Census_Date <= '{run_date}'"
    ))


pathway_recodes <- c(
  "Pathway 3 - Other" = "P3",
  "Awaiting confirmation MDT" = "Other",
  "Awaiting referral to SPA" = "Other",
  "Pathway 3 - D2A" = "P3",
  "Pathway 0" = "Other",
  "Pathway 1 - D2A" = "P1",
  "Awaiting confirmation Social" = "Other",
  "Pathway 2 - Other" = "P2",
  "Pathway 2 - D2A" = "P2",
  "Pathway 2" = "P2",
  "Pathway 2  Safeguarding concern" = "P2",
  "Pathway 2   Specialist  eg BIRU" = "P2",
  "Awaiting confirmation Other" = "Other",
  "Pathway 1 - Other" = "P1",
  "Pathway 1" = "P1",
  "P3 / Other Complex Discharge" = "P3",
  "Pathway 3 / Other Complex Discharge" = "P3",
  "Uncoded" = "Other",
  "Repatriation" = "Other",
  "NCTR Null" = "Other",
  "Not Set" = "Other",
  "18a  Infection  bxviii  Standard" = "Other",
  "xviii. Awaiting discharge to a care home but have not had a COVID 19 test (in 48 hrs preceding discharge)." = "Other",
  "15b  Repat  bxv  WGH" = "Other",
  "Meets Criteria to Reside" = "Other"
)


# max census date

max_date <- nctr_df_full %>%
  filter(!is.na(NHS_Number)) %>%
  filter(Organisation_Site_Code %in% c('RVJ01', 'RA701', 'RA301', 'RA7C2')) %>%
  pull(Census_Date) %>%
  max()

# attr_df <-
#   RODBC::sqlQuery(
#     con,
#     "select * from (
# select a.*, ROW_NUMBER() over (partition by nhs_number order by attribute_period desc) rn from
# [MODELLING_SQL_AREA].[dbo].[New_Cambridge_Score] a) b where b.rn = 1"
#   )
# 
# saveRDS(attr_df, "data/attr_df.RDS")
attr_df <- readRDS("data/attr_df.RDS")

# validation testing dates


dates <- nctr_df_full  %>%
  filter(between(Census_Date, validation_start, validation_end-ddays(1)))%>%
  filter(Census_Date >= start_date,
         Census_Date < validation_end,
         Census_Date > ymd("2023-07-01"), # data before this are spurious
         Census_Date < max(Census_Date) - ddays(n_days),
         # Data not submitted for UHBW on this day
         Census_Date != ymd("2024-07-17"),
         # remove dates near Christmas
         abs(lubridate::interval(Census_Date, ymd("2023-12-25"))/ddays(1)) > 15
  ) %>%
  pull(Census_Date) %>%
  unique()


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
  # mutate(site = "system") %>%
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


set.seed(123)

output_valid_full_fn <- function(d) {
  require(tidyverse)
  require(tidymodels)
  require(lubridate)
  
  report_start <- d
  report_end <- report_start + ddays(n_days)
  nctr_df <- nctr_df_full
  
  # run the separate codes
  
  # NEW ADMITS
  
  nctr_df <- nctr_df %>% filter(Census_Date <= d)
  source("code_admits_fcast.R", local = TRUE)
  source("code_new_admits.R", local = TRUE)
  
  
  # new admits simulation output
  na_sim_out <- df_new_admit %>%
    group_by(rep, site, day, pathway, source) %>%
    summarise(count = sum(count)) %>%
    group_by(site, day, pathway, source) %>%
    summarise(across(count, list(
      mean = mean,
      u95 = {\(x) quantile(x, 0.925)},
      l95 = {\(x) quantile(x, 0.075)}
    ))) %>% 
    filter(day <= n_days) %>%
    mutate(source = "simulated", metric = "new_admits")%>%
    filter(day <= n_days) %>%
    filter(site != "nbt") %>%
    mutate(date = d + ddays(day-1),
           day = factor(day, levels = 1:10)
    ) %>%
    dplyr::select(site, day, date, source, metric, n = count_mean) %>%
    ungroup() %>%
    complete(nesting(site, day, date), pathway, metric, source, fill = list(n = 0)) %>%
    bind_rows(summarise(mutate(., site = "system"),n = sum(n), .by = -n))
  
  # new admits observed output
  
  na_out_df <-  nctr_sum_full %>%
    ungroup() %>%
    filter(Date_Of_Admission >= d) %>%
    group_by(nhs_number, Date_Of_Admission) %>%
    mutate(spell_id = cur_group_id(),
           der_date_nctr = as.Date(if_else(any(!ctr),min(Census_Date[!ctr]) - ddays(1),max(Census_Date)))) %>%
    # rowwise() %>%
    # mutate(der_date_nctr = min(der_date_nctr, Date_NCTR, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(der_date_nctr = pmin(der_date_nctr, Date_NCTR, na.rm = TRUE)) %>%
    dplyr::select(nhs_number, spell_id, pathway, site, Date_Of_Admission, der_date_nctr) %>%
    distinct() %>%
    mutate(day = (der_date_nctr - d)/ddays(1)) %>%
    filter(between(day, 1, 10)) %>%
    group_by(site, day, pathway) %>%
    count(name = "n") %>%
    ungroup() %>%
    complete(site, day, pathway, fill = list(n = 0)) %>%
    mutate(date = d + ddays(day-1)) %>%
    mutate(day = factor(day, levels = 1:10)) %>%
    ungroup() %>%
    mutate(source = "observed", metric = "new_admits")  %>%
    bind_rows(summarise(mutate(., site = "system"),n = sum(n), .by = -n)) %>%
    bind_rows(na_sim_out)
  
  # CURRENT ADMITS
  
  nctr_df <- nctr_df_full %>% filter(Census_Date == d)
  
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
    # mutate(site = "system") %>%
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
  
  source("code_curr_admits.R", local = TRUE)
  
  df_curr_admits <- df_curr_admits %>%
    bind_rows(summarise(mutate(., site = "system"),count = sum(count), .by = -count)) 
  
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
    mutate(pathway = ifelse(!ctr & any(pathway != "Other"), head(pathway[pathway != "Other"], 1), pathway)) %>%
    mutate(pathway = coalesce(pathway[which(!ctr)[1]], "Other")) %>%
    dplyr::select(nhs_number,
                  site,
                  spell_id,
                  bed_type,
                  los,
                  pathway,
                  discharge_rdy_los) %>%
    # take first active LOS value for each spell
    group_by(spell_id) %>%
    arrange(discharge_rdy_los) %>%
    slice(1) %>%
    mutate(days_until_rdy = (discharge_rdy_los - los)+1) %>%
    group_by(day = days_until_rdy, site, pathway) %>%
    count(name = "n") %>%
    ungroup() %>%
    filter(between(day, 1, 10)) %>%
    complete(site, day, pathway, fill = list(n = 0)) %>%
    mutate(date = d + ddays(day-1)) %>%
    mutate(day = factor(day, levels = 1:10)) %>%
    ungroup()  %>%
    # complete(nesting(site, day, date), pathway, fill = list(n = 0)) %>%
    bind_rows(summarise(mutate(., site = "system"),n = sum(n), .by = -n)) 
  
  ca_out_df <- df_curr_admits %>%
    group_by(rep, site, pathway, day) %>%
    summarise(count = sum(count)) %>%
    group_by(site, pathway, day) %>% 
    summarise(
      n = mean(count),
      u95 = quantile(count, 0.975),
      l95 = quantile(count, 0.025)
    ) %>%
    filter(between(day, 1, 10)) %>%
    mutate(date = d + ddays(day-1)) %>%
    mutate(day = factor(day, levels = 1:10)) %>%
    mutate(source = "simulated", metric = "curr_admits") %>%
    dplyr::select(-u95, -l95) %>%
    ungroup()%>%
    complete(nesting(site, day, date), pathway, fill = list(n = 0)) %>%
    bind_rows(mutate(emp_drain, source = "observed", metric = "curr_admits")) 
  
  
  # baseline model
  nctr_df <- nctr_df_full
  nctr_df <- nctr_df_full %>% filter(Census_Date <= report_start,
                                     Organisation_Site_Code != 'RVJ01') # remove NBT for validation
  
  
  discharges_ts <- nctr_df_full %>%
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
    group_by(NHS_Number, Date_Of_Admission) %>%
    mutate(pathway = ifelse(!der_ctr & any(pathway != "Other"), head(pathway[pathway != "Other"], 1), pathway)) %>%
    mutate(keep_date = case_when(any(!der_ctr) ~ Census_Date[!der_ctr][1], .default = tail(Census_Date, 1))) %>%
    filter(Census_Date < max(Census_Date)) %>%
    dplyr::select(
      date = keep_date,
      nhs_number,
      site,
      pathway
    ) %>%
    ungroup() %>%
    distinct()  %>%
    group_by(site, pathway, date = date + ddays(1)) %>%
    count() %>%
    ungroup() %>%
    complete(nesting(site, date), pathway, fill = list(n = 0))  %>%
    filter(date != max(date),
           date != min(date)) %>%
    filter(date > max(date) - dweeks(4)) 
  
  
  # mean number of discharges per day/site
  bl_out_df <- discharges_ts %>%
    ungroup() %>%
    # filter(date >= (max(date) - dweeks(4))) %>%
    summarise(n = mean(n, na.rm = TRUE), .by = c(site, pathway)) %>%
    expand_grid(day = 1:10) %>%
    mutate(date = d + ddays(day - 1)) %>%
    bind_rows(summarise(mutate(., site = "system"), n = sum(n), .by = -n)) %>%
    mutate(metric = "simulated", source = "baseline")
  
  
  out_ls <- list(na_out_df = na_out_df,
                 ca_out_df = ca_out_df,
                 bl_out_df = bl_out_df)
  
  cat("writing file")
  saveRDS(out_ls, glue::glue("data/intermediate/valid_{d}_nrep_{n_rep}.RDS"))
  out_ls
}

output_valid_full_fn_safe <- safely(output_valid_full_fn)

# read intermediate results

int_files <- list.files(path = "data/intermediate/", full.names = TRUE)

int_dates <- str_split(int_files, "_") %>%
  map_chr(2) %>%
  ymd()

dates_left <- setdiff(dates, int_dates) %>% as_date()

out_int <- map(int_files, readRDS)

# options(future.globals.maxSize = 16000 * 1024^2)
# future::plan(future::multisession, workers = parallel::detectCores() - 10)
# out <- furrr::future_map(dates_left, output_valid_full_fn_safe,
#                          .options = furrr::furrr_options(
#                            seed = TRUE,
#                            globals = c(
#                              "run_date",
#                              "n_days",
#                              "dates_spells",
#                              "nctr_df",
#                              "nctr_df_full",
#                              "nctr_sum_full",
#                              "pathway_recodes",
#                              "plot_int",
#                              "n_rep",
#                              "get_sd_from_ci",
#                              "attr_df",
#                              "max_date",
#                              "seed"
#                            )))


out <- out_int
out <- out %>% map("result")

out %>%
  # map("result") %>%
  map_lgl(is.null) %>%
  which()

bind_rows(
  out %>%
    # map("result") %>%
    map("na_out_df") %>%
    bind_rows(.id = "id") %>%
    filter(site != "nbt") %>%
    complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
    select(id, site, day, pathway, source, n) %>%
    pivot_wider(values_from = n, names_from = source) %>%
    mutate(diff = observed - simulated, metric = "new_admits"),
  out %>%
    # map("result") %>%
    map("ca_out_df") %>%
    bind_rows(.id = "id") %>%
    filter(site != "NBT") %>%
    complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
    select(id, site, day, pathway, source, n) %>%
    pivot_wider(values_from = n, names_from = source) %>%
    mutate(diff = observed - simulated, metric = "curr_admits")
) %>%
  summarise(
    simulated = sum(simulated),
    observed = sum(observed),
    .by = c(id, site, day, pathway)
  ) %>%
    mutate(diff = observed - simulated) %>%
  left_join(
    out %>%
      # map("result") %>%
      map("bl_out_df") %>%
      bind_rows(.id = "id") %>%
      mutate(day = factor(day, levels = 1:10)) %>%
      filter(site != "nbt") %>%
      complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0))
  ) %>%
  mutate(diff_bl = observed - n) %>%
  summarise(
    diff = mean(diff),
    diff_bl = mean(diff_bl),
    .by = c(site, day, pathway)
  ) %>%
  mutate(perf = diff_bl - diff) %>%
  filter(site != "system") %>%
  mutate(site = recode(site, "bri" = "Bristol Royal Infirmary", "weston" = "Weston General Hospital")) %>%
  ggplot(aes(x = as.numeric(day), y = perf)) +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_x_continuous(breaks = 1:10) +
  scale_colour_manual(values = c("green3", "red2")) +
  theme_minimal() +
  ggforce::geom_link2(aes(colour = after_stat(
    ifelse(
      y > 0,
      "Model outperforms baseline",
      "Model underperforms baseline"
    )
  ))) +
  # geom_line() +
  ggh4x::facet_grid2(site ~ pathway, scales = "free_y", independent = "y") +
  labs(x = "Day",
       colour = "",
       y = str_wrap("Difference between baseline model residual and simulation model residual", 50)) +
  theme(legend.position = "bottom")


bind_rows(
  out %>%
    #map("result") %>%
    map("na_out_df") %>%
    bind_rows(.id = "id") %>%
    filter(site != "nbt") %>%
    complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
    select(id, site, day, pathway, source, n) %>%
    pivot_wider(values_from = n, names_from = source) %>%
    mutate(diff = observed - simulated, metric = "new_admits"),
  out %>%
    #map("result") %>%
    map("ca_out_df") %>%
    bind_rows(.id = "id") %>%
    filter(site != "NBT") %>%
    complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
    select(id, site, day, pathway, source, n) %>%
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
     # map("result") %>%
      map("bl_out_df") %>%
      bind_rows(.id = "id") %>%
      mutate(day = factor(day, levels = 1:10)) %>%
      filter(site != "nbt") %>%
      complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
      rename(baseline = n)
  ) %>%
  summarise(across(c(simulated, baseline), \(x) yardstick::rmse_vec(observed, x)), .by = c(site, day, pathway)) %>%
  mutate(ratio = baseline/simulated)%>%
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
    # map("result") %>%
    map("na_out_df") %>%
    bind_rows(.id = "id") %>%
    filter(site != "nbt") %>%
    complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
    select(id, site, day, pathway, source, n) %>%
    pivot_wider(values_from = n, names_from = source) %>%
    mutate(diff = observed - simulated, metric = "new_admits"),
  out %>%
    # map("result") %>%
    map("ca_out_df") %>%
    bind_rows(.id = "id") %>%
    filter(site != "NBT") %>%
    complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
    select(id, site, day, pathway, source, n) %>%
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
      # map("result") %>%
      map("bl_out_df") %>%
      bind_rows(.id = "id") %>%
      mutate(day = factor(day, levels = 1:10)) %>%
      filter(site != "nbt") %>%
      complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
      rename(baseline = n)
  ) %>% 
  mutate(across(c(simulated, baseline), \(x) abs(observed - x))) %>%
  summarise(across(c(simulated, baseline), list(mean = \(x) median(x),
                                                upper = \(x) quantile(x, 0.75),
                                                lower = \(x) quantile(x, 0.25))
                   ), .by = c(site, day, pathway)) %>%
  # mutate(across(matches(c("simulated", "baseline")), sqrt)) %>%
  pivot_longer(-c(site, day, pathway), names_sep = "_", names_to = c("metric", "stat")) %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  mutate(ratio = baseline/simulated) %>%
  select(-simulated, -baseline) %>%
  pivot_wider(names_from = stat, values_from = ratio) %>%
  filter(site != "system") %>%
  ggplot(aes(x = as.numeric(day), y = mean)) +
  geom_hline(yintercept = 1, linetype = 2) +
  scale_x_continuous(breaks = 1:10) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.25) +
  
  scale_colour_manual(values = c("green3", "red2")) +
  theme_minimal() +
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
    #map("result") %>%
    map("na_out_df") %>%
    bind_rows(.id = "id") %>%
    filter(site != "nbt") %>%
    complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
    select(id, site, day, pathway, source, n) %>%
    pivot_wider(values_from = n, names_from = source) %>%
    mutate(diff = observed - simulated, metric = "new_admits"),
  out %>%
    #map("result") %>%
    map("ca_out_df") %>%
    bind_rows(.id = "id") %>%
    filter(site != "NBT") %>%
    complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
    select(id, site, day, pathway, source, n) %>%
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
      # map("result") %>%
      map("bl_out_df") %>%
      bind_rows(.id = "id") %>%
      mutate(day = factor(day, levels = 1:10)) %>%
      filter(site != "nbt") %>%
      complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
      rename(baseline = n)
  ) %>%
  mutate(sim_start_date = min(date), .by = id) %>%
  summarise(across(c(simulated, baseline), \(x) yardstick::rmse_vec(observed, x)), .by = c(site, sim_start_date, pathway)) %>%
  mutate(ratio = baseline/simulated) %>%
  filter(site != "system") %>% 
  mutate(group = str_c(site, "_", pathway)) %>%
  ggplot(aes(x = sim_start_date, y = ratio, group = group)) +
  geom_hline(yintercept = 1, linetype = 2) +
  # scale_x_continuous(breaks = 1:10) +
  scale_colour_manual(values = c("green3", "red2")) +
  theme_minimal() +
  geom_line() +
  # ggforce::geom_link2(aes(colour = after_stat(
  #   ifelse(
  #     y > 1,
  #     "Model outperforms baseline",
  #     "Model underperforms baseline"
  #   )
  # ))) +
  facet_grid(site ~ pathway) +
  # ggh4x::facet_grid2(site ~ pathway, scales = "free_y", independent = "y") +
  labs(x = "Day",
       colour = "",
       y = str_wrap("RMSE ratio between baseline and simulation models", 50)) +
  theme(legend.position = "bottom")


bind_rows(
  out %>%
    # map("result") %>%
    map("na_out_df") %>%
    bind_rows(.id = "id") %>%
    filter(site != "nbt") %>%
    complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
    select(id, site, day, pathway, source, n) %>%
    pivot_wider(values_from = n, names_from = source) %>%
    mutate(diff = observed - simulated, metric = "new_admits"),
  out %>%
    # map("result") %>%
    map("ca_out_df") %>%
    bind_rows(.id = "id") %>%
    filter(site != "NBT") %>%
    complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
    select(id, site, day, pathway, source, n) %>%
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
      # map("result") %>%
      map("bl_out_df") %>%
      bind_rows(.id = "id") %>%
      mutate(day = factor(day, levels = 1:10)) %>%
      filter(site != "nbt") %>%
      complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
      rename(baseline = n)
  ) %>%
  summarise(across(c(simulated, baseline), \(x) yardstick::rmse_vec(observed, x)), .by = c(site, day, pathway)) %>%
  mutate(ratio = baseline/simulated)%>%
  filter(site != "system") %>%
  ggplot(aes(x = as.numeric(day), y = simulated)) +
  # geom_hline(yintercept = 1, linetype = 2) +
  scale_x_continuous(breaks = 1:10) +
  # scale_colour_manual(values = c("green3", "red2")) +
  theme_minimal() +
  geom_line() +
  geom_line(aes(y = baseline), col = "red") +
  # facet_grid(site ~ pathway) +
  ggh4x::facet_grid2(site ~ pathway, scales = "free_y", independent = "y") +
  labs(x = "Day",
       colour = "",
       y = str_wrap("RMSE ratio between baseline and simulation models", 50)) +
  theme(legend.position = "bottom")



discharges_ts <- nctr_df_full %>%
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
  group_by(NHS_Number, Date_Of_Admission) %>%
  mutate(pathway = ifelse(!der_ctr & any(pathway != "Other"), head(pathway[pathway != "Other"], 1), pathway)) %>%
  mutate(keep_date = case_when(any(!der_ctr) ~ Census_Date[!der_ctr][1], .default = tail(Census_Date, 1))) %>%
  filter(Census_Date < max(Census_Date)) %>%
  dplyr::select(
    date = keep_date,
    nhs_number,
    site,
    pathway
  ) %>%
  ungroup() %>%
  distinct()  %>%
  group_by(site, pathway, date = date + ddays(1)) %>%
  count() %>%
  ungroup() %>%
  complete(nesting(site, date), pathway, fill = list(n = 0)) 


discharges_ts %>%
  mutate(calib = ifelse(between(date, validation_start, start_date), "yes", "no" )) %>%
  summarise(n = sum(n), .by = c(site, pathway, calib)) %>%
  mutate(prop = n/sum(n), .by = c(site, calib)) %>%
  ggplot(aes(x = pathway, y = prop, fill = calib)) +
  geom_col(position = "dodge") +
  facet_wrap(vars(site))

%>%
  select(site, pathway, calib, prop) %>%
  pivot_wider(names_from = calib, values_from = prop)

discharges_ts %>%
  filter(date < max(date)) %>%
  filter(date >= max(date) - dweeks(26)) %>%
ggplot(aes(x = date, y = n)) + geom_line() + ggh4x::facet_grid2(site ~ pathway, scales = "free_y", independent = "y") + geom_smooth()


day_cap <- 3

bind_rows(
  out %>%
    #map("result") %>%
    map("na_out_df") %>%
    bind_rows(.id = "id") %>%
    filter(site != "nbt") %>%
    complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
    select(id, site, day, pathway, source, n) %>%
    pivot_wider(values_from = n, names_from = source) %>%
    mutate(diff = observed - simulated, metric = "new_admits"),
  out %>%
    #map("result") %>%
    map("ca_out_df") %>%
    bind_rows(.id = "id") %>%
    filter(site != "NBT") %>%
    complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
    select(id, site, day, pathway, source, n) %>%
    pivot_wider(values_from = n, names_from = source) %>%
    mutate(diff = observed - simulated, metric = "curr_admits")
) %>%
  filter(as.numeric(as.character(day)) <= day_cap) %>%
  summarise(
    simulated = sum(simulated),
    observed = sum(observed),
    .by = c(id, site, pathway)
  ) %>%
  left_join(
    out %>%
      # map("result") %>%
      map("bl_out_df") %>%
      bind_rows(.id = "id") %>%
      mutate(day = factor(day, levels = 1:10)) %>%
      filter(site != "nbt") %>%
      complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
      rename(baseline = n)  %>%
      filter(as.numeric(as.character(day)) <= day_cap) %>%
      summarise(
        baseline = sum(baseline),
        .by = c(id, site, pathway)
      ) 
  ) %>%
  # pivot_longer(cols = c(simulated, observed, baseline)) %>%
  # summarise(value = mean(value), .by = c(site, pathway, name)) %>%
  # mutate(prop = value/sum(value), .by = c(site, name)) %>%
  # select(site, pathway, name, prop) %>%
  # filter(site != "system") %>%
  # ggplot(aes(x = pathway, y = prop, fill = name)) + geom_col(position = "dodge") + facet_wrap(vars(site))

  # pivot_wider(names_from = name, values_from = prop) %>%
  summarise(across(c(simulated, baseline), \(x) yardstick::rmse_vec(observed, x)), .by = c(site, pathway)) %>%
  mutate(ratio = baseline/simulated) %>%
  filter(site != "system") %>%
  ggplot(aes(x = pathway, y = ratio)) +
  geom_hline(yintercept = 1, linetype = 2) +
  geom_col() +
  facet_wrap(vars(site))

+
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
