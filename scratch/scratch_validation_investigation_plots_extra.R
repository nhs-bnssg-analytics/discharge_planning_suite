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
# start_date <- validation_end - dweeks(13

fc_train_length_wks <- 10

start_date <- validation_start + dweeks(fc_train_length_wks) # use full period save ARIMA training period

seed <- FALSE
plot_int <- FALSE

n_rep <- 1E1

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


out <- readRDS("data/final_validation_full_out_1e3_newpropsloslogic.RDS")


census_log <- nctr_df_full %>%
  filter(Person_Stated_Gender_Code %in% 1:2) %>%
  mutate(nhs_number = as.character(NHS_Number),
         nhs_number = if_else(is.na(nhs_number), CDS_Unique_Identifier, nhs_number),
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
  group_by(CDS_Unique_Identifier) %>%
  mutate(pathway = ifelse(length(pathway[pathway != "Other"]) > 0, head(pathway[pathway != "Other"], 1), "Other")) %>%
  mutate(start_date = min(Census_Date)) %>%
  mutate(end_date = as.Date(if_else(any(!der_ctr),min(Census_Date[!der_ctr]) - ddays(1),max(Census_Date)))) %>%
  dplyr::select(
    nhs_number,
    id = CDS_Unique_Identifier,
    start_date,
    end_date,
    site,
    pathway
  ) %>%
  ungroup() %>%
  distinct()



calc_new_admit_disch_rdy <- function(d, h = 10, census_log) {
  census_log %>%
    filter(start_date > d, between(end_date, d, d+ddays(h-1))) %>%
    group_by(site, pathway, date = end_date) %>%
    count(name = "observed") %>%
    ungroup() %>%
    complete(site, pathway, date = seq.Date(from = d, length.out = h, by = "day"), fill = list(observed = 0)) %>%
    mutate(day = factor(seq_len(h)), .by = c(site, pathway))
  }

calc_cur_admit_disch_rdy <- function(d, h = 10, census_log) {
  census_log %>%
    filter(start_date <= d, between(end_date, d, d+ddays(h-1))) %>%
    group_by(site, pathway, date = end_date) %>%
    count(name = "observed") %>%
    ungroup() %>%
    complete(site, pathway, date = seq.Date(from = d, length.out = h, by = "day"), fill = list(observed = 0)) %>% 
    mutate(day = factor(seq_len(h)), .by = c(site, pathway))
  }


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


observed <- map(dates, \(d) list(
  new_admits = calc_new_admit_disch_rdy(d, census_log = census_log),
  cur_admits = calc_cur_admit_disch_rdy(d, census_log = census_log)
  )) %>%
map(bind_rows, .id = "source") %>%
bind_rows(.id = "id") 

observed %>%
  summarise(observed = sum(observed), .by = c(id, source, site, pathway, day)) %>% 
  summarise(across(observed, list(mean = mean,
                           uq = \(x) quantile(x, 0.975), 
                           lq = \(x) quantile(x, 0.025))),
            .by = c(site, source, pathway, day)) %>%
  ggplot(aes(x = day, y = observed_mean, ymin = observed_lq, ymax = observed_uq)) +
  geom_pointrange() +
  ggh4x::facet_grid2(pathway~site+source, scales = "free", independent = "y")



rmse <- function(y_true, y_pred, sum_fn = mean) {

  
  # Input validation: Check if inputs are numeric vectors and have the same length
  if (!is.numeric(y_true) || !is.numeric(y_pred)) {
    warning("Inputs must be numeric vectors.")
    return(NA) # Return NA for invalid input
  }
  
  if (length(y_true) != length(y_pred)) {
    warning("True and predicted values must have the same length.")
    return(NA)  # Return NA for invalid input
  }
  
  
  # Calculate the squared differences
  errors <- y_true - y_pred
  squared_errors <- errors^2
  
  # Calculate the mean of the squared errors
  mean_squared_error <- sum_fn(squared_errors)
  
  # Take the square root to get RMSE
  rmse_value <- sqrt(mean_squared_error)
  
  return(rmse_value)
}

bind_rows(
  out %>%
    map("result") %>%
    map("na_out_df") %>%
    bind_rows(.id = "id") %>%
    filter(site != "nbt") %>%
    complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
    dplyr::select(id, site, day, pathway, source, n) %>%
    pivot_wider(values_from = n, names_from = source) %>%
    mutate(diff = observed - simulated, metric = "new_admits"),
  out %>%
    map("result") %>%
    map("ca_out_df") %>%
    bind_rows(.id = "id") %>%
    filter(site != "NBT") %>%
    complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
    dplyr::select(id, site, day, pathway, source, n) %>%
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
      map("result") %>%
      map("bl_out_df") %>%
      bind_rows(.id = "id") %>%
      mutate(day = factor(day, levels = 1:10)) %>%
      filter(site != "nbt") %>%
      complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
      rename(baseline = n)
  ) %>%
  summarise(across(c(simulated, baseline), list(mean = \(x) rmse(y_true = observed, y_pred = x),
                                                uq = \(x) rmse(y_true = observed, y_pred = x, sum_fn = \(x) quantile(x, 0.975)),
                                                lq = \(x) rmse(y_true = observed, y_pred = x, sum_fn = \(x) quantile(x, 0.025))
                                                )), .by = c(site, day, pathway)) %>%
  mutate(
    ratio_mean = baseline_mean/simulated_mean,
    ratio_uq = baseline_uq/simulated_uq,
    ratio_lq = baseline_lq/simulated_lq
    ) %>%
  filter(site != "system") %>%
  ggplot(aes(x = as.numeric(day), y = ratio_mean)) +
  geom_hline(yintercept = 1, linetype = 2) +
  geom_ribbon(aes(ymin = ratio_lq, ymax = ratio_uq), alpha = 0.15) +
  # geom_path() +
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
  # facet_grid(site ~ pathway) +
  ggh4x::facet_grid2(site ~ pathway, scales = "free_y", independent = "y") +
  labs(x = "Day",
       colour = "",
       y = str_wrap("RMSE ratio between baseline and simulation models", 50)) +
  theme(legend.position = "bottom")


bind_rows(
  out %>%
    map("result") %>%
    map("na_out_df") %>%
    bind_rows(.id = "id") %>%
    filter(site != "nbt") %>%
    complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
    dplyr::select(id, site, day, pathway, source, n) %>%
    pivot_wider(values_from = n, names_from = source) %>%
    mutate(source = "new_admits"),
  out %>%
    map("result") %>%
    map("ca_out_df") %>%
    bind_rows(.id = "id") %>%
    filter(site != "NBT") %>%
    complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
    dplyr::select(id, site, day, pathway, source, n) %>%
    pivot_wider(values_from = n, names_from = source) %>%
    mutate(source = "cur_admits")
) %>%
  dplyr::select(-observed) %>%
  left_join(observed) %>%
  summarise(
    simulated = sum(simulated),
    observed = sum(observed),
    .by = c(id, site, day, pathway)
  ) %>%
  left_join(
    out %>%
      map("result") %>%
      map("bl_out_df") %>%
      bind_rows(.id = "id") %>%
      mutate(day = factor(day, levels = 1:10)) %>%
      filter(site != "nbt") %>%
      complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
      rename(baseline = n)
  ) %>%
  mutate(across(c(simulated, baseline), list(error = \(x) observed - x))) %>%
  mutate(across(matches("_error"), list(sq = \(x) x^2))) %>%
  summarise(across(matches("error_sq"), list(mean = \(x) mean(x, na.rm = TRUE),
                                             uq = \(x) quantile(x, 0.975, na.rm = TRUE),
                                             lq = \(x) quantile(x, 0.025, na.rm = TRUE)
                                             )), .by = c(site, day, pathway)) %>%
  mutate(across(matches("error_sq_"), list(root = sqrt))) %>%
  mutate(
    ratio = baseline_error_sq_mean_root/simulated_error_sq_mean_root,
    ratio_uq = baseline_error_sq_uq_root/simulated_error_sq_uq_root,
    ratio_lq = baseline_error_sq_lq_root/simulated_error_sq_lq_root
    ) %>%
  filter(site != "system") %>%
  ggplot(aes(x = as.numeric(day), y = ratio)) +
  geom_hline(yintercept = 1, linetype = 2) +
  scale_x_continuous(breaks = 1:10) +
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
    map("result") %>%
    map("na_out_df") %>%
    bind_rows(.id = "id") %>%
    filter(site != "nbt") %>%
    complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
    dplyr::select(id, site, day, pathway, source, n) %>%
    pivot_wider(values_from = n, names_from = source) %>%
    mutate(source = "new_admits"),
  out %>%
    map("result") %>%
    map("ca_out_df") %>%
    bind_rows(.id = "id") %>%
    filter(site != "NBT") %>%
    complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
    dplyr::select(id, site, day, pathway, source, n) %>%
    pivot_wider(values_from = n, names_from = source) %>%
    mutate(source = "cur_admits")
) %>%
  dplyr::select(-observed) %>%
  left_join(observed) %>%
  summarise(
    simulated = sum(simulated),
    observed = sum(observed),
    .by = c(id, site, day, pathway)
  ) %>%
  left_join(
    out %>%
      map("result") %>%
      map("bl_out_df") %>%
      bind_rows(.id = "id") %>%
      mutate(day = factor(day, levels = 1:10)) %>%
      filter(site != "nbt") %>%
      complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
      rename(baseline = n)
  ) %>%
  mutate(site = recode(site, !!!c("bri" = "Bristol Royal Infirmary", "weston" = "Weston General Hospital"))) %>%
  mutate(across(c(simulated, baseline), list(error = \(x) observed - x))) %>%
  mutate(across(matches("_error"), list(abs = \(x) abs(x)))) %>%
  mutate(
    ratio = baseline_error_abs/simulated_error_abs,
    ) %>%
  summarise(median_ratio = quantile(ratio, 0.5, na.rm = TRUE),
            uq_ratio = quantile(ratio, 0.75, na.rm = TRUE),
            lq_ratio = quantile(ratio, 0.25, na.rm = TRUE),
            .by = c(site, day, pathway)) %>%
  filter(site != "system") %>%
  ggplot(aes(x = as.numeric(day), y = median_ratio)) +
  geom_hline(yintercept = 1, linetype = 2) +
  scale_x_continuous(breaks = 1:10) +
  scale_colour_manual(values = c("green3", "red2")) +
  geom_ribbon(aes(ymax = uq_ratio, ymin = lq_ratio), alpha = 0.15) +
  ggforce::geom_link2(aes(colour = after_stat(
    ifelse(
      y > 1,
      "Model outperforms baseline",
      "Model underperforms baseline"
    )
  ))) +
  facet_grid(site ~ pathway) +
  theme_minimal() +
  ggh4x::facet_grid2(site ~ pathway, scales = "free_y", independent = "y") +
  labs(x = "Day",
       colour = "",
       y = str_wrap("Ratio of baseline and simulation model residuals to the observed values", 50)) +
  theme(legend.position = "bottom")



ggsave(last_plot(),
       filename = "./validation/validation_1_absresid_ratio.png",
       bg = "white",
       width = 10,
       height = 7.5,
       scale = 0.65)



bind_rows(
  out %>%
    map("result") %>%
    map("na_out_df") %>%
    bind_rows(.id = "id") %>%
    filter(site != "nbt") %>%
    complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
    dplyr::select(id, site, day, pathway, source, n) %>%
    pivot_wider(values_from = n, names_from = source) %>%
    mutate(source = "new_admits"),
  out %>%
    map("result") %>%
    map("ca_out_df") %>%
    bind_rows(.id = "id") %>%
    filter(site != "NBT") %>%
    complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
    dplyr::select(id, site, day, pathway, source, n) %>%
    pivot_wider(values_from = n, names_from = source) %>%
    mutate(source = "cur_admits")
) %>%
  dplyr::select(-observed) %>%
  left_join(observed) %>%
  summarise(
    simulated = sum(simulated),
    observed = sum(observed),
    .by = c(id, site, day, pathway)
  ) %>%
  left_join(
    out %>%
      map("result") %>%
      map("bl_out_df") %>%
      bind_rows(.id = "id") %>%
      mutate(day = factor(day, levels = 1:10)) %>%
      filter(site != "nbt") %>%
      complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
      rename(baseline = n)
  ) %>%
  mutate(site = recode(site, !!!c("bri" = "Bristol Royal Infirmary", "weston" = "Weston General Hospital"))) %>%
  mutate(across(c(simulated, baseline), list(error = \(x) observed - x))) %>%
  mutate(across(matches("_error"), list(abs = \(x) abs(x)))) %>%
  mutate(
    ratio = baseline_error_abs - simulated_error_abs,
  ) %>%
  summarise(median_ratio = quantile(ratio, 0.5, na.rm = TRUE),
            uq_ratio = quantile(ratio, 0.75, na.rm = TRUE),
            lq_ratio = quantile(ratio, 0.25, na.rm = TRUE),
            .by = c(site, day, pathway)) %>%
  filter(site != "system") %>%
  ggplot(aes(x = as.numeric(day), y = median_ratio)) +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_x_continuous(breaks = 1:10) +
  scale_colour_manual(values = c("green3", "red2")) +
  geom_ribbon(aes(ymax = uq_ratio, ymin = lq_ratio), alpha = 0.15) +
  ggforce::geom_link2(aes(colour = after_stat(
    ifelse(
      y > 0,
      "Model outperforms baseline",
      "Model underperforms baseline"
    )
  ))) +
  facet_grid(site ~ pathway) +
  theme_minimal() +
  ggh4x::facet_grid2(site ~ pathway, scales = "free_y", independent = "y") +
  labs(x = "Day",
       colour = "",
       y = str_wrap("RMSE ratio between baseline and simulation models", 50)) +
  theme(legend.position = "bottom")


bind_rows(
  out %>%
    map("result") %>%
    map("na_out_df") %>%
    bind_rows(.id = "id") %>%
    filter(site != "nbt") %>%
    complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
    dplyr::select(id, site, day, pathway, source, n) %>%
    pivot_wider(values_from = n, names_from = source) %>%
    mutate(source = "new_admits"),
  out %>%
    map("result") %>%
    map("ca_out_df") %>%
    bind_rows(.id = "id") %>%
    filter(site != "NBT") %>%
    complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
    dplyr::select(id, site, day, pathway, source, n) %>%
    pivot_wider(values_from = n, names_from = source) %>%
    mutate(source = "cur_admits")
) %>%
  dplyr::select(-observed) %>%
  left_join(observed) %>%
  summarise(
    simulated = sum(simulated),
    observed = sum(observed),
    .by = c(id, site, pathway)
  ) %>%
  left_join(
    out %>%
      map("result") %>%
      map("bl_out_df") %>%
      bind_rows(.id = "id") %>%
      mutate(day = factor(day, levels = 1:10)) %>%
      filter(site != "nbt") %>%
      complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
      rename(baseline = n) %>%
      summarise(baseline = sum(baseline), .by = c(id, site, pathway))
  ) %>%
  filter(site != "system") %>%
  mutate(across(c(simulated, baseline), list(error = \(x) observed - x))) %>%
  mutate(across(matches("_error"), list(sq = \(x) x^2))) %>%
  summarise(across(matches("error_sq"), list(mean = \(x) mean(x, na.rm = TRUE)
                                             # uq = \(x) quantile(x, 0.975, na.rm = TRUE),
                                             # lq = \(x) quantile(x, 0.025, na.rm = TRUE)
                                             )), .by = c(site, id, pathway)) %>%
  mutate(across(matches("error_sq_"), list(root = sqrt))) %>%
  mutate(
    ratio = baseline_error_sq_mean_root/simulated_error_sq_mean_root,
    ) %>%
  summarise(across(ratio, list(mean = \(x) median(x, na.rm = TRUE),
                               uq = \(x) quantile(x, 0.75, na.rm = TRUE),
                               lq = \(x) quantile(x, 0.25, na.rm = TRUE))
                   ),
            .by = c(site, pathway)
  ) %>%
  ggplot(aes(x = "", y = ratio_mean, ymin = ratio_lq, ymax = ratio_uq)) +
  geom_hline(yintercept = 1, linetype = 2) +
  geom_pointrange() +
  theme_minimal() +
  facet_grid(site ~ pathway) +
  ggh4x::facet_grid2(pathway ~ site, scales = "free_y", independent = "y") +
  labs(x = "Day",
       colour = "",
       y = str_wrap("RMSE ratio between baseline and simulation models", 50)) +
  theme(legend.position = "bottom")



bind_rows(
  out %>%
    map("result") %>%
    map("na_out_df") %>%
    bind_rows(.id = "id") %>%
    filter(site != "nbt") %>%
    complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
    dplyr::select(id, site, day, pathway, source, n) %>%
    pivot_wider(values_from = n, names_from = source) %>%
    mutate(diff = observed - simulated, metric = "new_admits"),
  out %>%
    map("result") %>%
    map("ca_out_df") %>%
    bind_rows(.id = "id") %>%
    filter(site != "NBT") %>%
    complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
    dplyr::select(id, site, day, pathway, source, n) %>%
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
      map("result") %>%
      map("bl_out_df") %>%
      bind_rows(.id = "id") %>%
      mutate(day = factor(day, levels = 1:10)) %>%
      filter(site != "nbt") %>%
      complete(nesting(id, site, day, date), source, metric, pathway, fill = list(n = 0)) %>%
      rename(baseline = n)
  ) %>%
  mutate(across(c(simulated, baseline), list(error = \(x) observed - x))) %>%
  mutate(across(matches("_error"), list(sq = \(x) x^2))) %>%
  summarise(across(matches("error_sq"), list(mean = mean,
                                             uq = \(x) quantile(x, 0.975),
                                             lq = \(x) quantile(x, 0.025)
                                             )), .by = c(site, day, pathway))%>%
  mutate(across(matches("error_sq_"), list(root = sqrt))) %>%
  mutate(
    ratio = baseline_error_sq_mean_root/simulated_error_sq_mean_root,
    ratio_uq = baseline_error_sq_uq_root/simulated_error_sq_uq_root,
    ratio_lq = baseline_error_sq_lq_root/simulated_error_sq_lq_root
    ) %>%
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
