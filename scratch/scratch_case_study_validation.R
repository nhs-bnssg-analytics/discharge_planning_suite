library(fitdistrplus)
library(tidyverse)
library(tidymodels)
library(lubridate)
library(dbplyr)

df_pred <- readRDS("data/plot_df_ex.RDS")

con <- DBI::dbConnect(odbc::odbc(), "xsw")

nctr_tbl <- tbl(con,
               in_catalog(catalog = "Analyst_SQL_Area",
                          schema = "dbo",
                          table = "vw_NCTR_Status_Report_Daily_JI")
)

# report_range <- ymd(df_pred$report_date[1]) + c(ddays(1), ddays(10))

pathway_recodes <- c(
  "Pathway 3 - Other" = "P3",
  "Awaiting confirmation MDT" = "Other",
  "Awaiting referral to SPA" = "Other",
  "Pathway 3 - D2A" = "P3",
  "Pathway 0" = "Other",
  "Other" = "Other",
  "Pathway 0  Awaiting Asylum Immigration" = "Other",
  "Pathway 1 - D2A" = "P1",
  "Pathway 1 - Other" = "P1",
  "Pathway 2 - Other" = "P2",
  "Pathway 3 - Other" = "P3",
  "Awaiting confirmation Social" = "Other",
  "Pathway 2 - Other" = "P2",
  "Pathway 2 - D2A" = "P2",
  "Pathway 2" = "P2",
  "Pathway 2  Safeguarding concern" = "P2",
  "Pathway 2   Specialist  eg BIRU" = "P2",
  "Pathway 2   Sub Acute Needs not able to be met by " = "P2",
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

report_range <- ymd("2025-10-15") + c(-ddays(0), ddays(9))

nctr_sum <- nctr_tbl %>%
  collect() %>%
  # data within the forecasting horizon
  filter(between(Census_Date, report_range[1]-ddays(10), report_range[2])) %>% # - 10 days to add a buffer for boundary for date NCTR
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
  ungroup() %>%
  mutate(
    der_los = (as.Date(Census_Date) - as.Date(Date_Of_Admission))/ddays(1),
    der_ctr = case_when(
      Criteria_To_Reside == "Y" | is.na(Criteria_To_Reside) ~ TRUE,
      !is.na(Days_NCTR) ~ FALSE,
      !is.na(Date_NCTR) ~ FALSE,
      Criteria_To_Reside == "N" ~ FALSE
    )) %>%
  mutate(report_date = report_range[1]) %>%
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
    date = Census_Date,
    nhs_number,
    sex,
    age = Person_Age,
    ctr = der_ctr,
    site,
    spec = Specialty_Code,
    bed_type = Bed_Type,
    los = der_los,
    pathway
  ) %>%
  ungroup()



df_act <- nctr_sum %>%
  filter(!ctr) %>%
  filter(date == min(date, na.rm = TRUE), .by = nhs_number) %>%
  filter(between(date, report_range[1], report_range[2])) %>%
  group_by(date, site, pathway) %>%
  count(name = "value") %>%
  # complete(date, site, pathway, fill = list(value = 0)) %>%
  ungroup() %>%
  mutate(src = "actual",
        day = interval(report_range[1]-ddays(1), date)/ddays(1)) %>%
  select(-date)



df_pred %>%
  filter(source == "model_pred", metric == "n", between(day, 1, 10)) %>%
  select(site, day, pathway, value) %>% 
  mutate(src = "modelled") %>%
bind_rows(df_act) %>%
  ggplot(aes(x = day, y = value, fill = src)) +
  geom_col(position = position_dodge(preserve = "single")) +
  facet_grid(pathway~site, scales = "free")


