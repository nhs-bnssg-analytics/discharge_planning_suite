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

source("utils/utils.R")
source("utils/theme.R")
source("utils/colour_functions.R")

# plot intermediate results?
plot_int <- FALSE
seed <- TRUE
n_rep <- 1E0

run_date <- today()
n_days <- 10

# latest nctr data
nctr_df <-
  RODBC::sqlQuery(
    con,
    "SELECT
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
  FROM Analyst_SQL_Area.dbo.vw_NCTR_Status_Report_Daily_JI"
  )


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
n_days <- 10

validation_end <- ymd("2024-09-01")
validation_start <- ymd("2023-07-01")
start_date <- validation_end - dweeks(13) 
nctr_df <- nctr_df %>% filter(between(Census_Date, validation_start, validation_end-ddays(1)))

sites <- nctr_df %>%
  filter(Organisation_Site_Code %in% c('RVJ01', 'RA701', 'RA301', 'RA7C2')) %>%
  mutate(site = case_when(Organisation_Site_Code == 'RVJ01' ~ 'nbt',
                          Organisation_Site_Code == 'RA701' ~ 'bri',
                          Organisation_Site_Code %in% c('RA301', 'RA7C2') ~ 'weston',
                          TRUE ~ '')) %>%
  pull(site) %>%
  unique()

dates <- nctr_df %>%
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