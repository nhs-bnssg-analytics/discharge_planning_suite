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

source("utils.R")
source("theme.R")
source("colour_functions.R")

plot_int <- TRUE

n_rep <- 1E3

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


nctr_sum <- nctr_df %>%
  filter(Person_Stated_Gender_Code %in% 1:2) %>%
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
    Date_Of_Admission = as.Date(Date_Of_Admission)
  ) 


nctr_sum %>%
  filter(Census_Date > ymd("2024-03-01")) %>%
  group_by(nhs_number, Date_Of_Admission) %>%
  mutate(spell_id = cur_group_id(),
         diff = (Census_Date - Date_Of_Admission)/ddays(1)) %>%
  group_by(spell_id) %>%
  filter(diff == min(diff)) %>%
  ungroup() %>%
  group_by(diff, site) %>%
  count() %>%
  filter(diff < 10) %>%
  # mutate(site = recode(site, "wgh" = "weston")) %>%
  ggplot(aes(x = diff, y = n)) +
    geom_col() +
    scale_x_continuous(breaks = 1:10) +
    facet_wrap(vars(str_to_upper(recode(site, "weston" = "wgh")))) +
  theme_bw() +
  labs(y = "Count",
       x = "Time between first census date and date of admission")

ggsave("materials/min_time_census.png", last_plot(), width = 10, height = 5, scale = 0.5)  
  