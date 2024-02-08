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

n_rep <- 1E4

run_date <- today()
n_days <- 5

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

# max census date

max_date <- nctr_df %>%
  filter(!is.na(NHS_Number)) %>%
  filter(Organisation_Site_Code %in% c('RVJ01', 'RA701', 'RA301', 'RA7C2')) %>%
  pull(Census_Date) %>%
  max()

# NCTR data summary

nctr_sum <- nctr_df %>%
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
  ) %>%
  group_by(Organisation_Site_Code) %>%
  filter(Census_Date == max(Census_Date)) %>%
  ungroup() %>%
  mutate(report_date = max(Census_Date)) %>%
  mutate(los = (report_date - Date_Of_Admission) / ddays(1)) %>%
  mutate(
    pathway = recode(
      Current_Delay_Code_Standard,
      "P3 / Other Complex Discharge" = "P3",
      "Uncoded" = "Other",
      "Repatriation" = "Other",
      "NCTR Null" = "Other",
      "Not Set" = "Other",
      "18a  Infection  bxviii  Standard" = "Other",
      "xviii. Awaiting discharge to a care home but have not had a COVID 19 test (in 48 hrs preceding discharge)." = "Other",
      "15b  Repat  bxv  WGH" = "Other"
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
    # this is a workaround for bad DQ / census date is not always consistent at time of running
    nhs_number = NHS_Number,
    ctr = Criteria_To_Reside,
    site,
    bed_type = Bed_Type,
    los,
    pathway
  ) %>%
  ungroup()

# report start (i.e. date we start reporting new D2A, i.e. 1 day after max_date)
report_start <- max_date + ddays(1)
report_end <- report_start + ddays(n_days)

source("code_admits_fcast.R")
source("code_new_admits.R")
source("code_curr_admits.R")


if(plot_int){
  bind_rows(df_curr_admits, df_new_admit) %>%
    group_by(site, day, pathway, source) %>%
    summarise(across(count, list(
      mean = mean,
      u95 = {\(x) quantile(x, 0.975)},
      l95 = {\(x) quantile(x, 0.025)}
    ))) %>% 
    filter(day <= 5)  %>%
    ggplot(aes(x = day, y = count_mean, fill = source)) +
    geom_col() +
    facet_grid(pathway ~ site, scales = "free") +
    labs(title = "New additions to D2A queue, by forecast source")
}


df_pred <- bind_rows(df_curr_admits, df_new_admit) %>%
  group_by(site, rep, day, pathway) %>%
  summarise(n = sum(count)) %>% # aggregate over source (current/new admits)
  group_by(site, day, pathway) %>% # compute CIs/mean over reps
  summarise(across(n, list(mean = mean,
                               u95 = {\(x) quantile(x, 0.975)},
                               l95 = {\(x) quantile(x, 0.025)}
                               ))) %>% 
  filter(day <= 5) %>%
  rename(n = n_mean,
         u95 = n_u95,
         l95 = n_l95)


# dataset for plotting (and storing on SQL)

plot_df_pred <- df_pred %>%
  mutate(ctr = "Y",
         source = "model_pred",
         report_date = max_date) %>%
  pivot_longer(cols = c(n, u95, l95),
               names_to = "metric",
               values_to = "value")

plot_df_current <- nctr_sum %>%
  filter(!is.na(nhs_number), !is.na(ctr)) %>%
  group_by(site, ctr, pathway) %>%
  count() %>%
  mutate(source = "current_ctr_data",
         report_date = max_date,
         day = 0) %>%
  pivot_longer(cols = c(n),
               names_to = "metric",
               values_to = "value")


plot_df <- bind_rows(plot_df_pred, 
                     plot_df_current) %>%
  mutate(pathway = factor(pathway, levels = (c("Other", "P1", "P2", "P3"))),
         report_date = as.character(report_date)) # convert date to character because RODBC/R/SQL can't handle writing this in a consistent way

# create the table
# RODBC::sqlQuery(con,
#                 query = 'USE modelling_sql_area CREATE TABLE dbo.discharge_pathway_projections
#                 (
#                 "site" varchar(255),
#                 "pathway" varchar(255),
#                 "day" float,
#                 "ctr" varchar(255),
#                 "source" varchar(255),
#                 "report_date" varchar(255),
#                 "metric" varchar(255),
#                 "value" float)'
#                 )

# change con to write to modelling sql area
RODBC::odbcClose(con)
con <- switch(.Platform$OS.type,
              windows = {
                "driver={SQL Server};server=Xsw-00-ash01;
                 database=MODELLING_SQL_AREA;
                 trusted_connection=true" |>
                 RODBC::odbcDriverConnect()
                },
              unix = {
                "/root/sql/sql_modelling_connect_string_linux" |>
                  readr::read_lines() |>
                  RODBC::odbcDriverConnect()
                }
)
# delete old data
query_delete <- "DELETE FROM MODELLING_SQL_AREA.dbo.discharge_pathway_projections"
RODBC::sqlQuery(con, query_delete)
RODBC::sqlSave(con, plot_df, tablename = 'discharge_pathway_projections', rownames = FALSE, append = TRUE)

