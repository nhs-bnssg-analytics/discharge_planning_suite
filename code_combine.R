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
  # pull(Census_Date) %>%
  group_by(Organisation_Site_Code) %>%
  summarise(max(Census_Date))
  max()

report_start <- max_date + ddays(1)
report_end <- report_start + ddays(n_days)

source("code_new_admits.R")



nctr_df <- nctr_df %>%
  # filter for our main sites / perhaps I shouldn't do this?
  filter(Organisation_Site_Code %in% c('RVJ01', 'RA701', 'RA301', 'RA7C2')) %>%
  # filter for CTR, we wont predict the NCTR outcome for those already NCTR/on a queue
  # filter(Criteria_To_Reside == "N") %>%
  mutate(site = case_when(Organisation_Site_Code == 'RVJ01' ~ 'nbt', 
                          Organisation_Site_Code == 'RA701' ~ 'bri', 
                          Organisation_Site_Code %in% c('RA301', 'RA7C2') ~ 'weston', 
                          TRUE ~ 'other'),
         Date_Of_Admission = as.Date(Date_Of_Admission)) %>%
  group_by(Organisation_Site_Code) %>%
  filter(Census_Date == max(Census_Date)) %>% 
  ungroup() %>%
  mutate(report_date = max(Census_Date)) %>% 
  mutate(los = (report_date - Date_Of_Admission)/ddays(1)) %>%
  mutate(pathway = recode(Current_Delay_Code_Standard,
                          "Uncoded" = "Other",
                          "Repatriation" = "Other",
                          "NCTR Null" = "Other",
                          "Not Set" = "Other",
                          "18a  Infection  bxviii  Standard" = "Other",
                          "xviii. Awaiting discharge to a care home but have not had a COVID 19 test (in 48 hrs preceding discharge)." = "Other",
                          "15b  Repat  bxv  WGH" = "Other"),
         pathway = coalesce(pathway, "Other")) %>%
  mutate(pathway = if_else(!pathway %in% c("P1", "P2", "P3", "P3 / Other Complex Discharge" ,"Other"), "Other", pathway)) %>%
  dplyr::select(report_date, # this is a workaround for bad DQ / census date is not always consistent at time of running
                nhs_number = NHS_Number,
                ctr = Criteria_To_Reside,
                site,
                bed_type = Bed_Type,
                los,
                pathway) %>%
  ungroup()


# LOS predictions

los_df <- nctr_df %>%
  # filter for our main sites / perhaps I shouldn't do this?
  # filter(Organisation_Site_Code %in% c('RVJ01', 'RA701', 'RA301', 'RA7C2')) %>%
  # filter for CTR, we wont predict the NCTR outcome for those already NCTR/on a queue
  filter(ctr == "Y") 


# attributes

attr_df <-
  RODBC::sqlQuery(
    con,
    "select * from (
select a.*, ROW_NUMBER() over (partition by nhs_number order by attribute_period desc) rn from
[MODELLING_SQL_AREA].[dbo].[New_Cambridge_Score] a) b where b.rn = 1"
  )

los_df <- los_df %>%
  left_join(attr_df, by = join_by(nhs_number == nhs_number)) %>%
dplyr::select(age, sex, cambridge_score, bed_type, site, los) %>%
  na.omit() 

# pathway model

rf_wf <- readRDS("data/rf_wf.RDS")

# los_tree

los_wf <- readRDS("data/los_wf.RDS")

los_df <- los_df %>%
  bake(extract_recipe(los_wf), .) %>%
  mutate(leaf = as.character(treeClust::rpart.predict.leaves(extract_fit_engine(los_wf), .))) %>%
  # bind RF pathway predicted probabilities
  bind_cols(predict(rf_wf, los_df, type = "prob"))

# los distributions

los_dist <- readRDS("data/dist_split.RDS") %>%
  enframe() %>%
  unnest_wider(value)


df_pred <- los_df %>%
  mutate(id = 1:n()) %>%
  left_join(los_dist, by = join_by(leaf == name)) %>%
  mutate(los_remaining = pmap(
    list(los, meanlog, sdlog),
    ~
      rlnormt(
        n_rep,
        meanlog = ..2,
        sdlog = ..3,
        range = c(..1, Inf)
      ) - ..1
  )) %>%
  dplyr::select(id, site, los_remaining, starts_with(".pred")) %>%
  unnest(los_remaining) %>%
  mutate(los_remaining = ifelse(los_remaining < 0, 0, los_remaining)) %>%
  mutate(los_remaining = los_remaining %/% 1) %>%
  group_by(site, id) %>%
  mutate(rep = 1:n()) %>%
  group_by(rep, site, los_remaining) %>%
  summarise(across(starts_with(".pred"), list(count = {\(x) sum(x)}))) %>%
  group_by(site, los_remaining) %>%
  summarise(across(starts_with(".pred"), list(
                                              mean = mean,
                                              u95 = {\(x) quantile(x, 0.975)},
                                              l95 = {\(x) quantile(x, 0.025)}
                                              ))) %>% 
  rename_with(.cols = starts_with(".pred"), .fn = \(x) str_remove_all(x, "_count")) %>%
  pivot_longer(cols = -c(site, los_remaining),
               names_to = c("pathway", "metric"),
               names_prefix = ".pred_",
               names_sep = "_") %>%
  # filter(los_remaining == 0) %>%
  pivot_wider(names_from = "metric")

# dataset for plotting (and storing on SQL)

plot_df_pred <- df_pred %>%
  filter(los_remaining %in% 0:5) %>%
  # mutate(pathway = ifelse(los_remaining == 0, pathway, "Not tomorrow")) %>%
  group_by(site, pathway, day = los_remaining) %>%
  summarise(n = sum(mean),
            u95 = sum(u95),
            l95 = sum(l95)) %>%
  mutate(ctr = "Y",
         source = "model_pred",
         report_date = max(nctr_df$report_date)) %>%
  pivot_longer(cols = c(n, u95, l95),
               names_to = "metric",
               values_to = "value")

plot_df_current <- nctr_df %>%
  filter(!is.na(nhs_number), !is.na(ctr)) %>%
  group_by(site, ctr, pathway) %>%
  count() %>%
  mutate(source = "current_ctr_data",
         report_date = max(nctr_df$report_date),
         day = 0) %>%
  pivot_longer(cols = c(n),
               names_to = "metric",
               values_to = "value")


plot_df <- bind_rows(plot_df_pred, 
                     plot_df_current) %>%
  mutate(pathway = factor(pathway, levels = (c("Other", "P1", "P2", "P3 / Other Complex Discharge", "Not tomorrow"))),
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

