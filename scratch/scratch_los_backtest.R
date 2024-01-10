library(fitdistrplus)
library(tidyverse)
library(tidymodels)
library(lubridate)


source("utils.R")
n_rep <- 1E4

con <- switch(.Platform$OS.type,
              windows = RODBC::odbcConnect(dsn = "xsw"),
              unix = xswauth::modelling_sql_area()
)


nctr_df <-
  RODBC::sqlQuery(
    con,
    "SELECT [RN]
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



nctr_ts <- nctr_df %>%
  filter(!is.na(NHS_Number)) %>%
  group_by(nhs_number = NHS_Number, arrival_date = Date_Of_Admission) %>%
  mutate(id = cur_group_id()) %>%
  group_by(nhs_number) %>%
  arrange(Census_Date) %>%
  group_by(nhs_number, arrival_date) %>%
  summarise(date_nctr = as.Date(if_else(any(!is.na(Date_NCTR)), Date_NCTR[!is.na(Date_NCTR)][1], max(Census_Date) + days(1)))) %>%
  group_by(date_nctr) %>%
  count() %>%
  arrange(date_nctr) %>%
  filter(year(date_nctr) < 2024, date_nctr > ymd("2023-09-01")) 
  
## atrributes

attr_df <-
  RODBC::sqlQuery(
    con,
    "select * from (
select a.*, ROW_NUMBER() over (partition by nhs_number order by attribute_period desc) rn from
[MODELLING_SQL_AREA].[dbo].[New_Cambridge_Score] a) b where b.rn = 1"
  )



los_df <- nctr_df %>%
  # filter for our main sites / perhaps I shouldn't do this?
  # filter for CTR, we wont predict the NCTR outcome for those already NCTR/on a queue
  filter(Criteria_To_Reside == "Y",
         !is.na(NHS_Number)) %>%
  mutate(site = case_when(Organisation_Site_Code == 'RVJ01' ~ 'nbt', 
                          Organisation_Site_Code == 'RA701' ~ 'bri', 
                          Organisation_Site_Code %in% c('RA301', 'RA7C2') ~ 'weston', 
                          TRUE ~ ''),
         Date_Of_Admission = as.Date(Date_Of_Admission)) %>%
  mutate(los = (Census_Date - Date_Of_Admission)/ddays(1),
         date = as.Date(Census_Date)) %>%
  dplyr::select(date, nhs_number = NHS_Number, site, bed_type = Bed_Type, los) %>%
  left_join(attr_df, by = join_by(nhs_number == nhs_number)) %>%
  select(date, age, sex, cambridge_score, bed_type, los) %>%
  na.omit() %>%
  group_by(date) %>%
  nest()


# models
los_wf <- readRDS("data/los_wf.RDS")
# los distributions
los_dist <- readRDS("data/dist_split.RDS") %>%
  enframe() %>%
  unnest_wider(value)


# function to generate predictions

pathway_pred <- function(data){
  
  # predict pathways (we do this first as they're going to be the same for any given los)
  
  data <- data %>%
    bake(extract_recipe(los_wf), .) %>%
    mutate(leaf = as.character(treeClust::rpart.predict.leaves(extract_fit_engine(los_wf), .)))
  
  df_pred <- data %>%
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
    select(id, los_remaining) %>%
    unnest(los_remaining) %>%
    mutate(los_remaining = los_remaining %/% 1) %>%
    group_by(id) %>%
    mutate(rep = 1:n()) %>%
    group_by(rep, los_remaining) %>%
    count() %>%
    group_by(los_remaining) %>%
    summarise(across(n, list(
      mean = mean,
      u95 = {\(x) quantile(x, 0.975)},
      l95 = {\(x) quantile(x, 0.025)}
    ))) %>% 
    filter(los_remaining == 0) 
  
  df_pred  
}


preds <- los_df %>%
  tail(100) %>%
  mutate(pred = map(data, pathway_pred))

pred_ts <- preds %>%
  unnest(pred) %>%
  select(-c(data, los_remaining))

nctr_ts %>%
  filter(date_nctr %within% interval(min(pred_ts$date), max(pred_ts$date))) %>%
  ggplot(aes(x = date_nctr, y = n)) +
  geom_line() +
  geom_line(data = pred_ts, aes(x = date, y = n_mean), linetype = 2)


pred_ts %>%
  left_join(pathway_ts, by = join_by(pathway == pathway, date == first_pathway_date)) %>%
  group_by(date, pathway) %>%
  mutate(rmse = rmse_vec(n, mean)) %>%
  group_by(pathway) %>%
  summarise(mean(rmse, na.rm = TRUE))

