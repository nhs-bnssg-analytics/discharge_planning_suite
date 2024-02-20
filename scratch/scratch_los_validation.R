library(fitdistrplus)
library(tidyverse)
library(tidymodels)
library(lubridate)

source("utils.R")

con <- switch(.Platform$OS.type,
              windows = RODBC::odbcConnect(dsn = "xsw"),
              unix = {"/root/sql/sql_connect_string_linux" |>
                  readr::read_lines() |>
                  RODBC::odbcDriverConnect()}
)


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



# NCTR data summary

los_df <- nctr_df %>%
  filter(Person_Stated_Gender_Code %in% 1:2,
         !is.na(NHS_Number)) %>%
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
  ) %>%
  mutate(los = (Census_Date - Date_Of_Admission) / ddays(1)) %>%
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
    Census_Date,
    Date_Of_Admission,
    nhs_number,
    sex,
    age = Person_Age,
    ctr = Criteria_To_Reside,
    site,
    bed_type = Bed_Type,
    los,
    pathway
  ) %>%
  ungroup() %>%
  group_by(nhs_number, Date_Of_Admission) %>%
  mutate(id = cur_group_id(),
         total_los = max(los)) %>%
  filter(total_los < 50)





attr_df <-
  RODBC::sqlQuery(
    con,
    "select * from (
select a.*, ROW_NUMBER() over (partition by nhs_number order by attribute_period desc) rn from
[MODELLING_SQL_AREA].[dbo].[New_Cambridge_Score] a) b where b.rn = 1"
  )

los_df <- los_df %>%
  ungroup() %>%
  left_join(select(attr_df, -sex, -age) %>% mutate(nhs_number = as.character(nhs_number)),
            by = join_by(nhs_number == nhs_number)) %>%
  dplyr::select(id, age, sex, cambridge_score, bed_type, site, los, total_los) #%>%
#na.omit()

test_df <- los_df %>%
  group_by(id) %>%
  nest() %>% 
  mutate(rows = map_dbl(data, nrow)) %>%
  filter(rows > 2) %>%
  mutate(samp = map(data, slice_sample, n = 1)) %>%
  select(samp) %>%
  unnest(samp) %>%
  ungroup()
  


# pathway model

los_wf <- readRDS("data/los_wf.RDS")

los_dist <- readRDS("data/dist_split.RDS") %>%
  enframe() %>%
  unnest_wider(value)

test_df <- test_df %>%
  select(total_los) %>%
  bind_cols(bake(extract_recipe(los_wf), test_df)) %>%
  mutate(leaf = as.character(treeClust::rpart.predict.leaves(extract_fit_engine(los_wf),
                                                             select(., -total_los)))) %>%
  left_join(los_dist, by = join_by(leaf == name)) %>%
  mutate(los_remaining = pmap_dbl(
    list(los, meanlog, sdlog),
    ~
      rlnormt(
        1,
        meanlog = ..2,
        sdlog = ..3,
        range = c(..1, Inf)
      ) - ..1
  ))
