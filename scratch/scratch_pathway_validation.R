library(fitdistrplus)
library(tidyverse)
library(tidymodels)
library(lubridate)
library(magrittr)

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

pathway_df <- nctr_df %>%
  mutate(pathway = recode(Current_Delay_Code_Standard,
                          "P3 / Other Complex Discharge" = "P3",
                          "18a  Infection  bxviii  Standard" = "Other",
                          "Uncoded" = "Other",
                          "Repatriation" = "Other",
                          "NCTR Null" = "Other",
                          "Not Set" = "Other",
                          "xviii. Awaiting discharge to a care home but have not had a COVID 19 test (in 48 hrs preceding discharge)." = "Other",
                          "15b  Repat  bxv  WGH" = "Other"),
         pathway = coalesce(pathway, "Other")) %>%
  mutate(pathway = if_else(pathway %in% c("Other", "P1", "P2", "P3"), pathway, "Other")) %>%
  filter(Person_Stated_Gender_Code %in% 1:2) %>%
  mutate(nhs_number = as.character(NHS_Number),
         nhs_number = if_else(is.na(nhs_number), glue::glue("unknown_{1:n()}"), nhs_number),
         sex = if_else(Person_Stated_Gender_Code == 1, "Male", "Female")) %>%
  group_by(nhs_number) %>%
  arrange(Census_Date) %>%
  # pathway is FIRST assigned pathway - under the assumption that this best reflects the paitents needs at time of NCTR
  mutate(pathway = ifelse(length(pathway[pathway != "Other"]) > 0, head(pathway[pathway != "Other"], 1), "Other")) %>%
  select(Census_Date,
         nhs_number,
         sex,
         age = Person_Age,
         pathway,
         bed_type = Bed_Type)


attr_df <-
  RODBC::sqlQuery(
    con,
    "select * from (
select a.*, ROW_NUMBER() over (partition by nhs_number order by attribute_period desc) rn from
[MODELLING_SQL_AREA].[dbo].[New_Cambridge_Score] a) b where b.rn = 1"
  )

rf_wf <- readRDS("data/rf_wf.RDS")

test_df <- pathway_df %>%
  left_join(mutate(attr_df, nhs_number = as.character(nhs_number))) %>% 
  # bind RF pathway predicted probabilities
  bind_cols(predict(rf_wf, ., type = "prob")) %>%
  select(Census_Date, pathway, starts_with(".pred")) %>%
  filter(Census_Date < max(Census_Date) - ddays(50)) %>%
  filter(Census_Date > ymd("2023-07-01")) # data before this are spurious 



date_samp <- seq.Date(from = min(test_df$Census_Date), to = max(test_df$Census_Date), by = "2 weeks")


map(date_samp, ~filter(test_df, Census_Date == .x) %>%
      rowwise() %>%
      mutate(samp = sample(x = c("Other", "P1", "P2", "P3"), size = 1, prob = c(.pred_Other, .pred_P1, .pred_P2, .pred_P3 ))) %>%
      ungroup() %>%
      mutate(pathway = factor(pathway),
             samp = factor(samp))) 
  


%>%
  rowwise() %>%
  mutate(samp = sample(x = c("Other", "P1", "P2", "P3"), size = 1, prob = c(.pred_Other, .pred_P1, .pred_P2, .pred_P3 ))) %>%
  ungroup() %>%
  mutate(pathway = factor(pathway),
         samp = factor(samp))

map(
  1:100,
  ~ slice_sample(test_df, n = 10) %>%
    select(pathway, samp) %>%
    map(table) %>%
    map(proportions) %>%
    map(as.numeric) %>%
    reduce(`-`)# %>%
    #abs()
) %>%
 reduce(rbind) %>%
 colMeans() 


  test_df %>%
    select(pathway, samp) %$%
    table(pathway, samp) %>%
    proportions


  
  yardstick::conf_mat(test_df, truth = pathway, estimate = samp) %>%
    autoplot()
  
  
  yardstick::conf_mat(test_df, truth = pathway, estimate = samp) %>%
    summary()
  