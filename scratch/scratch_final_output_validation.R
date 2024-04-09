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

run_date <- ymd("2024-03-25")
n_days <- 10

# latest nctr data
nctr_df <-
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

# max census date

max_date <- nctr_df %>%
  filter(!is.na(NHS_Number)) %>%
  filter(Organisation_Site_Code %in% c('RVJ01', 'RA701', 'RA301', 'RA7C2')) %>%
  pull(Census_Date) %>%
  max()

# NCTR data summary

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
  ) %>%
  group_by(Organisation_Site_Code) %>%
  filter(Census_Date == max(Census_Date)) %>%
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

# report start (i.e. date we start reporting new D2A, i.e. 1 day after max_date)
report_start <- max_date + ddays(1)
report_end <- report_start + ddays(n_days)


attr_df <-
  RODBC::sqlQuery(
    con,
    "select * from (
select a.*, ROW_NUMBER() over (partition by nhs_number order by attribute_period desc) rn from
[MODELLING_SQL_AREA].[dbo].[New_Cambridge_Score] a) b where b.rn = 1"
  )


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
    filter(day <= n_days)  %>%
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
                           u85 = {\(x) quantile(x, 0.85)},
                           l85 = {\(x) quantile(x, 0.15)}
  ))) %>% 
  filter(day <= n_days) %>%
  rename(n = n_mean,
         u85 = n_u85,
         l85 = n_l85)

# Now simulate the queue evolution
source("code_queue_sim.R")

# dataset for plotting (and storing on SQL)

plot_df_pred <- df_pred %>%
  mutate(ctr = "Y",
         source = "model_pred",
         report_date = max_date) %>%
  pivot_longer(cols = c(n, u85, l85),
               names_to = "metric",
               values_to = "value")

plot_df_current <- nctr_sum %>%
  filter(!is.na(nhs_number), !is.na(ctr)) %>%
  group_by(site, ctr, pathway) %>%
  count() %>%
  mutate(ctr = if_else(ctr, "Y", "N"),
         source = "current_ctr_data",
         report_date = max_date,
         day = 0) %>%
  pivot_longer(cols = c(n),
               names_to = "metric",
               values_to = "value")

plot_df_fcast <- df_admit_fcast %>% select(-date)


plot_df <- bind_rows(plot_df_pred, 
                     plot_df_current,
                     plot_df_queue_sim,
                     plot_df_fcast) %>%
  mutate(pathway = factor(pathway, levels = (c("Other", "P1", "P2", "P3"))),
         report_date = as.character(report_date)) # convert date to character because RODBC/R/SQL can't handle writing this in a consistent way


# now empirical values



run_date <- ymd("2024-03-25")

nctr_df <-
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
  Where Census_Date >= '{run_date}'"
    ))

# max census date

# NCTR data summary

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
  ) %>%
  mutate(
    der_los = (as.Date(Census_Date) - as.Date(Date_Of_Admission))/ddays(1),
    der_ctr = case_when(
      Criteria_To_Reside == "Y" | is.na(Criteria_To_Reside) ~ TRUE,
      !is.na(Days_NCTR) ~ FALSE,
      !is.na(Date_NCTR) ~ FALSE,
      Criteria_To_Reside == "N" ~ FALSE
    )) %>%
  mutate(los = (run_date - Date_Of_Admission) / ddays(1)) %>%
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
    # this is a workaround for bad DQ / census date is not always consistent at time of running
    nhs_number,
    sex,
    age = Person_Age,
    ctr = der_ctr,
    Date_NCTR,
    site,
    bed_type = Bed_Type,
    los = der_los,
    pathway
  ) %>%
  ungroup()


nctr_sum %>%
  arrange(Census_Date) %>%
  # remove anyone currently NCTR  
  group_by(nhs_number, Date_Of_Admission) %>%
  mutate(spell_id = cur_group_id()) %>%
  group_by(spell_id) %>%
  filter(Date_Of_Admission > run_date | ctr[1]) %>%
  mutate(der_date_nctr = as.Date(if_else(any(!ctr),min(Census_Date[!ctr]) - ddays(1),max(Census_Date)))) %>%
  ungroup() %>%
  mutate(der_date_nctr = pmin(der_date_nctr, Date_NCTR, na.rm = TRUE)) %>% 
  group_by(spell_id) %>%
  select(site, spell_id, der_date_nctr) %>%
  distinct() %>%
  group_by(site, date = der_date_nctr) %>%
  count() %>%
  mutate(day = (date - run_date)/ddays(1)) %>%
  left_join(plot_df %>%
                filter(source == "model_pred") %>%
                pivot_wider(names_from = "metric", values_from = "value") %>%
                select(site, day, pathway, n_pred = n) %>%
                group_by(site, day) %>%
                summarise(n_pred = sum(n_pred))) %>%
  filter(day <= 10) %>%
  ggplot(aes(x = date, y = n)) +
  geom_col() +
  geom_point(aes(y = n_pred)) +
  facet_wrap(vars(site))

source("shinyApp/colour_functions.R")
source("shinyApp/theme.R")


cols_add <- c(
  "NCTR but not\non D2A queue" = "#999999",
  "Additional P1" = "#8d488d",
  "Additional P2" = "#003087",
  "Additional P3" = "#853358"
)



nctr_sum %>%
  arrange(Census_Date) %>%
  # remove anyone currently NCTR  
  group_by(nhs_number, Date_Of_Admission) %>%
  mutate(spell_id = cur_group_id()) %>%
  group_by(spell_id) %>%
  filter(Date_Of_Admission > run_date | ctr[1]) %>%
  mutate(der_date_nctr = as.Date(if_else(any(!ctr),min(Census_Date[!ctr]) - ddays(1),max(Census_Date)))) %>%
  ungroup() %>%
  mutate(der_date_nctr = pmin(der_date_nctr, Date_NCTR, na.rm = TRUE),
         der_pathway = ifelse(length(pathway[pathway != "Other"]) > 0, head(pathway[pathway != "Other"], 1), "Other")) %>% 
  group_by(spell_id) %>%
  select(site, spell_id, der_date_nctr, pathway) %>%
  distinct() %>% 
  group_by(site, date = der_date_nctr, pathway) %>%
  count() %>%
  ungroup() %>%
  complete(site, date, pathway, fill = list(n = 0)) %>%
  mutate(day = (date - run_date)/ddays(1)) %>%
  left_join(plot_df %>%
                filter(source == "model_pred") %>%
                pivot_wider(names_from = "metric", values_from = "value") %>%
                select(site, day, pathway, n_pred = n, u85, l85)) %>%
  filter(day <= 10) %>%
  mutate(pathway = recode(pathway, 
                          "Other" = "NCTR but not\non D2A queue",
                          "P1" = "Additional P1",
                          "P2" = "Additional P2",
                          "P3" = "Additional P3")) %>%
                          
  ggplot(aes(x = date + ddays(1), y = n)) +
  geom_col(aes(fill = pathway)) +
  geom_point(aes(y = n_pred)) +
  geom_errorbar(aes(ymin = l85, ymax = u85)) +
  bnssgtheme() +
  scale_x_date(date_breaks = "5 days", labels = date_format('%a\n%d %b')) +
  scale_fill_manual(values = cols_add) +
  ggh4x::facet_grid2(site ~ pathway, independent = "y", scales = "free_y", switch = "y")






















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
RODBC::sqlSave(con,
               plot_df,
               tablename = 'discharge_pathway_projections',
               rownames = FALSE,
               append = TRUE)

