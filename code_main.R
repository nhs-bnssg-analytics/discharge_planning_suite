library(fitdistrplus)
library(tidyverse)
library(tidymodels)
library(lubridate)
library(dbplyr)
library(dtplyr)
library(RMySQL)
library(fable)
library(fabletools)

con <- switch(.Platform$OS.type,
              windows = RODBC::odbcConnect(dsn = "xsw"),
              unix = {
                conn_str <- readr::read_lines("/root/sql/sql_connect_string_linux")
                DBI::dbConnect(odbc::odbc(), .connection_string = conn_str)
              })

source("utils/utils.R")
source("utils/theme.R")
source("utils/colour_functions.R")

# plot intermediate results?
plot_int <- TRUE
seed <- FALSE

n_rep <- 1E2
n_days <- 10

nctr_tbl <- tbl(con,
                in_catalog(
                  catalog = "analyst_sql_area",
                  schema = "dbo",
                  table = "vw_NCTR_Status_Report_Daily_JI"
                )
) 


pds <- tbl(con,
           in_catalog(
             catalog = "analyst_sql_area",
             schema = "dbo",
             table = "tbl_bnssg_datasets_combined_PDS"
           )
) %>%
  mutate(nhs_number = as.character(Pseudo_NHS_Number))


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
  "Meets Criteria to Reside" = "Other",
  "Uncoded" = "Other",
  "Pathway 2" = "P2",
  "Other"= "Other",
  "Pathway 1" = "P1",
  "Repatriation" = "Other",
  "NCTR Null"= "Other",
  "Awaiting referral to SPA" = "Other",
  "Awaiting confirmation MDT" = "Other",
  "Pathway 2 - Other" = "P2",
  "Pathway 1 - D2A" = "P1",
  "Pathway 3 - Other" = "P3",
  "Not Set" = "Other",
  "Pathway 0" = "P0",
  "Pathway 3 / Other Complex Discharge" = "P3",
  "Pathway 3 - D2A" = "P3",
  "Awaiting confirmation Other" = "Other",
  "Pathway 2 - D2A" = "P2",
  "Pathway 1 - Other" = "P1",
  "Awaiting confirmation Social" = "Other"
)


nctr_df <- nctr_tbl %>%
  left_join(
    pds %>% 
      select(pds_nhs_number = Pseudo_NHS_Number, la_pds = Locality_Area) %>% 
      distinct(),
    by = join_by(NHS_Number == pds_nhs_number)
  ) %>%
  filter(Organisation_Site_Code %in% c('RVJ01', 'RA701', 'RA301', 'RA7C2')) %>%
  filter(!is.na(NHS_Number)) %>% # cant join attributes otherwise
  window_order(Census_Date, .by = CDS_Unique_Identifier) %>%
  mutate(la = last(Local_Authority_grouped), .by = CDS_Unique_Identifier) %>%
  ungroup() %>%
  mutate(
    la = if_else(la == "Other", replace(la_pds, " Area", ""), la),
    la = case_when(
      la == "NOT BNSSG" | la == "Other" | is.na(la) ~ "Other",
      TRUE ~ la
    ),
    la = tolower(la) 
  ) %>%
  collect()


# max census date
max_date <- nctr_df %>%
  filter(!is.na(NHS_Number)) %>%
  filter(Organisation_Site_Code %in% c('RVJ01', 'RA701', 'RA301', 'RA7C2')) %>%
  slice_max(order_by = Census_Date, by = Organisation_Site_Code) %>%
  pull(Census_Date) %>%
  as.Date() %>%
  min() 

# recode LA
# nctr_df <- nctr_df %>%
#   mutate(la = case_when(
#     Local_Authority == "SOUTH GLOUCESTERSHIRE COUNCIL" ~ "south gloucestershire",
#     Local_Authority == "South Glos" ~ "south gloucestershire",
#     Local_Authority == "NORTH SOMERSET COUNCIL" ~ "north somerset",
#     Local_Authority == "North Somerset" ~ "north somerset",
#     Local_Authority == "BRISTOL CITY COUNCIL" ~ "bristol",
#     Local_Authority == "Bristol" ~ "bristol",
#     .default = "Other"
#   )) 


# NCTR data summary

nctr_sum <- nctr_df %>%
  filter(Person_Stated_Gender_Code %in% 1:2) %>%
  mutate(nhs_number = as.character(NHS_Number),
         nhs_number = if_else(is.na(nhs_number), glue::glue("unknown_{1:n()}"), nhs_number),
         sex = if_else(Person_Stated_Gender_Code == 1, "Male", "Female")) %>%
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
  filter(Census_Date == max_date) %>%
  ungroup() %>%
  mutate(
    der_los = (as.Date(Census_Date) - as.Date(Date_Of_Admission))/ddays(1),
    der_ctr = case_when(
      Criteria_To_Reside == "Y" | is.na(Criteria_To_Reside) ~ TRUE,
      !is.na(Days_NCTR) ~ FALSE,
      !is.na(Date_NCTR) ~ FALSE,
      Criteria_To_Reside == "N" ~ FALSE
    )) %>%
  mutate(report_date = max_date) %>%
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
  pivot_longer(
    cols = c(site, la),
    names_to = "grp_type",
    values_to = "grp",
  ) %>%
  dplyr::select(
    report_date,
    nhs_number,
    sex,
    age = Person_Age,
    ctr = der_ctr,
    grp,
    spec = Specialty_Code,
    bed_type = Bed_Type,
    los = der_los,
    pathway
  ) %>%
  ungroup()

# (DEPRECATED) report start (i.e. date we start reporting new D2A - 1 day after max_date)
# report start (i.e. date we start reporting new D2A - day of latest census)
report_start <- max_date
report_end <- report_start + ddays(n_days)


attr_df <-
  RODBC::sqlQuery(
    RODBC::odbcDriverConnect("driver={SQL Server};\n  server=Xsw-00-ash01;\n  trusted_connection=true"),
    "select * from (
select a.*, ROW_NUMBER() over (partition by nhs_number order by attribute_period desc) rn from
[MODELLING_SQL_AREA].[dbo].[New_Cambridge_Score] a) b where b.rn = 1"
  )


nctr_df <- nctr_df %>%
  filter(Census_Date <= report_start)

source("code_admits_fcast.R")
source("code_new_admits.R")
source("code_curr_admits.R")

if(plot_int){
  bind_rows(df_curr_admits, df_new_admit) %>%
    group_by(grp, day, pathway, source) %>%
    summarise(across(count, list(
      mean = mean,
      u85 = {\(x) quantile(x, 0.925)},
      l85 = {\(x) quantile(x, 0.075)}
    ))) %>% 
    filter(between(day, 1, n_days))  %>%
    ggplot(aes(x = day, y = count_mean, fill = source)) +
    geom_col() +
    facet_grid(pathway ~ grp, scales = "free") +
    labs(title = "New additions to D2A queue, by forecast source")
}


df_pred <- bind_rows(df_curr_admits, df_new_admit) %>%
  group_by(grp, rep, day, pathway) %>%
  summarise(n = sum(count)) %>% # aggregate over source (current/new admits)
  group_by(grp, day, pathway) %>% # compute CIs/mean over reps
  summarise(across(n, list(mean = mean,
                           u85 = {\(x) quantile(x, 0.925)},
                           l85 = {\(x) quantile(x, 0.075)}
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
  group_by(grp, ctr, pathway) %>%
  count() %>%
  mutate(ctr = if_else(ctr, "Y", "N"),
         source = "current_ctr_data",
         report_date = max_date,
         day = 0) %>%
  pivot_longer(cols = c(n),
               names_to = "metric",
               values_to = "value")

plot_df_fcast <- df_admit_fcast %>% dplyr::select(-date)


plot_df <- bind_rows(plot_df_pred, 
                     plot_df_current,
                     plot_df_queue_sim,
                     plot_df_fcast) %>%
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
# RODBC::odbcClose(con)

con <- switch(
  .Platform$OS.type,
  windows = {
    dbConnect(
      odbc::odbc(),
      Driver = "SQL Server",
      Server = "Xsw-00-ash01",
      Database = "MODELLING_SQL_AREA",
      Trusted_Connection = "True"
    )
  },
  unix = {
    "/root/sql/sql_modelling_connect_string_linux" |>
      readr::read_lines() |>
      RODBC::odbcDriverConnect()
  }
)

dbWriteTable(
  con, 
  name = Id(schema = "BNSSG\\Nick.Howlett", table = "discharge_pathway_projections"), 
  value = plot_df, 
  overwrite = TRUE
)

# Write to ICS MySQL db

host <- Sys.getenv("DB_HOST")
dbname <- Sys.getenv("DB_NAME")
user <- Sys.getenv("DB_USER")
password <- Sys.getenv("DB_CRED")

# Create the connection
conn <- DBI::dbConnect(DBI::dbDriver("MySQL"),
                  dbname = dbname,
                  host = host,
                  port = 3306,
                  user = user,
                  password=password)


# delete old data
query_delete <- str_c("DELETE FROM discharge_pathway_projections")
DBI::dbGetQuery(conn, query_delete)
DBI::dbWriteTable(conn, "discharge_pathway_projections", value = plot_df, overwrite = TRUE, row.names = FALSE)

