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

n_rep <- 1E4

run_date <- today()

nctr_df <-
  RODBC::sqlQuery(
    con,
    "SELECT 
       a.HOSPITAL_PROVIDER_SPELL_IDENTIFIER
      ,a.[ACTIVITY_TREATMENT_FUNCTION_CODE]
      ,a.[ADMINISTRATIVE_CATEGORY_CODE_ON_ADMISSION]
      ,a.[CRITERIA_TO_RESIDE]
      ,a.[DESTINATION_OF_DISCHARGE_HOSPITAL_PROVIDER_SPELL]
      ,a.[DISCHARGE_PATHWAY]
      ,a.[DISCHARGE_READY_DATE_HOSPITAL_PROVIDER_SPELL]
      ,a.[Pseudo_NHS_NUMBER]
      ,a.[ORGANISATION_IDENTIFIER_CODE_OF_PROVIDER]
      ,a.[ORGANISATION_SITE_IDENTIFIER_OF_TREATMENT]
      ,a.[PATIENT_CLASSIFICATION_CODE]
      ,a.[PRIMARY_DIAGNOSIS_ICD]
      ,a.[PRIMARY_PROCEDURE_OPCS]
      ,a.[REMAIN_REASON]
      ,a.[REPORTING_PERIOD_END_DATE]
      ,a.[REPORTING_PERIOD_START_DATE]
	    ,b.[START_DATE_HOSPITAL_PROVIDER_SPELL]
	    ,b.[START_TIME_HOSPITAL_PROVIDER_SPELL]
      ,a.[START_DATE_EPISODE]
      ,a.[START_TIME_EPISODE]
      ,a.[WARD_CODE]
      ,a.[WARD_INTENDED_CLINICAL_CARE_INTENSITY]
  FROM [ABI].[FDF].[Inpatient] a
  left join [ABI].[FDF].[Admission] b
  on a.HOSPITAL_PROVIDER_SPELL_IDENTIFIER = b.HOSPITAL_PROVIDER_SPELL_IDENTIFIER"
  )



nctr_df <- nctr_df %>%
  mutate(spec = factor(ACTIVITY_TREATMENT_FUNCTION_CODE)) %>%
  # filter for our main sites / perhaps I shouldn't do this?
  filter(
    ORGANISATION_SITE_IDENTIFIER_OF_TREATMENT %in% c('RVJ01', 'RA701', 'RA301', 'RA7C2')
  ) %>%
  # filter for CTR, we wont predict the NCTR outcome for those already NCTR/on a queue
  # filter(Criteria_To_Reside == "N") %>%
  mutate(
    site = case_when(
      ORGANISATION_SITE_IDENTIFIER_OF_TREATMENT == 'RVJ01' ~ 'nbt',
      ORGANISATION_SITE_IDENTIFIER_OF_TREATMENT == 'RA701' ~ 'bri',
      ORGANISATION_SITE_IDENTIFIER_OF_TREATMENT %in% c('RA301', 'RA7C2') ~ 'weston',
      TRUE ~ 'other'
    ),
    Date_Of_Admission = as.Date(START_DATE_HOSPITAL_PROVIDER_SPELL),
    REPORTING_PERIOD_START_DATE = as.Date(parse_date_time(
      REPORTING_PERIOD_START_DATE, orders = c("ymd", "dmy HMS")
    ))
  ) %>%
  group_by(site) %>%
  filter(REPORTING_PERIOD_START_DATE == max(REPORTING_PERIOD_START_DATE)) %>%
  # distinct(HOSPITAL_PROVIDER_SPELL_IDENTIFIER) %>%
  ungroup() %>%
  distinct() %>%
  # mutate(report_date = max(REPORTING_PERIOD_START_DATE)) %>%
  mutate(los = (REPORTING_PERIOD_START_DATE - Date_Of_Admission) / ddays(1)) %>%
  mutate(
    pathway = recode(
      DISCHARGE_PATHWAY,
      "iv" = "Other",
      "z" = "Other",
      "1C" = "P1",
      "ixc" = "Other",
      "viii" = "Other",
      "x" = "Other",
      "0A" = "P0",
      "v" = "Other",
      "vii" = "Other",
      "ii" = "Other",
      "iii" = "Other",
      "xv" = "Other",
      "xr" = "Other",
      "xii" = "Other",
      "ix" = "Other",
      "3J" = "P3",
      "xi" = "Other",
      "viiio" = "Other",
      "xiii" = "Other",
      "xvii" = "Other",
      "i" = "Other",
      "2G" = "P2",
      "2H" = "P2",
      "xviii" = "Other",
      "99" = "Other",
      "xiv" = "Other",
      "vi" = "Other",
      "xvi" = "Other",
      "1E" = "P1",
      "1A" = "P1",
      "3B" = "P3",
      "2E" = "P2",
      "0B" = "P0",
      "viiih" = "Other",
      "ixh" = "Other",
      "2F" = "P2",
      "1D" = "P1"
    ),
    pathway = coalesce(pathway, "Other")
  ) %>%
  dplyr::select(
    report_date = REPORTING_PERIOD_START_DATE,
    # this is a workaround for bad DQ / census date is not always consistent at time of running
    nhs_number = Pseudo_NHS_NUMBER,
    ctr = CRITERIA_TO_RESIDE,
    site,
    spec,
    # bed_type = Bed_Type,
    los,
    pathway
  ) %>%
  ungroup()


# LOS predictions

los_df <- nctr_df %>%
  # filter for our main sites / perhaps I shouldn't do this?
  # filter(Organisation_Site_Code %in% c('RVJ01', 'RA701', 'RA301', 'RA7C2')) %>%
  # filter for CTR, we wont predict the NCTR outcome for those already NCTR/on a queue
  filter(ctr != "X") 


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
  select(
    -c(
      "nhs_number",
      "site",
      #"spec",
      "attribute_period",
      "spend_12_months",
      "smoking",
      "bmi",
      "ethnicity",
      "practice_code",
      "lsoa",
      "Version",
      "rn"
    )
    ) %>%
  na.omit() 

# pathway model

rf_wf <- readRDS("data/rf_wf.RDS")


# los_tree

los_wf <- readRDS("data/los_wf.RDS")

los_df <- los_df %>%
  bake(extract_recipe(los_wf), .) %>%
  mutate(leaf = as.character(treeClust::rpart.predict.leaves(extract_fit_engine(los_wf), .))) %>%
  # bind RF pathway predicted probabilities
  bind_cols(extract_recipe(rf_wf) %>%
              bake(los_df) %>%
              predict(extract_fit_engine(rf_wf), data = .) %>%
              pluck("predictions") %>%
              as_tibble() %>%
              rename_with(.fn = ~paste0(".pred_", .x)))




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
  select(id, los_remaining, starts_with(".pred")) %>%
  unnest(los_remaining) %>%
  mutate(los_remaining = ifelse(los_remaining < 0, 0, los_remaining)) %>%
  mutate(los_remaining = los_remaining %/% 1) %>%
  group_by(id) %>%
  mutate(rep = 1:n()) %>%
  group_by(rep, los_remaining) %>%
  summarise(across(starts_with(".pred"), list(count = {\(x) sum(x)}))) %>%
  group_by(los_remaining) %>%
  summarise(across(starts_with(".pred"), list(
                                              mean = mean,
                                              u95 = {\(x) quantile(x, 0.975)},
                                              l95 = {\(x) quantile(x, 0.025)}
                                              ))) %>% 
  rename_with(.cols = starts_with(".pred"), .fn = \(x) str_remove_all(x, "_count")) %>%
  pivot_longer(cols = -c(los_remaining),
               names_to = c("pathway", "metric"),
               names_prefix = ".pred_",
               names_sep = "_") %>%
  # filter(los_remaining == 0) %>%
  pivot_wider(names_from = "metric")

# dataset for plotting (and storing on SQL)

plot_df_pred <- df_pred %>%
  mutate(pathway = ifelse(los_remaining == 0, pathway, "Not tomorrow")) %>%
  group_by(pathway) %>%
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
  filter(!is.na(nhs_number)#,
         # !is.na(ctr)
         ) %>%
  group_by(ctr, pathway) %>%
  count() %>%
  mutate(source = "current_ctr_data",
         report_date = max(nctr_df$report_date)) %>%
  pivot_longer(cols = c(n),
               names_to = "metric",
               values_to = "value")


plot_df <- bind_rows(plot_df_pred, 
                     plot_df_current) %>%
  mutate(pathway = factor(pathway, levels = (c("Other", "P1", "P2", "P3", "Not tomorrow"))),
         report_date = as.character(report_date)) # convert date to character because RODBC/R/SQL can't handle writing this in a consistent way

# create the table
# RODBC::sqlQuery(con, query = 'USE modelling_sql_area CREATE TABLE dbo.discharge_pathway_projections  ("pathway" varchar(255), "ctr" varchar(255), "source" varchar(255), "report_date" float, "metric" varchar(255), "value" float)')

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
RODBC::sqlSave(con, plot_df, tablename = 'dbo.discharge_pathway_projections', rownames = FALSE, append = TRUE)