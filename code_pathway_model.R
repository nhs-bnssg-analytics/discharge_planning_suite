library(fitdistrplus)
library(tidyverse)
library(tidymodels)


con <- switch(.Platform$OS.type,
              windows = RODBC::odbcConnect(dsn = "xsw"),
              unix = xswauth::modelling_sql_area()
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

validation_end <- ymd("2024-09-01")
validation_start <- ymd("2023-07-01")
weeks_test <- 13
nctr_df <- nctr_df %>% filter(between(Census_Date, validation_start, validation_end-ddays(1)))

pathway_df <- nctr_df %>%
  mutate(pathway = recode(Current_Delay_Code_Standard,
                          !!!pathway_recodes),
         pathway = coalesce(pathway, "Other")) %>%
  mutate(pathway = if_else(pathway %in% c("Other", "P1", "P2", "P3"), pathway, "Other")) %>%
  filter(Person_Stated_Gender_Code %in% 1:2) %>%
  mutate(nhs_number = as.character(NHS_Number),
         nhs_number = if_else(is.na(nhs_number), glue::glue("unknown_{1:n()}"), nhs_number),
         sex = if_else(Person_Stated_Gender_Code == 1, "Male", "Female")) %>%
  group_by(nhs_number, Date_Of_Admission) %>%
  mutate(spell_id = cur_group_id()) %>%
  group_by(spell_id) %>%
  mutate(
    der_los = (as.Date(Census_Date) - as.Date(Date_Of_Admission))/lubridate::ddays(1),
    der_ctr = case_when(
      Criteria_To_Reside == "Y" | is.na(Criteria_To_Reside) ~ TRUE,
      !is.na(Days_NCTR) ~ FALSE,
      !is.na(Date_NCTR) ~ FALSE,
      Criteria_To_Reside == "N" ~ FALSE
    ),
    # der_date_nctr = as.Date(if_else(any(!der_ctr),min(Census_Date[!der_ctr]) - lubridate::ddays(1),max(Census_Date)))) %>%
    der_date_nctr = as.Date(if_else(any(!der_ctr),min(Census_Date[!der_ctr]) ,max(Census_Date)))) %>%
  ungroup() %>%
  mutate(der_date_nctr = pmax(Date_Of_Admission, pmin(der_date_nctr, Date_NCTR, na.rm = TRUE), na.rm = TRUE)) %>%
  mutate(der_date_nctr = as.Date(der_date_nctr)) %>%
  group_by(nhs_number) %>%
  arrange(Census_Date) %>%
  group_by(nhs_number, Date_Of_Admission) %>%
  mutate(pathway = ifelse(!der_ctr & any(pathway != "Other"), head(pathway[pathway != "Other"], 1), pathway)) %>%
  # mutate(pathway = ifelse(any(!der_ctr), pathway[!der_ctr][1], "Other")) %>%
  ungroup() %>%
  group_by(nhs_number) %>%
  reframe(Census_Date = Census_Date[1],
          date_nctr = der_date_nctr[1],
          pathway = ifelse(length(pathway[pathway != "Other"]) > 0, head(pathway[pathway != "Other"], 1), "Other"),
          sex = sex[1],
          age = Person_Age[1],
          spec = Specialty_Code[1],
          bed_type = Bed_Type[1]) %>%
  dplyr::select(
         Census_Date,
         site,
         date_nctr,
         nhs_number,
         sex,
         age,
         pathway,
         #spec, # spec is too multinomial
         bed_type) %>%
  na.omit()


mortality_df <- local({
  string_mortality <-"SELECT
      [Derived_Pseudo_NHS]
      ,[Dec_Age_At_Death]
      ,[DEC_SEX]
      ,[DEC_SEX_DESC]
      ,[DEC_MARITAL_STATUS]
      ,[DEC_MARITAL_STATUS_DESC]
      ,[DEC_AGEC]
      ,[DEC_AGECUNIT]
      ,[DEC_AGECUNIT_DESC]
      ,[REG_DATE_OF_DEATH]
      ,[REG_DATE]
  FROM [ABI].[Civil_Registration].[Mortality]"
  con<-RODBC::odbcDriverConnect("driver={SQL Server};\n  server=Xsw-00-ash01;\n  trusted_connection=true")
  
  RODBC::sqlQuery(con, string_mortality) %>%
    na.omit() %>%
    # filter(REG_DATE < ymd("2024-09-04")) %>%
    filter(REG_DATE < validation_end) %>%
    mutate(Derived_Pseudo_NHS = as.character(Derived_Pseudo_NHS),
           REG_DATE_OF_DEATH = lubridate::ymd(REG_DATE_OF_DEATH)) %>%
    mutate(Derived_Pseudo_NHS = as.character(Derived_Pseudo_NHS)) %>%
    select(nhs_number = Derived_Pseudo_NHS, date_death = REG_DATE_OF_DEATH)
})

# remove patients who died

pathway_df <- pathway_df %>%
  left_join(mortality_df) %>% 
  filter(is.na(date_death) | date_death > date_nctr) %>%
  select(-date_death)

# attributes to join

# attr_df <-
#   RODBC::sqlQuery(
#     con,
#     "select * from (
# select a.*, ROW_NUMBER() over (partition by nhs_number order by attribute_period desc) rn from
# [MODELLING_SQL_AREA].[dbo].[New_Cambridge_Score] a) b where b.rn = 1"
#   )

attr_df <- readRDS("data/attr_df.RDS")


# modelling
model_df <- pathway_df %>%
  left_join(dplyr::select(attr_df, -sex, -age) %>% mutate(nhs_number = as.character(nhs_number)),
            by = join_by(nhs_number == nhs_number)) %>%
  dplyr::select(Census_Date,
         pathway,
         #site,
         cambridge_score,
         age,
         sex,
         #spec,
         bed_type
         # smoking,
         # ethnicity,
         #segment
  ) %>%
  filter(!is.na(pathway))
  #na.omit()

# save full proportions

# splits based on 6 months from end of validation
model_df_split <- make_splits(x = list(
  analysis = which(model_df$Census_Date <= validation_end - dweeks(weeks_test)),
  assessment = which(model_df$Census_Date > validation_end - dweeks(weeks_test))
),
data = select(model_df, -Census_Date))

model_df <- model_df %>% select(-Census_Date)

model_df_train <- training(model_df_split)
model_df_test <- testing(model_df_split)


# model_df_train %>%
#   pull(pathway) %>%
#   table() %>%
#   proportions() %>%
#   as.list() %>%
#   as_tibble() %>%
#   pivot_longer(cols = everything(),
#                names_to = "IC Pathway",
#                values_to = "Proportion") %>%
#   mutate(Proportion = percent(Proportion)) %>%
#   show_in_excel()


model_df_train %>%
  pull(pathway) %>%
  table() %>%
  proportions() %>%
  as.list() %>%
  unlist() %>%
  saveRDS("data/pathway_prop.RDS")

mod_rec <- recipe(pathway ~ ., data = model_df_split) %>%
  # step_zv() %>%
  # step_nzv(all_predictors()) %>%
  step_impute_mean(all_numeric_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_novel(all_nominal_predictors(), -sex, new_level = "other") %>%
  step_other(all_nominal_predictors(), -sex, threshold = 0.1) %>%
  # step_unknown(all_nominal_predictors()) %>%
  # step_other(all_nominal_predictors(), threshold = 0.05) %>%
  step_dummy(all_nominal_predictors()) #%>%
  # embed::step_lencode_glm(all_nominal_predictors(), outcome = vars(pathway)) %>%
  # themis::step_smote(pathway)


rf_spec <- rand_forest(
  trees = 1000
) %>%
  set_mode("classification") %>%
  set_engine("ranger", importance = "permutation")


rf_wf <- workflow() %>%
  add_recipe(mod_rec) %>%
  add_model(rf_spec)

rf_fit <- last_fit(rf_wf, model_df_split)

rf_fit %>%
  extract_fit_parsnip() %>%
  vip::vi() %>%
  mutate(Variable = recode(Variable 
                           ,age = "Age"
                           ,cambridge_score = "Cambridge Score"
                           ,bed_type_Neuro.MSK = "Bed Type: Neuro-MSK"
                           ,bed_type_Medicine = "Bed Type: Medicine"
                           ,bed_type_Surgery = "Bed Type: Surgery"
                           ,bed_type_other  = "Bed Type: Other"
                           ,sex_Male = "Sex"
                           )) %>%
  mutate(Variable = factor(Variable)) %>%
  mutate(Variable = fct_reorder(Variable, Importance)) %>%
  ggplot(aes(x = Importance, y = Variable)) + 
  geom_col() +
  theme_minimal()

ggsave(last_plot(),
       filename = "validation/calib_pathway_importance.png",
       bg = "white",
       width = 10,
       height = 7.5,
       scale = 0.45)

roc_df <- rf_fit$.predictions[[1]] %>%
  dplyr::select(starts_with(".pred"), truth = pathway, -.pred_class) %>%
  pivot_longer(
    cols = starts_with(".pred"),
    names_to = "class",
    values_to = "prob",
    names_prefix = ".pred_"
  ) %>%
  mutate(truth = factor(truth), class = factor(class)) %>%
  group_by(class) %>%
  nest() %>%
  mutate(data = map2(data,
                     class,
                     ~ mutate(.x, truth = factor(
                       if_else(truth == .y, truth, "X")
                     )))) %>%
  mutate(roc = map(data, ~roc_curve(.x, truth = truth, prob))) %>%
  mutate(auc = map(data, ~roc_auc(.x, truth = truth, prob))) %>%
  dplyr::select(class, roc, auc) %>%
  unnest(cols = c(roc, auc))


(validation_plot_pathway <-
  ggplot(roc_df, aes(
    x = 1 - specificity,
    y = sensitivity,
    col = glue::glue("{class}\nAUC: {round(.estimate, 2)}")
  )) +
  geom_line() +
  ggplot2::geom_abline(lty = 3) +
  ggplot2::coord_equal() +
  ggplot2::theme_minimal() +
  theme(legend.position = "bottom") +
  labs(colour = ""))

ggsave(validation_plot_pathway,
       filename = "validation/validation_plot_pathway.png",
       bg = "white",
       width = 10,
       height = 10,
       scale = 0.45)

# save workflow
final_wf <- rf_fit %>% extract_workflow()
saveRDS(final_wf, "data/rf_wf.RDS")

