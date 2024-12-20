library(fitdistrplus)
library(tidyverse)
library(tidymodels)
library(treesnip)

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


# splits based on 6 months from end of validation
model_df_split <- make_splits(x = list(
  analysis = which(model_df$Census_Date <= validation_end - dweeks(weeks_test)),
  assessment = which(model_df$Census_Date <= validation_end - dweeks(weeks_test))
  # assessment = which(model_df$Census_Date > validation_end - dweeks(weeks_test))
),
data = select(model_df, -Census_Date))

model_df <- model_df %>% select(-Census_Date)

model_df_train <- training(model_df_split)
model_df_test <- testing(model_df_split)

pathway_folds <- vfold_cv(model_df_train, strata = pathway)

metrics <- metric_set(accuracy, sens, spec, mn_log_loss, roc_auc, brier_class, bal_accuracy)
model_control <- control_grid(save_pred = TRUE)


mod_rec <- recipe(pathway ~ ., data = model_df_split) %>%
  step_impute_mean(all_numeric_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_novel(all_nominal_predictors(), -sex, new_level = "other") 

rf_spec <- rand_forest(
  trees = 1000
) %>%
  set_mode("classification") %>%
  set_engine("ranger", importance = "permutation")




eval_fit_prop <- function(fit) {
  bind_rows(
    {
      fit %>%
        collect_predictions() %>%
        count(pathway) %>%
        mutate(source = "empirical")
    },
    {
      fit %>%
        collect_predictions() %>%
        summarise(across(c(matches(".pred_[OP]")), sum, .names = "{str_remove(.col, '.pred_')}")) %>%
        pivot_longer(everything(), names_to = "pathway", values_to = "n") %>%
        mutate(source = "model")
    }
  ) %>%
    pivot_wider(names_from = source, values_from = n) %>%
    rowwise() %>%
    mutate(mape = mape_vec(empirical, model),
           pe = (empirical - model)/empirical,
           msd = msd_vec(empirical, model)) %>%
    ungroup() %>%
    mutate(
      mape_mean = mean(mape)
    )
  
}

# standard RF (to beat)
{
rf_wf <- workflow() %>%
  add_recipe(mod_rec) %>%
  add_model(rf_spec)

rf_fit <- last_fit(rf_wf, model_df_split, metrics = metrics)

rf_fit %>% collect_metrics()


rf_fit %>%
  collect_predictions() %>%
  conf_mat(truth = pathway, .pred_class)

eval_fit_prop(rf_fit)
}

# RF with case_weights
{

# based on data proportions
# pathway_weights <- 
#     model_df_train$pathway %>%
#     table %>%
#     proportions() %>%
#     as_tibble() %>%
#     mutate(wts = importance_weights(1 / n)) %>%
#     select(pathway = '.', wts)

# bespoke  
pathway_weights <- tibble(pathway = c("Other", "P1", "P2", "P3"),
       wts = importance_weights(c(1.035, 0.745, 0.7933839, 0.25)))
  
model_df_weights <- 
 left_join(model_df, pathway_weights)

model_df_split_weights <-  initial_split(model_df_weights, strata = pathway)

mod_rec_weights <- recipe(pathway ~ ., data = model_df_split_weights) %>%
  step_impute_mean(all_numeric_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_novel(all_nominal_predictors(), -sex, new_level = "other") 


rf_cw_wf <- workflow() %>%
  add_recipe(mod_rec_weights) %>%
  add_model(rf_spec) %>%
  add_case_weights(wts)


rf_cw_fit <- last_fit(rf_cw_wf, model_df_split_weights, metrics = metrics)

rf_cw_fit %>% collect_metrics()

eval_fit_prop(rf_cw_fit)

}

# down sampling
{
mod_rec_dsamp <- recipe(pathway ~ ., data = model_df_split) %>%
  step_impute_mean(all_numeric_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_novel(all_nominal_predictors(), -sex, new_level = "other") %>%
  step_other(all_nominal_predictors(), -sex, threshold = 0.1) %>%
  step_dummy(all_nominal_predictors()) %>%
  themis::step_downsample(pathway, under_ratio = 3)

rf_wf_dsamp <- workflow() %>%
  add_recipe(mod_rec_dsamp) %>%
  add_model(rf_spec)

rf_fit_dsamp <- last_fit(rf_wf_dsamp, model_df_split)

bind_rows({
  rf_fit_dsamp %>%
    collect_predictions() %>%
    count(pathway) %>%
    mutate(source = "empirical")
}, {
  rf_fit_dsamp %>%
    collect_predictions() %>%
    summarise(across(c(matches(".pred_[OP]")), sum, .names = "{str_remove(.col, '.pred_')}")) %>%
    pivot_longer(everything(), names_to = "pathway", values_to = "n") %>%
    mutate(source = "model")
}) %>%
  pivot_wider(names_from = source, values_from = n) %>%
  rowwise() %>%
  mutate(mape = mape_vec(empirical, model)) %>%
  ungroup() %>%
  mutate(mape_mean = mean(mape))
}


# knn
{
knn_model <- nearest_neighbor(neighbors = tune(), weight_func = tune(), dist_power = tune()) %>% 
  set_engine("kknn") %>% 
  set_mode("classification")

knn_wf <- workflow() %>%
  add_recipe(mod_rec) %>%
  add_model(knn_model)


knn_grid <- grid_regular(neighbors(range = c(1, 10)),
                         weight_func(),
                         dist_power(),
                         levels = 5)

doParallel::registerDoParallel()
set.seed(2021)

knn_res <- tune_grid(
  knn_wf,
  mod_rec,
  grid = knn_grid,
  control = model_control,
  metrics = metrics,
  resamples = pathway_folds
)

knn_wf_final <- 
  knn_wf %>%
  finalize_workflow(knn_res %>% select_best(metric = "bal_accuracy"))

knn_fit <- last_fit(knn_wf_final, model_df_split)

eval_fit_prop(knn_fit)

}
# catboost


cb_spec <-  
  parsnip::boost_tree(
    mode = "classification",
    trees = 1000,
    min_n = tune(),
    learn_rate = tune(),
    tree_depth = tune(),
  ) %>%
  set_engine("catboost")


cb_wf <- 
  workflow() %>%
  add_recipe(mod_rec) %>%
  add_model(cb_spec)

cb_grid <- grid_regular(min_n(),
                        learn_rate(),
                        tree_depth(),
                        levels = 3)




library(doParallel)
myCluster = makeCluster(6)
registerDoParallel(myCluster)
clusterCall(myCluster, function() library(catboost))

cb_res <- tune_grid(
  cb_wf,
  mod_rec,
  grid = cb_grid,
  control = model_control,
  metrics = metrics,
  resamples = pathway_folds
)


cb_fit <- last_fit(cb_wf, model_df_split, metrics = metrics)

eval_fit_prop(cb_fit)
