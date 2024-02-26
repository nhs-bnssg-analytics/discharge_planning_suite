library(fitdistrplus)
library(tidyverse)
library(tidymodels)

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
  reframe(pathway = ifelse(length(pathway[pathway != "Other"]) > 0, head(pathway[pathway != "Other"], 1), "Other"),
          sex = sex[1],
          age = Person_Age[1],
          spec = Specialty_Code[1],
          bed_type = Bed_Type[1]) %>%
  select(nhs_number,
         sex,
         age,
         pathway,
         #spec, # spec is too multinomial
         bed_type)

# attributes to join

attr_df <-
  RODBC::sqlQuery(
    con,
    "select * from (
select a.*, ROW_NUMBER() over (partition by nhs_number order by attribute_period desc) rn from
[MODELLING_SQL_AREA].[dbo].[New_Cambridge_Score] a) b where b.rn = 1"
  )




# modelling
model_df <- pathway_df %>%
  left_join(select(attr_df, -sex, -age) %>% mutate(nhs_number = as.character(nhs_number)),
            by = join_by(nhs_number == nhs_number)) %>%
  select(pathway,
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

model_df %>%
  pull(pathway) %>%
  table() %>%
  proportions() %>%
  as.list() %>%
  unlist() %>%
  saveRDS("data/pathway_prop.RDS")

model_df_split <- initial_split(model_df, strata = pathway)
model_df_train <- training(model_df_split)
model_df_test <- testing(model_df_split)


mod_rec <- recipe(pathway ~ ., data = model_df_split) %>%
  step_zv() %>%
  step_impute_mean(all_numeric_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_novel(all_nominal_predictors(), new_level = "other") %>%
  step_other(all_nominal_predictors(), threshold = 0.1) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_nzv(all_predictors()) %>%
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

roc_df <- rf_fit$.predictions[[1]] %>%
  select(starts_with(".pred"), truth = pathway, -.pred_class) %>%
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
  select(class, roc, auc) %>%
  unnest(cols = c(roc, auc))


validation_plot_pathway <-
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
  labs(colour = "")

ggsave(validation_plot_pathway,
       filename = "validation/validation_plot_pathway.png",
       bg = "white",
       width = 8,
       height = 8,
       scale = 0.7)

# save workflow
final_wf <- rf_fit %>% extract_workflow()
saveRDS(final_wf, "data/rf_wf.RDS")



# rf_fit$.predictions[[1]] %>% conf_mat(truth = pathway, estimate = .pred_class) %>% 
#   summary()
# 
# rf_fit$.predictions[[1]] %>% conf_mat(truth = pathway, estimate = .pred_class) %>%
#   autoplot()
# # xgb
# 
# xgb_spec <- boost_tree(
#   trees = 1000,
#   tree_depth = tune(), min_n = tune(),
#   loss_reduction = tune(),                     ## first three: model complexity
#   sample_size = tune(), mtry = tune(),         ## randomness
#   learn_rate = tune()                          ## step size
# ) %>%
#   set_engine("xgboost") %>%
#   set_mode("classification")
# 
# xgb_grid <- grid_latin_hypercube(
#   tree_depth(),
#   min_n(),
#   loss_reduction(),
#   sample_size = sample_prop(),
#   finalize(mtry(), model_df_train),
#   learn_rate(),
#   size = 30
# )
# 
# xgb_wf <- workflow() %>%
#   add_recipe(mod_rec) %>%
#   add_model(xgb_spec)
# 
# set.seed(123)
# model_df_folds <- vfold_cv(model_df_train, strata = pathway)
# 
# model_df_folds
# 
# doParallel::registerDoParallel()
# 
# set.seed(234)
# xgb_res <- tune_grid(
#   xgb_wf,
#   resamples = model_df_folds,
#   grid = xgb_grid,
#   control = control_grid(save_pred = TRUE)
# )
# 
# xgb_res %>%
#   collect_metrics() %>%
#   filter(.metric == "roc_auc") %>%
#   select(mean, mtry:sample_size) %>%
#   pivot_longer(mtry:sample_size,
#                values_to = "value",
#                names_to = "parameter"
#   ) %>%
#   ggplot(aes(value, mean, color = parameter)) +
#   geom_point(alpha = 0.8, show.legend = FALSE) +
#   facet_wrap(~parameter, scales = "free_x") +
#   labs(x = NULL, y = "AUC")
# 
# saveRDS(object = xgb_res, "xgb_grid_res.RDS")
# 
# best_auc <- select_best(xgb_res, "roc_auc")
# 
# final_xgb <- finalize_workflow(
#   xgb_wf,
#   best_auc
# )
# 
# final_xgb
# 
# xgb_fit <- last_fit(final_xgb, model_df_split)
# 
# xgb_fit$.predictions[[1]] %>% conf_mat(truth = pathway, estimate = .pred_class) %>% 
#   summary()
# 
# xgb_fit$.predictions[[1]] %>% conf_mat(truth = pathway, estimate = .pred_class) %>%
#   autoplot()
