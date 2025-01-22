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
# weeks_test <- 13
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
  filter(Organisation_Site_Code %in% c('RVJ01', 'RA701', 'RA301', 'RA7C2')) %>%
  mutate(
    site = case_when(
      Organisation_Site_Code == 'RVJ01' ~ 'nbt',
      Organisation_Site_Code == 'RA701' ~ 'bri',
      Organisation_Site_Code %in% c('RA301', 'RA7C2') ~ 'weston',
      TRUE ~ 'other'
    )) %>%
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
  mutate(discharge_rdy_los = (der_date_nctr - as.Date(Date_Of_Admission))/lubridate::ddays(1)) %>%
  mutate(discharge_rdy_los = pmax(discharge_rdy_los, 0)) %>%
  group_by(nhs_number) %>%
  arrange(Census_Date) %>%
  group_by(nhs_number, Date_Of_Admission) %>%
  # mutate(pathway = ifelse(!der_ctr & any(pathway != "Other"), head(pathway[pathway != "Other"], 1), pathway)) %>%
  mutate(pathway = ifelse(length(pathway[pathway != "Other"]) > 0, head(pathway[pathway != "Other"], 1), "Other")) %>%
  mutate(discharge_rdy_los = min(discharge_rdy_los)) %>%
  # mutate(pathway = ifelse(any(!der_ctr), pathway[!der_ctr][1], "Other")) %>%
  slice(1) %>%
  ungroup() %>%
  dplyr::select(
    Census_Date,
    date_nctr = der_date_nctr,
    site,
    nhs_number,
    sex,
    age = Person_Age,
    pathway,
    los = discharge_rdy_los,
    # spec = Specialty_Code, # spec is too multinomial
    bed_type = Bed_Type) %>%
  na.omit() %>% 
  # remove the last 10 days to avoid any boundary effects from the derivation of pathways
  filter(Census_Date < max(Census_Date) - ddays(10))


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
    dplyr::select(nhs_number = Derived_Pseudo_NHS, date_death = REG_DATE_OF_DEATH)
})

# remove patients who died

pathway_df <- pathway_df %>%
  left_join(mortality_df) %>% 
  filter(is.na(date_death) | date_death > date_nctr) %>%
  dplyr::select(-date_death)

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
                site,
                cambridge_score,
                age,
                sex,
                # spec,
                los,
                bed_type
                # smoking,
                # ethnicity,
                #segment
  ) %>%
  filter(!is.na(pathway))


fit_rf_model <- function(model_df) {

# Splits are the full data (this is a cheat to see if I can fix pathway props)
model_df_split <- rsample::make_splits(x = list(
  analysis = 1:nrow(model_df),
  assessment = 1:nrow(model_df)
),
data = dplyr::select(model_df, -Census_Date))

model_df <- model_df %>% dplyr::select(-Census_Date)

model_df_train <- training(model_df_split)
model_df_test <- testing(model_df_split)

model_df_folds <- vfold_cv(model_df_train, strata = pathway)


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

# props <- model_df_train %>%
#   pull(pathway) %>%
#   table() %>%
#   proportions() %>%
#   as.list() %>%
#   unlist()

props <- model_df_train %>%
  arrange(los) %>%
  mutate(los_cut = cut(los, breaks = c(0, 3, 4, 5, 6, 7,  8, 9, 10, Inf), include.lowest = TRUE)) %>%
  mutate(los_cut = fct_reorder(los_cut, los))%>%
  mutate(los = los_cut) %>%
  nest(.by = los) %>%
  mutate(props = map(data, \(data) {
    data %>%
  pull(pathway) %>%
  table() %>%
  proportions() %>%
  as.list() %>%
  unlist()})) %>%
  mutate(props = set_names(props, los)) %>%
  arrange(los) %>%
  pull(props) #%>%
  # enframe() %>%
  # unnest_wider(value) %>%
  # pivot_longer(-name, names_to = "pathway") %>%
  # ggplot(aes(x = name, y = value, fill = pathway)) +
  # geom_col(position = "stack")

mod_rec <- recipe(pathway ~ ., data = model_df_split) %>%
  # step_zv() %>%
  # step_nzv(all_predictors()) %>%
  add_role(los, new_role = "not for prediction") %>%
  step_impute_mean(all_numeric_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_novel(all_nominal_predictors(), -sex, new_level = "other") #%>%
  # step_other(all_nominal_predictors(), -sex, spec, threshold = 0.1) 


rf_spec <- rand_forest(
  trees = 1000
) %>%
  set_mode("classification") %>%
  set_engine("ranger", importance = "permutation")


rf_wf <- workflow() %>%
  add_recipe(mod_rec) %>%
  add_model(rf_spec)

# fit folds

rf_fit_rs <- rf_wf %>%
  fit_resamples(model_df_folds, control = control_grid(save_pred = TRUE)) %>%
  mutate(roc_df = map( .predictions, \(x) { x %>%
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
    })) 


# roc_df <- rf_fit$.predictions[[1]] %>%
  #   dplyr::select(starts_with(".pred"), truth = pathway, -.pred_class) %>%
  #   pivot_longer(
  #     cols = starts_with(".pred"),
  #     names_to = "class",
  #     values_to = "prob",
  #     names_prefix = ".pred_"
  #   ) %>%
  #   mutate(truth = factor(truth), class = factor(class)) %>%
  #   group_by(class) %>%
  #   nest() %>%
  #   mutate(data = map2(data,
  #                      class,
  #                      ~ mutate(.x, truth = factor(
  #                        if_else(truth == .y, truth, "X")
  #                      )))) %>%
  #   mutate(roc = map(data, ~roc_curve(.x, truth = truth, prob))) %>%
  #   mutate(auc = map(data, ~roc_auc(.x, truth = truth, prob))) %>%
  #   dplyr::select(class, roc, auc) %>%
  #   unnest(cols = c(roc, auc))




rf_fit <- last_fit(rf_wf, model_df_split)

# rf_fit %>%
#   extract_fit_parsnip() %>%
#   vip::vi() %>%
#   mutate(Variable = recode(Variable 
#                            ,age = "Age"
#                            ,cambridge_score = "Cambridge Score"
#                            ,bed_type_Neuro.MSK = "Bed Type: Neuro-MSK"
#                            ,bed_type_Medicine = "Bed Type: Medicine"
#                            ,bed_type_Surgery = "Bed Type: Surgery"
#                            ,bed_type_other  = "Bed Type: Other"
#                            ,sex_Male = "Sex"
#   )) %>%
#   mutate(Variable = factor(Variable)) %>%
#   mutate(Variable = fct_reorder(Variable, Importance)) %>%
#   ggplot(aes(x = Importance, y = Variable)) + 
#   geom_col() +
#   theme_minimal()
# 
# ggsave(last_plot(),
#        filename = "validation/calib_pathway_importance.png",
#        bg = "white",
#        width = 10,
#        height = 7.5,
#        scale = 0.45)
# 
# roc_df <- rf_fit$.predictions[[1]] %>%
#   dplyr::select(starts_with(".pred"), truth = pathway, -.pred_class) %>%
#   pivot_longer(
#     cols = starts_with(".pred"),
#     names_to = "class",
#     values_to = "prob",
#     names_prefix = ".pred_"
#   ) %>%
#   mutate(truth = factor(truth), class = factor(class)) %>%
#   group_by(class) %>%
#   nest() %>%
#   mutate(data = map2(data,
#                      class,
#                      ~ mutate(.x, truth = factor(
#                        if_else(truth == .y, truth, "X")
#                      )))) %>%
#   mutate(roc = map(data, ~roc_curve(.x, truth = truth, prob))) %>%
#   mutate(auc = map(data, ~roc_auc(.x, truth = truth, prob))) %>%
#   dplyr::select(class, roc, auc) %>%
#   unnest(cols = c(roc, auc))
# 
# 
# (validation_plot_pathway <-
#     ggplot(roc_df, aes(
#       x = 1 - specificity,
#       y = sensitivity,
#       col = glue::glue("{class}\nAUC: {round(.estimate, 2)}")
#     )) +
#     geom_line() +
#     ggplot2::geom_abline(lty = 3) +
#     ggplot2::coord_equal() +
#     ggplot2::theme_minimal() +
#     theme(legend.position = "bottom") +
#     labs(colour = ""))
# 
# ggsave(validation_plot_pathway,
#        filename = "validation/validation_plot_pathway.png",
#        bg = "white",
#        width = 10,
#        height = 10,
#        scale = 0.45)

# save workflow
final_wf <- rf_fit %>% extract_workflow()
list(props = props, fit = rf_fit, fit_rs = rf_fit_rs)
}


fits <- model_df %>%
  nest(.by = site) %>%
  mutate(fit = map(data, ~fit_rf_model(.x)))

saveRDS(dplyr::select(fits, site, fit), "data/rf_fit_props_site.RDS") 

# ROC 

library(patchwork)

roc_plot <- function(fit, site) {
  labels <- fit$fit_rs$roc_df %>%
    bind_rows(.id = "fold") %>%
    ungroup() %>%
    select(fold, class, .estimate) %>%
    distinct() %>%
    summarise(roc = mean(.estimate),
              u95 = quantile(.estimate, 0.975), 
              l95 = quantile(.estimate, 0.025), 
              .by = class) %>%
    mutate(label = str_c(class, "\n", "ROC: ", round(roc, 2), " [", round(l95, 2), ", ", round(u95, 2), "]")) %>%
    mutate(label = set_names(label, class)) %>%
    pull(label)
  
  fit$fit_rs$roc_df %>%
    bind_rows(.id = "fold") %>%
    ungroup() %>%
    mutate(class = recode(class, !!!labels))%>%
    ggplot(aes(
      x = 1 - specificity,
      y = sensitivity,
      group = fold
    )) +
    geom_path(alpha = 0.5) +
    ggplot2::geom_abline(lty = 3) +
    ggplot2::coord_equal() +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.spacing.x = unit(x = 0.75, units = "cm"))+
    labs(title = site,
         colour = "",
         y = "Sensitivity",
         x = "1 - Specificity") +
    facet_grid(.~class)
  }

fits %>%
  filter(site != "nbt") %>%
  mutate(site = recode(site, !!!c("bri" = "Bristol Royal Infirmary", "weston" = "Weston General Hospital"))) %>%
  mutate(plot = map2(fit, site, \(fit, site) roc_plot(fit, site))
    ) %>%
  pull(plot) %>%
 wrap_plots(ncol = 1) +
 plot_layout(axes = "collect")


ggsave(last_plot(),
       filename = "validation/rf_roc_folds.png",
       bg = "white",
       width = 12,
       height = 7.5,
       scale = 0.8)

fits %>%
  filter(site != "nbt") %>%
  mutate(site = recode(site, !!!c("bri" = "Bristol Royal Infirmary", "weston" = "Weston General Hospital"))) %>%
  mutate(vip = map2(fit, site, \(fit, site) {
    fit$fit %>%
      extract_fit_parsnip() %>%
      vip::vi() %>%
      mutate(Variable = recode(Variable
                               ,age = "Age"
                               ,bed_type = "Bed type"
                               ,cambridge_score = "Cambridge Score"
                               ,bed_type_Neuro.MSK = "Bed Type: Neuro-MSK"
                               ,bed_type_Medicine = "Bed Type: Medicine"
                               ,bed_type_Surgery = "Bed Type: Surgery"
                               ,bed_type_other  = "Bed Type: Other"
                               ,sex = "Sex"
                               ,sex_Male = "Sex"
      )) %>%
      mutate(Variable = factor(Variable)) %>%
      mutate(Variable = fct_reorder(Variable, Importance)) %>%
      ggplot(aes(x = Importance, y = Variable)) +
      geom_col() +
      theme_minimal() +
      labs(title = site)
  })) %>%
  pull(vip) %>%
  patchwork::wrap_plots(axes = "collect")


ggsave(last_plot(),
       filename = "validation/rf_vip.png",
       bg = "white",
       width = 10,
       height = 5,
       scale = 0.7)


fits %>%

filter(site != "nbt") %>%
  mutate(site = recode(site, !!!c("bri" = "Bristol Royal Infirmary", "weston" = "Weston General Hospital"))) %>%
  hoist(fit, "props") %>%
  select(site, props) %>%
  unnest_wider(props) %>%
  pivot_longer(cols = -site) %>%
  pivot_wider(names_from = site, values_from = value) %>%
  rename("IC pathway" = name) %>%
  show_in_excel()
  
# filter(site != "nbt") %>%
map2(.$fit$fit, .$site, \(fit, site) {
  fit %>%
  extract_fit_parsnip() %>%
    vip::vi() %>%
    mutate(Variable = recode(Variable
                             ,age = "Age"
                             ,bed_type = "Bed type"
                             ,cambridge_score = "Cambridge Score"
                             ,bed_type_Neuro.MSK = "Bed Type: Neuro-MSK"
                             ,bed_type_Medicine = "Bed Type: Medicine"
                             ,bed_type_Surgery = "Bed Type: Surgery"
                             ,bed_type_other  = "Bed Type: Other"
                             ,sex = "Sex"
                             ,sex_Male = "Sex"
    )) %>%
    mutate(Variable = factor(Variable)) %>%
    mutate(Variable = fct_reorder(Variable, Importance)) %>%
    ggplot(aes(x = Importance, y = Variable)) +
    geom_col() +
    theme_minimal()})





# %>%
#   ggplot(aes(
#     x = 1 - specificity,
#     y = sensitivity,
#     group = id, glue::glue("{class}\nAUC: {round(.estimate, 2)}")
#   )) +
#   geom_line() +
#   ggplot2::geom_abline(lty = 3) +
#   ggplot2::coord_equal() +
#   ggplot2::theme_minimal() +
#   theme(legend.position = "bottom") +
#   labs(colour = "") +
#   facet_wrap(vars(class), nrow = 1)

predict(extract_workflow(fits$fit[[2]]$fit), fits$data[[2]], type = "prob") %>% View()

n_samp <- 50
samp_size <- 1000
n_rep = 1e3


fits %>%
  mutate(wf = map(fit, "fit")) %>%
  mutate(wf = map(wf, extract_workflow)) %>%
  mutate(samp = map(data, \(x) map(seq_len(n_samp), ~sample_n(x, 1000)))) %>%
  mutate(preds = map2(samp, wf, \(x, y) map(x, ~predict(y, .x, type = "prob"))))  %>%
  dplyr::select(site, samp, preds) %>%
  unnest(cols = c(samp, preds)) %>%
  reframe(bind = map2(samp, preds, bind_cols), .by = site) %>%
  mutate(out = map(bind, \(x) 
                   {x %>% 
  mutate(pathwaysamp = pmap(list(.pred_Other,
                                  .pred_P1,
                                  .pred_P2,
                                  .pred_P3),
                             ~factor(sample(c("Other", "P1", "P2", "P3"),
                                            n_rep,
                                            prob = c(..1, ..2, ..3, ..4),
                                            replace = TRUE)),
                             levels = c("Other", "P1", "P2", "P3"))) %>%
  dplyr::select(pathway, pathwaysamp) %>%
  mutate(pathwaysamp = map(pathwaysamp, table)) %>%
  unnest_wider(col = pathwaysamp, names_sep = "_rf_") %>%
  mutate(across(-pathway, as.numeric)) %>%
  mutate(across(-pathway, \(x) coalesce(x, 0))) %>%
  summarise(pathway = list(table(pathway)/samp_size), across(-pathway, \(x) sum(x)/(n_rep*samp_size))) %>%
  unnest_wider(col = pathway, names_sep = "_emp_") %>%
  mutate(across(matches("pathway_emp"), as.numeric)) %>%
  pivot_longer(cols = everything(), names_sep = "_", names_to = c("bla", "source", "pathway")) %>%
  dplyr::select(source, pathway, value)})) %>%
  dplyr::select(site, out) %>%
  mutate(id = seq_len(n_samp), .by = site) %>%
  unnest(cols = out) %>%
  pivot_wider(names_from = source, values_from = value) %>%
  mutate(residual = emp - rf) %>%
  summarise(mean_residual = mean(residual),
            u95 = quantile(residual, 0.975),
            l95 = quantile(residual, 0.025), .by = c(site, pathway)) %>%
  ggplot(aes(x = pathway)) +
  geom_errorbar(aes(ymin = l95, ymax = u95)) +
  geom_point(aes(y = mean_residual)) +
  facet_wrap(vars(site), ncol = 1)

         