library(fitdistrplus)
library(tidyverse)
library(tidymodels)

source("utils/utils.R")
source("code_data_training.R")

validation_end <- ymd("2024-09-01")
validation_start <- ymd("2023-07-01")
weeks_test <- 13
nctr_df <- nctr_df %>% filter(between(census_date, validation_start, validation_end-ddays(1)))

 pathway_df <- nctr_df %>%
  group_by(nhs_number, admission_date, grp) %>%
  mutate(spell_id = cur_group_id()) %>%
  group_by(spell_id) %>%
  mutate(
    der_los = (as.Date(census_date) - as.Date(admission_date))/lubridate::ddays(1),
    # der_date_nctr = as.Date(if_else(any(!der_ctr),min(census_date[!der_ctr]) - lubridate::ddays(1),max(census_date)))) %>%
    der_date_nctr = as.Date(if_else(any(!ctr),min(census_date[!ctr]) ,max(census_date)))) %>% # should it be max(census_date)+1 (if you become NCTR since last census you might have crossed another midnight..?)
  ungroup() %>%
  mutate(der_date_nctr = pmax(admission_date, pmin(der_date_nctr, date_nctr, na.rm = TRUE), na.rm = TRUE)) %>%
  mutate(der_date_nctr = as.Date(der_date_nctr)) %>%
  arrange(census_date) %>%
  group_by(spell_id) %>%
  mutate(pathway = ifelse(!ctr & any(pathway != "Other"), head(pathway[pathway != "Other"], 1), pathway)) %>%
  mutate(discharge_rdy_los = (der_date_nctr - as.Date(admission_date))/lubridate::ddays(1)) %>%
  # take max with zero for some cases where it is -1 due to date nctr == date admission and so los = -1
  mutate(discharge_rdy_los = pmax(discharge_rdy_los, 0)) %>%
  mutate(los = cut(discharge_rdy_los, breaks = c(0, 3, 4, 5, 6, 7, 8, 9, 10, Inf), include.lowest = TRUE)) %>%
  # mutate(pathway = ifelse(any(!der_ctr), pathway[!der_ctr][1], "Other")) %>%
  slice(1) %>%
  ungroup() %>%
  dplyr::select(
    census_date,
    date_nctr = der_date_nctr,
    grp,
    nhs_number,
    sex,
    age,
    los,
    pathway,
    # spec = Specialty_Code, # spec is too multinomial
    bed_type
  ) %>%
  na.omit() %>%
  # remove the last 10 days to avoid any boundary effects from the derivation of pathways
  filter(census_date < max(census_date) - ddays(10))


# remove patients who died


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
  dplyr::select(census_date,
                pathway,
                grp,
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

  # browser()
# Splits are the full data (this is a cheat to see if I can fix pathway props)
model_df_split <- rsample::make_splits(x = list(
  analysis = 1:nrow(model_df),
  assessment = 1:nrow(model_df)
),
data = dplyr::select(model_df, -census_date))

model_df <- model_df %>% dplyr::select(-census_date)

model_df_train <- training(model_df_split)
model_df_test <- testing(model_df_split)

model_df_folds <- vfold_cv(model_df_train, strata = pathway)



props <- model_df_train %>%
  nest(.by = los) %>%
  mutate(props = map(data, \(data) {
    data %>%
      pull(pathway) %>%
      table() %>%
      proportions() %>%
      as.list() %>%
      unlist()
  })) %>%
  mutate(props = set_names(props, los)) %>%
  arrange(los) %>%
  pull(props) #%>%



mod_rec <- recipe(pathway ~ ., data = model_df_train) %>%
  # step_zv() %>%
  # step_nzv(all_predictors()) %>%
  step_impute_mean(all_numeric_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_novel(all_nominal_predictors(), -sex, new_level = "other") %>%
  step_other(all_nominal_predictors(), -sex, threshold = 0.1) 


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






rf_fit <- last_fit(rf_wf, model_df_split)

final_wf <- rf_fit %>% extract_workflow()
list(props = props, fit = rf_fit, fit_rs = rf_fit_rs)
}


fits <- model_df %>%
  nest(.by = grp) %>%
  mutate(fit = map(data, ~fit_rf_model(.x)))

saveRDS(dplyr::select(fits, grp, fit), "data/rf_fit_props_grp.RDS") 

fits <- readRDS("data/rf_fit_props_grp.RDS")

library(gt)

fits %>%
  filter(grp != "nbt") %>%
  mutate(fit = set_names(fit, grp)) %>%
  pull(fit) %>%
  map("props") %>%
  map(enframe) %>%
  map(\(x) unnest_wider(x, value)) %>%
  bind_rows(.id = "grp") %>%
  pivot_longer(cols = -c(grp, name), names_to = "pathway", values_to = "proportion") %>%
  pivot_wider(names_from = name, values_from = proportion) %>%
  gt() %>%
  tab_spanner(label = "Medical length of stay",
              columns = -c(grp, pathway)) %>%
  fmt_number(
    columns = -c(grp, pathway),
    decimals = 3
  ) %>%
  gt::gtsave("materials/pathway_props.html")
  

# ROC 

library(patchwork)

roc_plot <- function(fit, grp) {
  labels <- fit$fit_rs$roc_df %>%
    bind_rows(.id = "fold") %>%
    ungroup() %>%
    dplyr::select(fold, class, .estimate) %>%
    distinct() %>%
    summarise(roc = mean(.estimate),
              u95 = quantile(.estimate, 0.975), 
              l95 = quantile(.estimate, 0.025), 
              .by = class) %>%
    mutate(label = str_c(class, "\n", "AUC: ", round(roc, 2), " [", round(l95, 2), ", ", round(u95, 2), "]")) %>%
    mutate(label = set_names(label, class)) %>%
    pull(label)
  
  fit$fit_rs$roc_df %>%
    bind_rows(.id = "fold") %>%
    ungroup() %>%
    mutate(class = recode(class, !!!labels)) %>%
    ggplot(aes(
      x = 1 - specificity,
      y = sensitivity,
      group = fold
    )) +
    geom_path(alpha = 0.5) +
    ggplot2::geom_abline(lty = 3) +
    # ggplot2::coord_equal() +
    ggplot2::theme_minimal() +
    ggplot2::scale_x_continuous(breaks = c(0, 0.5, 1)) +
    ggplot2::theme(plot.title = element_blank(),
                   panel.background = element_rect(fill = NA, color = "#DDDDDD", linewidth = 1),
      panel.spacing.x = unit(x = 0.75, units = "cm"))+
    labs(#title = grp,
         colour = "",
         y = "Sensitivity",
         x = "1 - Specificity") +
    facet_grid(str_c(grp)~class)
  }

fits %>%
  filter(grp != "nbt") %>%
  mutate(grp = recode(grp, !!!c("bri" = "Bristol Royal\nInfirmary", "weston" = "Weston General\nHospital"))) %>%
  mutate(plot = map2(fit, grp, \(fit, grp) roc_plot(fit, grp))
    ) %>%
  pull(plot) %>%
 wrap_plots(ncol = 1) +
 plot_layout(axes = "collect") #+
 #plot_annotation("Validation 3e")


ggsave(last_plot(),
       filename = "validation/rf_roc_folds.png",
       bg = "white",
       width = 10,
       height = 5.5,
       scale = 0.7)

fits %>%
  filter(grp != "nbt") %>%
  mutate(grp = recode(grp, !!!c("bri" = "Bristol Royal Infirmary", "weston" = "Weston General Hospital"))) %>%
  mutate(vip = map2(fit, grp, \(fit, grp) {
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
                               ,los = "mLOS"
                               ,sex = "Sex"
                               ,sex_Male = "Sex"
      )) %>%
      mutate(Variable = factor(Variable)) %>%
      mutate(Variable = fct_reorder(Variable, Importance)) %>%
      ggplot(aes(x = Importance, y = Variable)) +
      geom_col(fill = "grey40") +
      theme_minimal() +
      labs(title = grp, x = "Permutation importance", y = "")
  })) %>%
  pull(vip) %>%
  patchwork::wrap_plots(axes = "collect") &
  theme(plot.title = element_text(size = 10), axis.title.y = element_blank())#+
  #patchwork::plot_annotation(title = "Calibration 3e")


ggsave(last_plot(),
       filename = "validation/rf_vip.png",
       bg = "white",
       width = 10,
       height = 5,
       scale = 0.55)


fits %>%

filter(grp != "nbt") %>%
  mutate(grp = recode(grp, !!!c("bri" = "Bristol Royal Infirmary", "weston" = "Weston General Hospital"))) %>%
  hoist(fit, "props") %>%
  select(grp, props) %>%
  unnest_wider(props) %>%
  pivot_longer(cols = -grp) %>%
  pivot_wider(names_from = grp, values_from = value) %>%
  rename("IC pathway" = name) %>%
  show_in_excel()
  
# filter(grp != "nbt") %>%
map2(.$fit$fit, .$grp, \(fit, grp) {
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
  dplyr::select(grp, samp, preds) %>%
  unnest(cols = c(samp, preds)) %>%
  reframe(bind = map2(samp, preds, bind_cols), .by = grp) %>%
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
  dplyr::select(grp, out) %>%
  mutate(id = seq_len(n_samp), .by = grp) %>%
  unnest(cols = out) %>%
  pivot_wider(names_from = source, values_from = value) %>%
  mutate(residual = emp - rf) %>%
  summarise(mean_residual = mean(residual),
            u95 = quantile(residual, 0.975),
            l95 = quantile(residual, 0.025), .by = c(grp, pathway)) %>%
  ggplot(aes(x = pathway)) +
  geom_errorbar(aes(ymin = l95, ymax = u95)) +
  geom_point(aes(y = mean_residual)) +
  facet_wrap(vars(grp), ncol = 1)

         