pathway_weights <- tibble(pathway = c("Other", "P1", "P2", "P3"),
                          wts = importance_weights(c(1, 1, 1, 1))
                          # wts = importance_weights(c(1.035, 0.745, 0.7933839, 0.25))
)
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


fit_weights <- function(weights) {
  
  pathway_weights <- tibble(pathway = c("Other", "P1", "P2", "P3"),
                            wts = importance_weights(weights)
  )
  model_df_weights <-
    left_join(model_df, pathway_weights)
  
  set.seed(1234)
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
  
  rf_cw_fit
  
}



grid <- expand_grid(Other = seq(0.95, 1.1, 0.05),
            P1 = seq(0.9, 0.95, 0.05),
            P2 = seq(1, 1.15, 0.05),
            P3 = seq(0.95, 1.05, 0.5)) 


options(future.globals.maxSize = 16000 * 1024^2)
future::plan(future::multisession, workers = parallel::detectCores() - 12)

fits <- grid %>%
mutate(fit = furrr::future_pmap(list(Other, P1, P2, P3), 
                                ~fit_weights(c(..1, ..2, ..3, ..4)),
                                .options = furrr::furrr_options(
                                  seed = TRUE)))



