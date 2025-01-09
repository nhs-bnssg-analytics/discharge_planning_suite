library(fitdistrplus)
library(tidyverse)
library(tidymodels)
source("utils/utils.R")

con <- switch(.Platform$OS.type,
              windows = RODBC::odbcConnect(dsn = "xsw"),
              unix = xswauth::modelling_sql_area()
)

nctr_df <-
  RODBC::sqlQuery(
    con,
    "SELECT
       [Organisation_Code_Provider]
      ,[Organisation_Code_Commissioner]
      ,[Census_Date]
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
      ,[Current_Delay_Code]
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

date_co <- validation_start #as.Date(max_census - lubridate::dmonths(6))

los_df <- nctr_df %>%
  ungroup() %>%
  filter(Census_Date > date_co, Date_Of_Admission > date_co) %>%
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
  arrange(Census_Date) %>%
  group_by(spell_id) %>%
  # (DEPRECATED) minus one as you mLOS is less than the time observed across the census snapshots (i.e. this is the left censor value)
  mutate(discharge_rdy_los = (der_date_nctr - as.Date(Date_Of_Admission))/lubridate::ddays(1)) %>%
  # take max with zero for some cases where it is -1 due to date nctr == date admission and so los = -1
  mutate(discharge_rdy_los = pmax(discharge_rdy_los, 0)) %>%
  # take first discharge ready value
  group_by(NHS_Number, Date_Of_Admission) %>%
  arrange(discharge_rdy_los) %>% 
  slice(1) %>%
  mutate(day_of_admission = weekdays(Date_Of_Admission)) %>%
  ungroup() %>%
  # filter(discharge_rdy_los >= 0) %>%
  filter(Organisation_Site_Code %in% c('RVJ01', 'RA701', 'RA301', 'RA7C2')) %>%
  mutate(Organisation_Site_Code = case_when(Organisation_Site_Code == 'RVJ01' ~ 'nbt',
                                            Organisation_Site_Code == 'RA701' ~ 'bri',
                                            Organisation_Site_Code %in% c('RA301', 'RA7C2') ~ 'weston',
                                            TRUE ~ '')) %>%
  mutate(Organisation_Site_Code = factor(Organisation_Site_Code)) %>%
  filter(Organisation_Site_Code != "nbt") %>%
  dplyr::select(
    Census_Date,
    nhs_number = nhs_number,
    admission_date = Date_Of_Admission,
    date_nctr = Date_NCTR,
    der_date_nctr,
    site = Organisation_Site_Code,
    day_of_admission,
    sex,
    age = Person_Age,
    spec = Specialty_Code,
    bed_type = Bed_Type,
    los = discharge_rdy_los
  )  #%>%
# # filter outlier LOS
#filter(los < 50) # higher than Q(.99)

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

los_df <- los_df %>%
  left_join(mortality_df) %>% 
  filter(is.na(date_death) | date_death > der_date_nctr) %>%
  select(-date_death, admission_date)

# ECDF

los_df %>%
  mutate(site = recode(site,
                       "bri" = "Bristol Royal Infirmary",
                       "weston" = "Weston General Hospital"
                       )) %>%
  ggplot() +
  stat_ecdf(aes(x = los+1), geom = "step") +
  scale_x_log10(guide = "axis_logticks") +
  scale_y_continuous(breaks = seq(0, 1, 0.1)) +
  theme_minimal() +
  theme(axis.ticks = element_line()) +
  labs(y = "Empirical Cumulative Distribution Function",
       x = "mLOS (Midnights Crossed)") +
  facet_wrap(vars(site), nrow = 1)


ggsave(last_plot(),
       filename = "./validation/los_ecdf.png",
       bg = "white",
       width = 10,
       height = 7.5,
       scale = 0.6)


saveRDS(los_df, "data/los_df.RDS")

#(DEPRECATED) This was the old test/train split 
# los_testing <- los_df %>%
#   filter(Census_Date > validation_end - dweeks(weeks_test)) %>%
#   select(-Census_Date)
# 
# los_train <- los_df %>%
#   filter(Census_Date <= validation_end - dweeks(weeks_test)) %>%
#   select(-Census_Date)

# Now use all data for training and test
los_testing <- los_df %>%
  filter(between(Census_Date, validation_start, validation_end)) %>%
  select(-Census_Date)

los_train <- los_df %>%
  filter(between(Census_Date, validation_start, validation_end)) %>%
  select(-Census_Date)

# attributes to join

# attr_df <-
#   RODBC::sqlQuery(
#     con,
#     "select * from (
# select a.*, ROW_NUMBER() over (partition by nhs_number order by attribute_period desc) rn from
# [MODELLING_SQL_AREA].[dbo].[New_Cambridge_Score] a) b where b.rn = 1"
#   )
# 
# saveRDS(attr_df, "data/attr_df.RDS")
attr_df <- readRDS("data/attr_df.RDS")

# modelling
model_df_train <- los_train %>%
  full_join(dplyr::select(attr_df, -sex, -age) %>% mutate(nhs_number = as.character(nhs_number)),
            by = join_by(nhs_number == nhs_number)) %>%
  # na.omit() %>%
  dplyr::select(
         # nhs_number,
         los,
         site,
         # day_of_admission,
         cambridge_score,
         age,
         sex,
         #spec, # spec has too many levels, some of which don't get seen enough to reliably create a model pipeline
         bed_type
         # smoking,
         # ethnicity,
         #segment
  ) %>%
  filter(sex != "Unknown",
         !is.na(los)) # remove this as only 1 case


model_df_test <- los_testing %>%
  full_join(dplyr::select(attr_df, -sex, -age) %>% mutate(nhs_number = as.character(nhs_number)),
            by = join_by(nhs_number == nhs_number)) %>%
  # na.omit() %>%
  dplyr::select(
                #nhs_number,
                los,
                site,
                # day_of_admission,
                cambridge_score,
                age,
                sex,
                #spec, # spec has too many levels, some of which don't get seen enough to reliably create a model pipeline
                bed_type
                # smoking,
                # ethnicity,
                #segment
  ) %>%
  filter(sex != "Unknown",
         !is.na(los)) 

set.seed(234)
los_folds <- vfold_cv(model_df_train, strata = los)
los_folds


tree_spec <- decision_tree(
  cost_complexity = tune(),
  tree_depth = tune(),
  min_n = tune()
) %>%
  set_engine("rpart", model=TRUE) %>%
  set_mode("regression")


tree_grid <- grid_regular(cost_complexity(range = c(-6, -1), trans = log10_trans()),
                          tree_depth(range = c(2, 7)),
                          min_n(range = c(1500, 3000)),
                          levels = 15)


tree_rec <- recipe(los ~ ., data = model_df_train)  %>%
  update_role(site, new_role = "site id") %>%  
  step_novel(all_nominal_predictors(), new_level = "other") %>%
  step_other(all_nominal_predictors(), threshold = 0.1)


tree_wf <- workflow() %>%
  add_model(tree_spec) %>%
  add_recipe(tree_rec)


doParallel::registerDoParallel()

tree_metrics <-  metric_set(
  rmse,
  rsq,
  mae#,
  #mape
)

set.seed(345)
tree_rs <- tune_grid(
  tree_wf,
  resamples = los_folds,
  grid = tree_grid,
  metrics = tree_metrics
)

autoplot(tree_rs) +
  theme_minimal(base_family = "IBMPlexSans") +
  # scale_x_continuous(transform = "log10", labels = label_math()) +
  scale_x_continuous(transform = "log10", labels = trans_format("log10", math_format(10^.x))) +
  scale_color_viridis_d(begin = 0.1, end = 0.8)  +
  labs(col = "Min n")



labeller_tree_depth <- function(string, prefix = "Tree-depth: ") paste0(prefix, string)
labeller_metric <- c(mae = "Mean Absolute Error",
                     rmse = "Root Mean Squared Error",
                     rsq = "R-squared")

tree_rs %>%
  collect_metrics() %>%
  ggplot(aes(x = cost_complexity, y = mean, col = factor(min_n))) +
  geom_line() +
  geom_point() +
  facet_grid(.metric ~ tree_depth,
             scales = "free_y",
             labeller = labeller(
               # tree_depth = as_labeller(labeller_tree_depth),
               .metric = as_labeller(labeller_metric)
               )
             ) +
  theme_minimal() +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "Accuracy measure", breaks = NULL, labels = NULL)) +
  scale_x_continuous(transform = "log10", labels = trans_format("log10", math_format(10^.x)), sec.axis = sec_axis(~ ., name = "Tree-depth", breaks = NULL, labels = NULL)) +
  scale_color_viridis_d(begin = 0.1, end = 0.8)  +
  # theme(legend.position = "bottom") +
  labs(col = "Min n",
       x = "Cost-Complextiy Parameter",
       y = "")


ggsave(last_plot(),
       filename = "./validation/los_calib_hyperparam.png",
       bg = "white",
       width = 15,
       height = 10,
       scale = 0.6)

# collect_metrics(tree_rs) %>% View()

tuned_wf<- finalize_workflow(tree_wf, select_by_pct_loss(tree_rs, metric = "rmse", limit = 0.25, tree_depth))

tuned_wf

# cv fit
tree_fit_cv <- 
  fit_resamples(tuned_wf, los_folds, metrics = tree_metrics,  control = control_grid(save_pred = TRUE))

tree_fit_cv %>%
  pull(.metrics) %>%
  bind_rows() %>%
  summarise(
    mean = mean(.estimate),
    u95 = quantile(.estimate, 0.975),
    l95 = quantile(.estimate, 0.025),
    .by = .metric
  ) %>%
  
  ggplot(aes(
    y = mean,
    ymin = l95,
    ymax = u95,
    x = NA
  )) +
  geom_point() +
  geom_errorbar() +
  ggh4x::facet_grid2(
    . ~ .metric,
    scales = "free",
    independent = "y",
    labeller = labeller(# tree_depth = as_labeller(labeller_tree_depth),
      .metric = as_labeller(labeller_metric))
  ) +
  labs(y = "", x = "") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        panel.grid.major.x = element_blank())

ggsave(last_plot(),
       filename = "./validation/dec_tree_cv_metrics.png",
       bg = "white",
       width = 10,
       height = 5,
       scale = 0.6)



# final fit
tree_fit <- fit(tuned_wf, model_df_train) 

tree <- extract_fit_engine(tree_fit)
# 
node.fun1 <- function(x, labs, digits, varlen)
{
  paste(
    ifelse(x$frame$var == "<leaf>", paste("Leaf ID:", rownames(x$frame), "\n"), ""),
    "Mean mLOS",
    round(x$frame$yval, 1),
    "\n",
    "n = ",
    scales::number(x$frame$n, big.mark = ",")
    )
}

png(filename = "materials/tree_plot.png",
    width = 1800,
    height = 1200,
    res = 275,
    bg = "white"
    )
rpart.plot::rpart.plot(tree, fallen.leaves = FALSE, type = 2, extra = 101, node.fun = node.fun1)
dev.off()


# append leaf number onto original data:

los_model_df <- model_df_train %>%
  bake(extract_recipe(tree_fit), .) %>%
  mutate(leaf = treeClust::rpart.predict.leaves(tree, .))

ggplot(los_model_df, aes(x = los)) +
  geom_histogram() +
  stat_summary(aes(x = 0, y = los, xintercept = stat(y), group = leaf), 
               fun = median, geom = "vline", col = "blue") +
  stat_summary(aes(x = 0, y = los, xintercept = stat(y), group = leaf), 
               fun = mean, geom = "vline", col = "red") +
  facet_wrap(vars(leaf), scales = "free")


# fit los dists on the data at each leaf

dists <- c(
  "exp"
  ,"lnorm"
  ,"gamma"
  ,"weibull"
)

fitdistcens_safe <- safely(fitdistcens)

# fit_dists <- los_model_df %>%
#   dplyr::select(leaf, left = los) %>%
#   mutate(right = left + 2) %>%
#   group_by(leaf) %>%
#   nest() %>%
#   mutate(data = map(data, as.data.frame)) %>%
#   # fit dist on censored data safely
#   mutate(fit = map(data, function(data) imap(dists, ~fitdistcens_safe(censdata = data, distr = .x)))) %>%
#   # throw out any dists that failed to fit with defaults
#   mutate(fit = map(fit, ~keep(.x, \(x) is.null(x$error)))) %>%
#   # pluck results
#   mutate(fit = map(fit, ~map(.x, "result"))) %>%
#   mutate(min_aic = map_dbl(fit, function(group) min(map_dbl(group, ~pluck(.x, "aic"))))) %>%
#   #select best based on lowest AIC
#   mutate(fit = flatten(pmap(list(fit, min_aic), function(fits, aic) keep(fits, \(x) x$aic == aic)))) %>%
#   mutate(dist = map_chr(fit, "distname")) %>%
#   mutate(fit_parms = map(fit, "estimate")) 

# fit_dists_full <- 
#   
  
  
fit_dists <- los_model_df %>%
  dplyr::select(site, leaf, los) %>%
  mutate(site = fct_drop(site)) %>%
  bind_rows(mutate(., leaf = -1)) %>%
  group_by(leaf) %>%
  nest() %>%
  mutate(data = map(data, ~mutate(.x, los = set_names(los, site)))) %>%
  mutate(los = map(data, pull, los)) %>%
  select(leaf, los)



  # mutate(data = map(data, as.data.frame)) %>%
  #   mutate(max_los = map_dbl(data, ~max(.x$los))) %>%
  #   mutate(ddist = map(data, ~partial(EnvStats::demp, discrete = TRUE, obs = c(.x$los)))) %>%
  #   mutate(pdist = map(data, ~partial(EnvStats::pemp, discrete = TRUE, obs = c(.x$los)))) %>%
  #   mutate(qdist = map(data, ~partial(EnvStats::qemp, discrete = TRUE, obs = c(.x$los)))) %>%
  #   mutate(rdist = map(data, ~partial(EnvStats::remp, obs = c(.x$los, 1e3)))) %>%
  #   mutate(tdist = map2(pdist, qdist, ~partial(rtruncdist, pdist = .x, qdist = .y)))


saveRDS(fit_dists, "data/fit_dists.RDS")
saveRDS(tree_fit, "data/los_wf.RDS")
cat("LOS model outputs written", fill = TRUE)

# VALIDATION code
# output table
fit_dists_full %>%
  filter(leaf != -1) %>%
  ungroup() %>%
  select(leaf, dist, fit_parms, aic = aic) %>%
  mutate(aic = round(aic, 1),
         fit_parms = map_chr(fit_parms, ~paste0(paste(names(.x), "=", round(.x, 2)), collapse = ", ")),
         rule = rpart.plot::rpart.rules(tree) %>%
           as.data.frame() %>%
           janitor::clean_names() %>%
           select(-y) %>%
           reduce(paste) %>%
           stringr::str_squish()) %>%
  show_in_excel()

# validation on test data


cdf_fn <- function(dist, x, parms) {
  fn <- dist_ptl_gen(dist, parms, "d")
  out <- cumsum(fn(x, !!!parms))
  (out - min(out)) / (max(out) - min(out))
}

validation_df <- los_testing  %>%
  full_join(dplyr::select(attr_df, -sex, -age) %>% mutate(nhs_number = as.character(nhs_number)),
            by = join_by(nhs_number == nhs_number)) %>%
  # na.omit() %>%
  dplyr::select(los,
                site,
                # day_of_admission,
                cambridge_score,
                age,
                sex,
                #spec, # spec has too many levels, some of which don't get seen enough to reliably create a model pipeline
                bed_type
                # smoking,
                # ethnicity,
                #segment
  ) %>%
  mutate(los = los + 1) %>%
  filter(sex != "Unknown",
         !is.na(los)) %>%
  bake(extract_recipe(tree_fit), .) %>%
  mutate(leaf = treeClust::rpart.predict.leaves(tree, .)) %>%
  group_by(leaf) %>%
  nest() %>%
  left_join(select(fit_dists, -data, -aic) %>% group_by(leaf) %>% slice(1)) %>%
  mutate(ks_test = pmap(list(data, pdist), ~ks.test(..1$los, ..2) %>% tidy())) %>%
  mutate(ad_test = pmap(list(data, pdist), ~DescTools::AndersonDarlingTest(..1$los, null = ..2))) %>%
  mutate(cdf_plot = pmap(
    list(data, dist, fit_parms),
    ~
      ggplot(..1, aes(x = los)) +
      geom_function(
        geom = "step",
        col = "black",
        fun = function(x) cdf_fn(x, dist = ..2, parms = ..3)#,
        #args = list(.x$fit_parms[[1]])
      ) +
      stat_ecdf(geom = "step") +
      labs(title = "CDF plot", x = "LOS", y = "CDF")+ theme_minimal())
  ) %>%
  mutate(qq_plot = pmap(
    list(data, dist, fit_parms),
    ~
      ggplot(..1, aes(sample = los)) +
      qqplotr::stat_qq_band(distribution = ..2, alpha = 0.5,
                            dparams = ..3) +
      qqplotr::stat_qq_line(distribution = ..2, col = "black",
                            dparams = ..3) +
      qqplotr::stat_qq_point(distribution = ..2,
                             dparams = ..3) +
      labs(title = "Q-Q plot", x = "Theoretical quantiles", y = "Empirical quantiles") + theme_minimal()
  )) %>%
  mutate(pp_plot = pmap(
    list(data, dist, fit_parms),
    ~
      ggplot(..1, aes(sample = los)) +
      qqplotr::stat_pp_band(distribution = ..2, alpha = 0.5,
                            dparams = ..3) +
      qqplotr::stat_pp_line(distribution = ..2, col = "black",
                            dparams = ..3) +
      qqplotr::stat_pp_point(distribution = ..2,
                             dparams = ..3) +
      labs(title = "P-P plot", x = "Theoretical\nprobabilities", y = "Empirical probabilities") + theme_minimal()
  ))

# in order to created grid of plot duets (one CDF & QQ for each LOS leaf
# partition) NOTE: for some reason I have to use cowplot to make a duet with a
# border, which can then be wrapped using patchwork

plots <- pmap(list(validation_df$cdf_plot, validation_df$pp_plot, validation_df$leaf),
              ~cowplot::plot_grid(..1, ..2, labels = paste(..3), hjust = -.2)
              #+ theme(plot.background = element_rect(fill = NA, colour = 'black', size = 1))
              )
(validation_plot_los <- patchwork::wrap_plots(plots))
# 
ggsave(validation_plot_los,
       filename = "./validation/validation_plot_los.png",
       width = 20,
       height = 10,
       scale = 0.7)

# validation_df_tot <- los_test %>%
#   bake(extract_recipe(tree_fit), .) %>%
#   mutate(leaf = -1) %>%
#   group_by(leaf) %>%
#   nest() %>%
#   left_join(select(fit_dists, -data, -min_aic)) %>%
#   mutate(ks_test = pmap(list(data, pdist), ~ks.test(..1$los, ..2) %>% tidy())) %>%
#   mutate(ad_test = pmap(list(data, pdist), ~DescTools::AndersonDarlingTest(..1$los, null = ..2))) %>%
#   mutate(cdf_plot = pmap(
#     list(data, dist, fit_parms),
#     ~
#       ggplot(..1, aes(x = los)) +
#       geom_function(
#         geom = "step",
#         col = "black",
#         fun = function(x) cdf_fn(x, dist = ..2, parms = ..3)#,
#         #args = list(.x$fit_parms[[1]])
#       ) +
#       stat_ecdf(geom = "step") +
#       labs(title = "CDF plot", x = "LOS", y = "CDF")+ theme_minimal()) 
#   ) %>%
#   mutate(qq_plot = pmap(
#     list(data, dist, fit_parms),
#     ~
#       ggplot(..1, aes(sample = los)) +
#       qqplotr::stat_qq_band(distribution = ..2, alpha = 0.5,
#                             dparams = ..3) +
#       qqplotr::stat_qq_line(distribution = ..2, col = "black",
#                             dparams = ..3) +
#       qqplotr::stat_qq_point(distribution = ..2,
#                              dparams = ..3) + 
#       labs(title = "Q-Q plot", x = "Theoretical quantiles", y = "Empirical quantiles") + theme_minimal()
#   )) %>%
#   mutate(pp_plot = pmap(
#     list(data, dist, fit_parms),
#     ~
#       ggplot(..1, aes(sample = los)) +
#       qqplotr::stat_pp_band(distribution = ..2, alpha = 0.5,
#                             dparams = ..3) +
#       qqplotr::stat_pp_line(distribution = ..2, col = "black",
#                             dparams = ..3) +
#       qqplotr::stat_pp_point(distribution = ..2,
#                              dparams = ..3) + 
#       labs(title = "P-P plot", x = "Theoretical probabilities", y = "Empirical probabilities") + theme_minimal()
#   )) 
# 
# (validation_plot_los_tot <- cowplot::plot_grid(validation_df_tot$cdf_plot[[1]], validation_df_tot$pp_plot[[1]]))
# 
# ggsave(validation_plot_los_tot,
#        filename = "./validation/validation_plot_los_tot.png",
#        width = 14,
#        bg = "white",
#        height = 7,
#        scale = 0.6)
