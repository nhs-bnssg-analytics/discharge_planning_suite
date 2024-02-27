
library(actuar)
library(extraDistr)

dists <- c("exp",
           "norm",
           "lnorm",
           "gamma",
           "weibull")


fit_lnorm <- los_model_df %>%
  dplyr::select(leaf, los) %>%
  group_by(leaf) %>%
  nest() %>%
  mutate(fit = map(data, function(data ) imap(dists, ~fitdist(data$los, .x)))) %>%
  mutate(min_aic = map_dbl(fit, function(group) min(map_dbl(group, ~pluck(.x, "aic"))))) %>%
  #select best based on lowest AIC
  mutate(fit = flatten(pmap(list(fit, min_aic), function(fits, aic) keep(fits, \(x) x$aic == aic)))) %>%
  mutate(dist = map_chr(fit, "distname")) %>%
  mutate(fit_parms = map(fit, "estimate")) %>%
  mutate(dist_partial = map2(dist, fit_parms, ~partial(get(glue::glue("r{.x}")), .y)))


fit_1 <- fit_lnorm$fit[[1]]
min_aic_1 <- fit_lnorm$min_aic[[1]]

keep(fit_1, \(x) x$aic == min_aic_1)


         
         
partial(rnorm, !!!c(mean = 10, sd = 2)) 
         
         
         
         
         
         
         
         
         
         
         
         
         
         %>%
  mutate(fit_parms = map(fit, pluck, "estimate"))  %>%
  select(leaf, fit, fit_parms) %>%
  mutate(fit_parms = set_names(fit_parms, leaf))