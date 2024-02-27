
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
  mutate(dist_partial = map2(dist, fit_parms, function(dist, parms) partial(get(glue::glue("r{dist}")), !!parms)))


d <- vector(mode = "list", length = nrow(fit_dists))
for (i in seq_len(nrow(fit_dists))) {
  d[[i]] <-
    partial(get(glue::glue("r{fit_dists$dist[[i]]}")), !!!fit_dists$fit_parms[[i]])
}

         
         
         
         
         
         
         
         
         
         
         
         
         %>%
  mutate(fit_parms = map(fit, pluck, "estimate"))  %>%
  select(leaf, fit, fit_parms) %>%
  mutate(fit_parms = set_names(fit_parms, leaf))