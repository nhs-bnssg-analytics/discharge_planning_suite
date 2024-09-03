

rtruncdist <- function(n, pdist, qdist, range = c(0, Inf)){
  # find truncated ranges of dist
  F_l <- pdist(min(range))
  F_u <- pdist(max(range))
  # draw from uniform between ranges
  u <- runif(n, min = F_l, max = F_u)
  qdist(u)
}


#' Title
#'
#' @inheritParams rlnorm
#' @param range vector of length two, indicating min and max of range of truncated draw
#'
#' @return
#' @export
rlnormt <- function(n, meanlog = 0, sdlog = 1, range = c(0, Inf)){
  # find truncated ranges of dist
  F_l <- plnorm(min(range), meanlog = meanlog, sdlog = sdlog)
  F_u <- plnorm(max(range), meanlog = meanlog, sdlog = sdlog)
  # draw from uniform between ranges
  u <- runif(n, min = F_l, max = F_u)
  qlnorm(u, mean = meanlog, sdlog = sdlog)
}

sample_prop <- function(x, size, ...) {
  sample(names(x), size, replace = TRUE, prob = x)
}


fit_all <- function(x){
  
  fit_ex <- fitdistrplus::fitdist(x,"exp")
  fit_nm <- fitdistrplus::fitdist(x,"norm")
  fit_ln <- fitdistrplus::fitdist(x,"lnorm")
  fit_gm <- fitdistrplus::fitdist(x,"gamma")
  fit_wb <- fitdistrplus::fitdist(x,"weibull")
  
  gof <- fitdistrplus::gofstat(list(fit_ex, 
                      fit_nm,
                      fit_ln,
                      fit_gm,
                      fit_wb),
                 fitnames = c("expo",
                              "normal",
                              "lognormal",
                              "gamma",
                              "weibull"))
  
  gof_df <- data.frame(measure = c("Kolmogorov-Smirnov statistic",
                                   "Cramer-von Mises statistic",
                                   "Anderson-Darling statistic",
                                   "Akaike's Information Criterion",
                                   "Bayesian Information Criterion"),
                       rbind(gof$ks,
                             gof$cvm,
                             gof$ad,
                             gof$aic,
                             gof$bic))
  
  gof_df
}

hours_to_bin_vec <- function(hours, length = 240) {
  # clamp hours between 0 and length
  vec <- rep(1, times = round(max(min(hours, length), 0)))
  length(vec) <- length
  vec <- dplyr::coalesce(vec, 0)
  vec <- setNames(vec, nm = paste0('t_', 1:length))
  vec
}

bin_vec_pad <- function(vec, length = 240) {
  length(vec) <- length
  vec <- dplyr::coalesce(vec, 0)
  vec <- setNames(vec, nm = paste0('t_', 1:length))
  vec
}


get_rownum_from_day_time <- function(datetime){
  day_num <- wday(datetime)
  time_num <- (datetime %>%
                 strftime(format = '%H') %>%
                 as.numeric())+1
  (day_num - 1) * 24 + time_num
}

get_day_prediction <- function(site){
  lambda <- switch(site,
                   nbt = 50,
                   bri = 75,
                   weston = 15)
  
  first_date <- Sys.Date()
  #return a list containing value = the 10-day predictions
  # date = the dates the predictions are for
  list(value = rpois(10, lambda = lambda), 
       date = seq(first_date, first_date + days(9), by = 'day'))
}

get_day_prediction_fcast <- function(df, site){
  df <- filter(df, site == !!site)
  list(value = pluck(df, "fcast"), 
       datetime = pluck(df, "datetime"))
}

slice_wrap <- function(vec, off = 0, len){
  vec[((off + seq_len(len) - 1) %% length(vec)) + 1]
}

mu_fn <- function(mu, sdlog_1, sdlog_2) {
  mu + ((sdlog_1^2)/2 - (sdlog_2^2)/2)
}

# sweep_fn <- function(sweep, hour, los_end) {
#   map2(hour, los_end, function(hour, los_end) {
#     map_lgl(seq_len(sweep), ~ .x >= hour & .x < los_end)
#   })
# } 

# We count patients along sweep line at the end of the hour (i.e. if you join in
# the hour you are counted, if you leave you are not)
sweep_fn <- function(sweep, hour, los_end) {
  map_dbl(seq_len(sweep), ~sum(.x >= hour & .x < los_end))
}

colMeans_safe <- function(x) {
  f = possibly(function() colMeans(x), otherwise = x)
  f()
}


get_sd_from_ci <- function(ci, q = c(0.075, 0.925)) {
  (ci[1] - ci[2])/(qnorm(q[1]) - qnorm(q[2]))
}

smpe_custom <- function(actual, predicted) {
  n <- length(actual)
  sum((predicted - actual) / ((actual + predicted) / 2)) * 100 / n
}

