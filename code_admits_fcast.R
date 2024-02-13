library(fitdistrplus)
library(tidyverse)
library(forecast)
library(tsibble)
library(fable)
library(fabletools)
library(fable.prophet)

df_admit_fcast <- local({
  
# arima training ends at the start of the reporting
arima_train_length <- 18 # (train length in weeks)
arima_end <- report_start
arima_start <- arima_end  - dweeks(arima_train_length)

min_adm_date <- nctr_df %>%
  filter(!is.na(NHS_Number)) %>%
  filter(Organisation_Site_Code %in% c('RVJ01', 'RA701', 'RA301', 'RA7C2')) %>%
  group_by(Organisation_Site_Code) %>%
  summarise(date = as.Date(max(Date_Of_Admission))) %>%
  pull(date) %>%
  min()

arima_fcast_days <- ceiling((report_end - min_adm_date)/ddays(1))


admissions <- nctr_df %>%
  filter(!is.na(NHS_Number)) %>%
  filter(Organisation_Site_Code %in% c('RVJ01', 'RA701', 'RA301', 'RA7C2')) %>%
  mutate(site = case_when(Organisation_Site_Code == 'RVJ01' ~ 'nbt',
                                            Organisation_Site_Code == 'RA701' ~ 'bri',
                                            Organisation_Site_Code %in% c('RA301', 'RA7C2') ~ 'weston',
                                            TRUE ~ '')) %>%
  group_by(nhs_number = NHS_Number) %>%
  distinct(Date_Of_Admission, .keep_all = TRUE) %>%
  group_by(site , date = as.Date(Date_Of_Admission)) %>%
  count() %>%
  filter(date > ymd("2023-07-01")) # data before this are spurious 
  

models <- admissions %>%
  ungroup() %>%
  complete(date, site, fill = list(count = 0)) %>%
  group_by(site) %>%
  arrange(date) %>%
  nest() %>%
  mutate(data = map(data, as_tsibble, index = date)) %>%
  mutate(model = map(data, ~model(.x, mdl = prophet(n))),
         fc = map(model, forecast, h = n_days))



  mutate(ts = map(data, ~ts(pull(.x, n), frequency = 365)),
         arima = map(ts, ~auto.arima(.x) %>% forecast::forecast(h = arima_fcast_days)),
         mean = map(arima, pluck, "mean"),
         upper = map(arima, ~pluck(.x, "upper") %>%
                       as.data.frame %>% rename(u_80 = `80%`, u_95 = `95%`)),
         lower = map(arima, ~pluck(.x, "lower") %>%
                       as.data.frame %>% rename(l_80 = `80%`, l_95 = `95%`)),
         frame = pmap(list(data, mean), ~tibble(date = seq(max(pluck(..1, "date")) + ddays(1),
                                                               by = "days",
                                                               length.out = arima_fcast_days),
                                                fcast = ..2)),
         frame = pmap(list(frame, upper, lower), bind_cols),
         frame = map2(data, frame, bind_rows)) %>%
  dplyr::select(site, frame) %>%
  unnest(cols = c(frame)) %>%
  pivot_longer(cols = -c(site, date),
               names_to = "metric",
               values_to = "value") %>%
  filter(!is.na(value)) %>%
  mutate(value = pmax(value, 0)) %>%
  mutate(tag = "ae_fcast",
         report_date = run_date) %>%
  filter(date >= arima_start,
         date <= report_end ) %>%
  mutate(site = recode(site, 'SOUTHMEAD HOSPITAL' = 'nbt', 
                       'BRISTOL ROYAL INFIRMARY' = 'bri', 
                       'WESTON GENERAL HOSPITAL' = 'weston'))


# if(save_int) saveRDS(models, "data/df_ae_fcast_sql.RDS")
# models
# })

models
})


if(plot_int){
  p <- df_admit_fcast %>%
    pivot_wider(names_from = metric,
                values_from = value) %>%
    ggplot(aes(x = date, y = n)) +
    geom_line() +
    geom_line(aes(y = fcast), col = "blue") +
    geom_ribbon(aes(ymin = l_80, ymax = u_80), alpha = 0.25) +
    geom_ribbon(aes(ymin = l_95, ymax = u_95), alpha = 0.25) +
    facet_wrap(vars(site), ncol = 1)
  print(p)
  rm(p)
}