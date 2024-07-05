library(fitdistrplus)
library(tidyverse)
library(forecast)
library(tsibble)
library(fable)
library(fabletools)

df_admit_fcast <- local({
  
# arima training ends at the start of the reporting
fc_train_length <- 26 # (train length in weeks)
fc_end <- report_start - ddays(1) # deduct one day as we must consider arrivals one day zero
fc_start <- fc_end  - dweeks(fc_train_length)

min_adm_date <- nctr_df %>%
  filter(!is.na(NHS_Number)) %>%
  filter(Organisation_Site_Code %in% c('RVJ01', 'RA701', 'RA301', 'RA7C2')) %>%
  group_by(Organisation_Site_Code) %>%
  summarise(date = as.Date(max(Date_Of_Admission))) %>%
  pull(date) %>%
  min()

fcast_days <- ceiling((report_end - min_adm_date)/ddays(1))


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
  mutate(
         model = map(data, ~model(.x, mdl = ARIMA(n))),
         fc = map(model, forecast, h = fcast_days)) %>%
  mutate(mean = map(fc, pluck, ".mean"),
         u_85 = map(fc, ~pluck(.x, "n") %>% quantile(0.925)),
         l_85 = map(fc, ~pluck(.x, "n") %>% quantile(0.075)),
         frame = pmap(list(data, mean), ~tibble(date = seq(max(pluck(..1, "date")) + ddays(1),
                                                          by = "days",
                                                          length.out = fcast_days),
                                               fcast = ..2)),
         frame = pmap(list(frame, u_85, l_85), ~mutate(..1, u_85 = ..2, l_85 = ..3)),
         frame = map2(data, frame, bind_rows),
         frame = map(frame, as_tibble)) %>%
  dplyr::select(site, frame) %>%
  unnest(cols = c(frame)) %>%
  pivot_longer(cols = -c(site, date),
               names_to = "metric",
               values_to = "value") %>%
  filter(!is.na(value)) %>%
  mutate(value = pmax(value, 0)) %>%
  mutate(source = "admit_fcast",
         ctr = "N") %>%
  filter(date >= fc_start,
         date <= report_end ) %>%
  mutate(day = (date - report_start)/ddays(1),
         report_date = run_date) %>%
  mutate(site = recode(site, 'SOUTHMEAD HOSPITAL' = 'nbt', 
                       'BRISTOL ROYAL INFIRMARY' = 'bri', 
                       'WESTON GENERAL HOSPITAL' = 'weston'))


models
})


if(plot_int){
  p <- df_admit_fcast %>%
    pivot_wider(names_from = metric,
                values_from = value) %>%
    ggplot(aes(x = date, y = n)) +
    geom_line() +
    geom_line(aes(y = fcast), col = "blue") +
    # geom_ribbon(aes(ymin = l_80, ymax = u_80), alpha = 0.25) +
    geom_ribbon(aes(ymin = l_85, ymax = u_85), alpha = 0.25) +
    facet_wrap(vars(site), ncol = 1)
  print(p)
  rm(p)
}