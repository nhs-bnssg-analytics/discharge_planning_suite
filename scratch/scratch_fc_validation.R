library(forecast)
library(tsibble)
library(fable)
library(fabletools)
library(fable.prophet)


  fc_train_length <- 26 # (train length in weeks)
  
  fcast_days <- 10
  
  
  admissions <- nctr_df %>%
    filter(between(Date_Of_Admission, validation_start, start_date)) %>%
    filter(!is.na(NHS_Number)) %>%
    filter(Organisation_Site_Code %in% c('RVJ01', 'RA701', 'RA301', 'RA7C2')) %>%
    mutate(site = case_when(Organisation_Site_Code == 'RVJ01' ~ 'nbt',
                            Organisation_Site_Code == 'RA701' ~ 'bri',
                            Organisation_Site_Code %in% c('RA301', 'RA7C2') ~ 'weston',
                            TRUE ~ '')) %>%
    group_by(nhs_number = NHS_Number) %>%
    distinct(Date_Of_Admission, .keep_all = TRUE) %>%
    group_by(site , date = as.Date(Date_Of_Admission)) %>%
    count()
  
  
  models <- admissions %>%
    ungroup() %>%
    complete(date, site, fill = list(count = 0)) %>%
    filter(site != "nbt") %>%
    mutate(site = recode(site,
                         "bri" = "Bristol Royal Infirmary",
                         "weston" = "Weston General Hospital"
    )) %>%
    group_by(site) %>%
    arrange(date) %>%
    nest() %>%
    mutate(data = map(data, as_tsibble, index = date)) %>%
    mutate(data_tr = map(data, stretch_tsibble, .init = fc_train_length*7, .step = 35)) %>%
    # pull(data_tr) %>% pluck(1) %>% as_tibble() %>% pull(".id") %>% table()
    mutate(
      # model = map(data, ~model(.x, mdl = prophet(n))),
      model = map(data_tr, ~model(.x, mdl = ARIMA(n))),
      fc = map(model, forecast, h = fcast_days))  %>%
    mutate(acc = map2(fc, data, ~accuracy(.x, data = .y, by = c(".id", ".model")))) %>%
    mutate(mape = map_dbl(acc, ~.x %>% pull(MAPE) %>% mean)) %>%
    mutate(mae = map_dbl(acc, ~.x %>% pull(MAE) %>% mean))
  
  
  fc <- models$fc[[1]] %>%
    nest(.by = .id)
  data <- models$data[[1]] %>%
    rename(n_obs = n) %>%
    ungroup()
  
  
  
  
 plots <- pmap(list(models$fc,
                    models$data,
                    models$site,
                    models$mape),
       \(fc, data, site, mape) nest(fc, .by = .id) %>%
         pull(data) %>% map( ~ full_join(.x, rename(data, n_obs = n)) %>%
                               mutate(max_date = max(date[!is.na(.model)]))  %>%
                               filter(between(date, max_date - dweeks(6), max_date)) %>%
                               mutate(n = map(n, hilo)) %>%
                               unnest(n) %>%
                               mutate(lower = map_dbl(n, ~.x$lower)) %>%
                               mutate(upper = map_dbl(n, ~.x$upper)) %>%
                               ggplot(aes(x = date)) +
                               geom_line(aes(y = n_obs), col = "black") +
                               geom_line(aes(y = .mean), col = "blue") +
                               geom_ribbon(aes(ymin = lower, ymax = upper), fill = "blue", alpha = 0.25) +
                               theme_minimal() +
                               labs(x = "Date", y = glue::glue("Daily admissions, {site} (MAPE: {round(mape,2 )})"))
                             )
       )



 (validation_plot_fc <- plots %>%
   map( ~ patchwork::wrap_plots(
     .x,
     ncol = 1,
     axes = "collect",
     guides = "collect"
   )) %>%
   patchwork::wrap_plots(nrow = 1, axes = "collect"))
 

ggsave(
  validation_plot_fc,
  filename = "./validation/validation_plot_fc.png",
  scale = 0.5,
  width = 16,
  height = 12
)

# 
# plots %>%
#   flatten %>%
#   `[`(1:6) %>%
#  patchwork::wrap_plots(nrow = 5,
#                        axes = "collect",
#                        guides = "collect")