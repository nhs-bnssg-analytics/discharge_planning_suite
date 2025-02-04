library(forecast)
library(tsibble)
library(fable)
library(fabletools)
library(fable.prophet)


  fc_train_length <- 10 # (train length in weeks)
  
  fcast_days <- 10
  
  
  admissions <- nctr_df %>%
    filter(between(Date_Of_Admission, validation_start, validation_end)) %>%
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
    ungroup() %>%
    filter(date != min(date), .by = site)
  
  
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
    mutate(data_flt = map(data, ~filter(.x, date < max(date)-ddays(fcast_days)))) %>%
    mutate(data_tr = map(data_flt, slide_tsibble, .size = fc_train_length*7, .step = 10)) %>%
    mutate(
      model = map(data_tr, ~model(.x, mdl = ARIMA((n)))),
      fc = map(model, forecast, h = fcast_days)
      ) %>%
    # mutate(acc = map2(fc, data, ~accuracy(.x, data = .y, by = c(".id", ".model")))) %>%
    mutate(acc = map2(fc, data, \(x, y) fabletools::accuracy(x, y, by = c(".id", ".model")))) %>%
    mutate(mape = map_dbl(acc, ~.x %>% pull(MAPE) %>% mean)) %>%
    mutate(mape_uq = map_dbl(acc, ~.x %>% pull(MAPE) %>% quantile(0.975))) %>%
    mutate(mape_lq = map_dbl(acc, ~.x %>% pull(MAPE) %>% quantile(0.025))) 
    
    
    # mutate(mae = map_dbl(acc, ~.x %>% pull(MAE) %>% mean)) %>%
    # mutate(me = map_dbl(acc, ~.x %>% pull(ME) %>% mean))
  
  models
  

    
  
  fc <- models$fc[[1]] %>%
    nest(.by = .id) %>%
    pull(data) %>%
    `[[`(1)
  data <- models$data[[1]] %>%
    rename(n_obs = n) %>%
    ungroup()
  
  
  
  
 
  
  plots <- pmap(list(models$fc,
                    models$data,
                    models$site,
                    models$mape,
                    models$mape_uq,
                    models$mape_lq
                    ),
       \(fc, data, site, mape, mape_uq, mape_lq) nest(fc, .by = .id) %>%
         pull(data) %>% map( \(x) {
                               plot_data <- full_join(x, rename(data, n_obs = n)) %>%
                               mutate(max_date = max(date[!is.na(.model)]))  %>%
                               filter(between(date, max_date - dweeks(10), max_date)) %>%
                               mutate(n = map(n, hilo)) %>%
                               unnest(n) %>%
                               mutate(lower = map_dbl(n, ~.x$lower)) %>%
                               mutate(upper = map_dbl(n, ~.x$upper)) 
                               
                               ggplot(plot_data, aes(x = date)) +
                               geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.25) +
                               geom_line(aes(y = n_obs), col = "black") +
                               geom_line(data = filter(plot_data, !is.na(.mean)),
                                          aes(x = date, y = .mean), col = "blue") +
                               scale_x_date(breaks = "15 days", labels=date_format("%d %b\n%Y"),
                                            sec.axis = sec_axis(~ ., name = glue::glue("{site}, MAPE: {round(mape,1)} [{round(mape_lq,1)}, {round(mape_uq,1)}]"), breaks = NULL, labels = NULL)) +
                               theme_minimal() +
                               labs(x = "Date", y = glue::glue("Daily admissions"))
                             
       }))


set.seed(234) # set seed to have same sample of plots
 (validation_plot_fc <- plots %>%
   map( ~ patchwork::wrap_plots(
     sample(.x, 5),
     ncol = 1,
     axes = "collect",
     guides = "collect"
   )) %>%
   patchwork::wrap_plots(nrow = 1, axes = "collect") + 
     patchwork::plot_annotation(title = "Validation 4d"))
 

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