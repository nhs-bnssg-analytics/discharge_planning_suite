library(tidyverse)
library(scales)


plot_custom <- function(df_predicted, df_actual, ribbon_df, vline_1, vline_2,
                        # runtime_1, runtime_2,
                        size_line = 1,  size_dashed_line = 1){
  format_time_custom <- function(date){
    format(date, "%H:%M %d %b")
  }
  
  plot_vline <- function(vline, color = 'blue', alpha = 0.5, size = 1){
    geom_vline(aes(xintercept = as.numeric(vline),
                   text = glue::glue('Prediction time start : {format_time_custom(vline)}')),
               linetype="dashed", color = color, alpha = alpha, size = size)
  }
  
  plot_mean <- function(df, alpha = 1){
    geom_line(data = df,
              aes(x = date, y = value,
                  text = glue::glue('Value : {round(value,2)}
                                      Date : {format_time_custom(date)}'),
                  group=1
              ),
              color = "#00BFC4", alpha = alpha, size = size_line)
  }
  
  plot_ribbon_95 <- function(df, alpha){
    geom_ribbon(data = df,
                aes(x = date, ymin = lower_95, ymax = upper_95,
                    text = glue::glue('95% confidence = [{round(lower_95,2)}, {round(upper_95,2)}]
                                        Date : {format_time_custom(date)}'),
                    group=1
                ),
                alpha = alpha)
  }
  
  plot_ribbon_80 <- function(df, alpha){
    geom_ribbon(data = df,
                aes(x = date, ymin = lower_80, ymax = upper_80,
                    text = glue::glue('80% confidence = [{round(lower_80,2)}, {round(upper_80,2)}]
                                        Date : {format_time_custom(date)}'),
                    group=1
                ),
                alpha = alpha)
  }
  
  suppressWarnings({
    ggplot() +
      plot_vline(vline_1) +
      plot_vline(vline_2) +
      plot_ribbon_95(ribbon_df %>%
                       head(24), 
                     alpha = 0.15) + 
      plot_ribbon_80(ribbon_df %>%
                       head(24), 
                     alpha = 0.15) +
      plot_ribbon_95(ribbon_df %>%
                       tail(24), 
                     alpha = 0.05) + 
      plot_ribbon_80(ribbon_df %>%
                       tail(24), 
                     alpha = 0.05) +  
      plot_mean(df_predicted %>%
                  tail(24),  alpha = 0.4) + 
      plot_mean(df_predicted %>%
                  head(24)) + 
      
      geom_line(data = df_actual, 
                aes(x = date, y = value,
                    text = glue::glue('Actual value : {value}
                                      Date : {format_time_custom(date)}'),
                    group=1
                ),
                color = "#F8766D", size = size_line)+
      
      scale_x_datetime(labels = date_format('%H:%M \n %d %b'), 
                       breaks = c(vline_1 - hours(24), vline_1), 
                       # date_minor_breaks = '12 hours'
                       # minor_breaks = c(vline_1 - hours(36),
                       #                  vline_1 - hours(24),
                       #                  vline_1 - hours(12),
                       #                  vline_1,
                       #                  vline_1 + hours(12))
                       minor_breaks = seq(vline_1 - hours(48), vline_1 + hours(48), by = '6 hours')
                       )+
      labs(x = '', y = '') +
      theme_bw()+ 
      theme(legend.position = 'none')
    })
}

get_plot_pars <- function(var_current, df_input, ribbon_input){ 
  df_predicted <- df_input %>% 
    filter(variable == var_current) %>%
    filter(field == 'predicted') %>%
    arrange(desc(date))
  
  df_actual <- df_input %>% 
    filter(variable == var_current) %>%
    filter(field == 'actual') %>%
    arrange(desc(date))
  
  ribbon <- ribbon_input %>% 
    filter(variable == var_current) %>%
    arrange(desc(date))
  
  split_string <- var_current %>%
    stringr::str_split(' - ') %>%
    unlist()
  
  site <- split_string[1]
  
  metric_name <- split_string[2]
  
  vline_2 <- ribbon$date %>% min()
  vline_1 <- vline_2 + hours(24)
  
  # runtime
  
  list(df_predicted = df_predicted, 
       df_actual = df_actual,
       ribbon = ribbon,
       site = site, 
       metric_name = metric_name, 
       vline_1 = vline_1, 
       vline_2 = vline_2
       # , 
       # runtime_1 = runtime_1, 
       # runtime_2 = runtime_2
  )
}

plot_los_dist <- function(params, site){
  funs <- imap(params, ~function(x) dlnorm(x, meanlog = mean(.x[['meanlog']], na.rm = TRUE),
                                           sdlog = mean(.x[['sdlog']], na.rm = TRUE)))
  
  x_max <- 500
  x <- seq(0, x_max, 0.1)
  
  site <- switch(site, 
    "Bristol Royal Infirmary - UHBW" = "bri",
    "Soutmead Hospital - NBT" = "nbt",
    "Weston General Hospital - UHBW" = "weston"
  )
  
  tibble(x = list(x)) %>%
    mutate(y = map(x, function(x) imap(funs, ~.x(x)))) %>%
    unnest_longer(col = c(y), indices_to = "site") %>%
    mutate(mean = map_dbl(site, ~exp(mean(params[[.x]][['meanlog']], na.rm = TRUE) + ((mean(params[[.x]][['sdlog']], na.rm = TRUE))^2/2)))) %>%
    mutate(median = map_dbl(site, ~exp(mean(params[[.x]][['meanlog']], na.rm = TRUE)))) %>%
    unnest(cols = c(x, y)) %>%
    mutate(y_min = 0,
           day = x/24,
           mean = mean/24,
           median = median/24)%>%
    filter(site == !!site) %>%
    ggplot(aes(day)) +
    geom_vline(aes(xintercept = median, col = "Median"), linetype = 2, size = 1.1) +
    geom_vline(aes(xintercept = mean, col = "Mean"), linetype = 2, size = 1.1) +
    # geom_text(aes(x = mean, y = 0.1, label = "mean")) +
    geom_line(aes(y = y)) +
    geom_ribbon(aes(ymax = y, ymin = y_min), alpha = 0.25) +
    scale_colour_discrete(guide = guide_legend(direction = "horizontal")) +
    scale_x_continuous("LOS (days)", limits = c(0, 10), breaks = seq(0, 10, 2)) +
    labs(title = "Length of stay distribution",
         y = "",
         col = "") +
    theme_bw() +
    theme(axis.text.y = element_blank(),
          # axis.text.x = element_text(size = rel(1)),
          # axis.title.x = element_text(size = rel(1)),
          legend.position = c(0.75, 0.80),
          legend.justification = 0.5,
          legend.background = element_rect(fill="white",
                                           colour = "white",
                                           size=2, linetype="solid"),
          legend.key.size = unit(3, "line"),
          # legend.text = element_text(size = rel(1))
          )
  
}

plot_occ_component <- function(df, input_site, title) {
  df %>%
    filter(site_dropdown == input_site) %>%
    pivot_wider(names_from = "metric",
                values_from = "value") %>%
    ggplot(aes(x = datetime, y = mean)) +
    geom_ribbon(aes(ymin = l_95, ymax = u_95), alpha = 0.25) +
    geom_line() +
    labs(title = title,
         x = "",
         y = "") + 
    theme_bw()
  }
  

construct_filename <- function(text) {
  systime <- Sys.time()
  date_report <- format(systime, format = '%d%b%y')
  time_report <- format(systime, format = '%H%M')
  glue::glue('Nowcast_{text}_{date_report}_{time_report}.html')
}

render_md <- function(md_file, output_file, params){
  tempReport <- file.path(tempdir(), md_file)
  file.copy(as.character(glue::glue("./{md_file}")),
            tempReport, overwrite = TRUE)
  
  rmarkdown::render(tempReport, output_file = output_file,
                    params = params,
                    envir = new.env(parent = globalenv()))
}

