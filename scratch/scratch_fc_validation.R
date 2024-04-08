library(fitdistrplus)
library(tidyverse)
library(forecast)
library(tsibble)
library(fable)
library(fabletools)
library(fable.prophet)


con <- switch(.Platform$OS.type,
              windows = RODBC::odbcConnect(dsn = "xsw"),
              unix = {"/root/sql/sql_connect_string_linux" |>
                  readr::read_lines() |>
                  RODBC::odbcDriverConnect()}
)

nctr_df <-
  RODBC::sqlQuery(
    con,
    "SELECT
       [RN]
      ,[Organisation_Code_Provider]
      ,[Organisation_Code_Commissioner]
      ,[Census_Date]
      ,[Month_Date]
      ,[Month]
      ,[Day_Of_Week]
      ,[Week_Start_Date]
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
      ,[Days_NCTR_On_Current_Code]
      ,[Current_Delay_Code]
      ,[Local_Authority_grouped]
      ,[Site_Name]
      ,[Current_Delay_Code_Standard]
      ,[Current_Delay_Code_Detailed]
      ,[Acute Community split]
      ,[Current_Covid_Status]
      ,[Planned_Date_Of_Discharge]
      ,[Date_Toc_Form_Completed]
      ,[Toc_Form_Status]
      ,[Discharge_Pathway]
      ,[DER_File_Name]
      ,[DER_Load_Timestamp]
  FROM Analyst_SQL_Area.dbo.vw_NCTR_Status_Report_Daily_JI"
  )




  # arima training ends at the start of the reporting
  fc_train_length <- 26 # (train length in weeks)
  # fc_end <- report_start
  # fc_start <- fc_end  - dweeks(fc_train_length)
  # 
  # min_adm_date <- nctr_df %>%
  #   filter(!is.na(NHS_Number)) %>%
  #   filter(Organisation_Site_Code %in% c('RVJ01', 'RA701', 'RA301', 'RA7C2')) %>%
  #   group_by(Organisation_Site_Code) %>%
  #   summarise(date = as.Date(max(Date_Of_Admission))) %>%
  #   pull(date) %>%
  #   min()
  
  fcast_days <- 10
  
  
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
    mutate(data_tr = map(data, stretch_tsibble, .init = fc_train_length*7, .step = 10)) %>%
    mutate(
      # model = map(data, ~model(.x, mdl = prophet(n))),
      model = map(data_tr, ~model(.x, mdl = ARIMA(n))),
      fc = map(model, forecast, h = fcast_days))  %>%
    mutate(acc = map2(fc, data, ~accuracy(.x, data = .y, by = c(".id", ".model")))) %>%
    mutate(mape = map_dbl(acc, ~.x %>% pull(MAPE) %>% mean)) %>%
    mutate(mae = map_dbl(acc, ~.x %>% pull(MAE) %>% mean))

  # autoplot(models$fc[[1]] %>% filter(.model == "mdl", .id == 1) %>% select(-.id),
  #          filter(models$data_tr[[1]], .id ==2) %>% filter(date >= max(date) -dweeks(6)) %>% select(-.id))

    autoplot(models$fc[[1]] %>% filter(.model == "mdl", .id == 10) %>% select(-.id),
             models$data[[1]] %>% filter(date >= max(date) -dweeks(6)))

  
  seq <- head(sort(unique(models$fc[[1]]$.id)), -1)
  
  
 out <- pmap(list(models$fc,
                  models$data_tr,
                  recode(models$site, "weston" = "wgh"),
                  models$mape), function(fc, data, site, mape) {
   map(seq, ~ {
     autoplot(
       fc %>% filter(.model == "mdl", .id == .x) %>% select(-.id),
       filter(data, .id == .x + 1) %>%
             filter(date >= max(date) - dweeks(6)) %>%
             select(-.id) 
     ) + theme_bw() + theme(legend.position = "off") +
       labs(y = glue::glue("Daily admissions, {str_to_upper(site)} (MAPE: {round(mape,2 )})")) 
   })
 })
       

plots <- map2(out, models$site, ~{
  patchwork::wrap_plots(.x, ncol = 1, axes = "collect", guides = "collect") +
  patchwork::plot_annotation(title = .y)
})  

(validation_plot_fc <- patchwork::wrap_plots(plots, nrow = 1, axes = "collect", guides = "collect"))


ggsave(
  validation_plot_fc,
  filename = "./validation/validation_plot_fc.png",
  scale = 0.5,
  width = 16,
  height = 12
)
