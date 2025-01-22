library(fitdistrplus)
library(tidyverse)
library(tidymodels)
library(lubridate)

con <- switch(.Platform$OS.type,
              windows = RODBC::odbcConnect(dsn = "xsw"),
              unix = {"/root/sql/sql_connect_string_linux" |>
                  readr::read_lines() |>
                  RODBC::odbcDriverConnect()}
)

# latest nctr data
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

# max census date
max_date <- nctr_df %>%
  filter(!is.na(NHS_Number)) %>%
  filter(Organisation_Site_Code %in% c('RVJ01', 'RA701', 'RA301', 'RA7C2')) %>%
  pull(Census_Date) %>%
  max()


admissions_nctr <- nctr_df %>%
  # filter(!is.na(NHS_Number)) %>%
  mutate(NHS_Number = ifelse(is.na(NHS_Number), CDS_Unique_Identifier, NHS_Number)) %>%
  filter(Organisation_Site_Code %in% c('RVJ01', 'RA701', 'RA301', 'RA7C2')) %>%
  mutate(site = case_when(Organisation_Site_Code == 'RVJ01' ~ 'NBT',
                          Organisation_Site_Code == 'RA701' ~ 'BRI',
                          Organisation_Site_Code %in% c('RA301', 'RA7C2') ~ 'WGH',
                          TRUE ~ '')) %>%
  group_by(nhs_number = NHS_Number) %>%
  distinct(Date_Of_Admission, .keep_all = TRUE) %>%
  group_by(site , date = as.Date(Date_Of_Admission)) %>%
  count() %>%
  filter(date > ymd("2023-07-01")) %>% # data before this are spurious  
  mutate(source = "nctr")


ucdaily_df <-
  RODBC::sqlQuery(
    con,
    "SELECT
    provider,
    metric_id,
    metric_name,
    value,
    report_date
    FROM [Analyst_SQL_Area].[dbo].[tbl_BNSSG_Datasets_UrgentCare_Daily]
    where metric_id in (346098, 346183, 347331)
    and report_date > '2022-07-01'"
  )

admissions_ucdaily <-  ucdaily_df %>%
  dplyr::select(site = provider, n = value, date = report_date) %>%
  mutate(source = "urgent care daily") %>%
  arrange(date) %>%
  distinct()

bind_rows(admissions_nctr, admissions_ucdaily) %>%
  ungroup() %>%
  filter(date > max(date) - dweeks(8)) %>%
  ggplot(aes(x = date, y = n, col = source)) +
  geom_path() +
  scale_y_continuous(limits = c(0, NA)) +
  ggh4x::facet_grid2(site~., independent = "y", scales = "free") +
  labs(title = "Total admissions, per data source")



# respiratory codes

resp_codes <- c(
  '340', # broad code
  '22',  # local Thoracic medicine code
  '2S',  # local Respiratory medicine code
  '88',   # local Thoracic surgery code
  'F7',  # local Virtual nodule respiratory code
  '7R'  # local Respiratory Lung Function Testing code
  )

resp_admissions_nctr <- nctr_df %>%
  filter(Specialty_Code %in% resp_codes) %>%
  # filter(!is.na(NHS_Number)) %>%
  filter(Organisation_Site_Code %in% c('RVJ01', 'RA701', 'RA301', 'RA7C2')) %>%
  mutate(site = case_when(Organisation_Site_Code == 'RVJ01' ~ 'NBT',
                          Organisation_Site_Code == 'RA701' ~ 'BRI',
                          Organisation_Site_Code %in% c('RA301', 'RA7C2') ~ 'WGH',
                          TRUE ~ '')) %>%
  group_by(nhs_number = NHS_Number) %>%
  distinct(Date_Of_Admission, .keep_all = TRUE) %>%
  group_by(site , date = as.Date(Date_Of_Admission)) %>%
  count() %>%
  filter(date > ymd("2023-07-01")) %>% # data before this are spurious  
  mutate(source = "nctr")


resp_admissions_nctr %>%
  ungroup() %>%
  filter(date > max(date) - dweeks(26)) %>%
ggplot(aes(x = date, y = n)) +
  geom_path() + 
  ggh4x::facet_grid2(site~., independent = "y", scales = "free") +
  labs(title = "NCTR admissions, Respiratory specialty codes")


library(feasts)
admissions_ucdaily %>%
  ungroup() %>%
filter(year(date) != 2022) %>%
summarise(n = sum(n, na.rm = TRUE), .by = c(date)) %>%
 as_tsibble(index = date)  %>%
  tsibble::fill_gaps(n = 0) %>%
  gg_season() +
  scale_x_date(limits = c(ymd(2024-11-01), ymd(2025-01-01)))

admissions_ucdaily %>% 
  ungroup() %>%
  summarise(n = sum(n, na.rm = TRUE), .by = c(date)) %>%
  mutate(year = year(date), month = month(date), day = day(date)) %>% 
  mutate(grp = ifelse(month %in% c(11,1), "yes", "no")) %>%
  mutate(grp = str_c(grp, cumsum(grp != lag(grp, default = "x")), sep = "_")) %>%
  mutate(grp = min(year), .by = grp) %>%
  mutate(date = ymd(str_c(ifelse(month == 1, "2025", "2024"), month, day, sep = "-"))) %>%
  filter(month %in% c(11, 1)) %>%
  ggplot(aes(x = date, y = n, col = factor(grp), group = grp)) + 
  geom_line() 


+
  ggh4x::facet_grid2(site~., independent = "y", scales = "free") 


admissions_ucdaily %>%
  summarise(n = sum(n, na.rm = TRUE), .by = c(date)) %>%
  mutate(year = year(date), month = month(date, label = TRUE), day = day(date)) %>%
  filter(year != 2022) %>%
  filter(month %in% c("Nov" ,"Dec", "Jan")) %>%
  ggplot(aes(x = day, y = n, col = factor(year))) + 
  geom_line() +
  scale_y_continuous(limits = c(0, NA)) +
  ggh4x::facet_grid2(.~month, independent = "y", scales = "free") +
  labs(title = "Seasonal plot - urgent care daily data")
  

admissions_nctr %>%
  ungroup() %>%
  summarise(n = sum(n, na.rm = TRUE), .by = c(date)) %>%
  mutate(year = year(date), month = month(date, label = TRUE), day = day(date)) %>%
  filter(month %in% c("Nov" ,"Dec", "Jan")) %>%
  ggplot(aes(x = day, y = n, col = factor(year))) + 
  geom_line() +
  scale_y_continuous(limits = c(0, NA)) +
  ggh4x::facet_grid2(.~month, independent = "y", scales = "free") +
  labs(title = "Seasonal plot - NCTR data")
