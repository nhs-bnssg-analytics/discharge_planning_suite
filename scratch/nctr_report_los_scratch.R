library(fitdistrplus)
library(tidyverse)
con <- switch(.Platform$OS.type,
              windows = RODBC::odbcConnect(dsn = "xsw"),
              unix = xswauth::modelling_sql_area()
)

age_breaks <- c(0, 45, seq(65, 85, by = 20), Inf)

los_df <-
  RODBC::sqlQuery(
    con,
    "SELECT
       [Organisation_Code_Provider]
      ,[Organisation_Code_Commissioner]
      ,[Census_Date]
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
      ,[Current_Delay_Code]
      ,[Current_Covid_Status]
      ,[Planned_Date_Of_Discharge]
      ,[Date_Toc_Form_Completed]
      ,[Toc_Form_Status]
      ,[Discharge_Pathway]
      ,[DER_File_Name]
      ,[DER_Load_Timestamp]
  FROM [Analyst_SQL_Area].[dbo].[NCTR_Status_Report_Daily]"
  )


los_df <- los_df %>%
  group_by(NHS_Number) %>%
  # take maximum date we have data for each patient
  filter(Census_Date == max(Census_Date)) %>%
  ungroup() %>%
  # keep only patients with either NCTR (we know they are ready for discharge)
  # OR whos max date recorded is before the latest data (have been discharged)
  filter(!is.na(Date_NCTR) | Census_Date != max(Census_Date)) %>%
  # Compute the fit for discharge LOS
  mutate(discharge_los = ifelse(!is.na(Date_NCTR), Current_LOS - Days_NCTR, Current_LOS + 1))  %>%
  # remove negative LOS (wrong end timestamps?)
  filter(discharge_los > 0) %>%
  filter(Organisation_Site_Code %in% c('RVJ01', 'RA701', 'RA301', 'RA7C2')) %>%
  mutate(Organisation_Site_Code = case_when(Organisation_Site_Code == 'RVJ01' ~ 'nbt',
                           Organisation_Site_Code == 'RA701' ~ 'bri',
                           Organisation_Site_Code %in% c('RA301', 'RA7C2') ~ 'weston',
                           TRUE ~ ''),
         age_band = cut(Person_Age,
                        breaks = age_breaks, include.lowest = TRUE)) %>%
  dplyr::select(site = Organisation_Site_Code, gender = Person_Stated_Gender_Code, age = Person_Age, age_band, los = discharge_los) %>%
  bind_rows(mutate(., age_band = "total"))

fit_lnorm <- los_df %>%
             dplyr::select(site, age_band, los) %>%
             group_by(site, age_band) %>%
             nest() %>%
             mutate(fit = map(data, ~fitdist(.x$los, "lnorm"))) %>%
             mutate(fit = map(fit, pluck, "estimate"))  %>%
             select(site, age_band, fit) %>%
             mutate(fit = set_names(fit, age_band))


fit_lnorm_split <- split(fit_lnorm$fit, fit_lnorm$site)

# Age-decile proportions
# pathway data from UHBW

pathway_df <- read_csv("resources/discharge.csv")%>%
  rename(pathway = "Final Pathway Type") %>% 
  mutate(pathway = coalesce(pathway, "P0")) %>%
  mutate(pathway = recode(pathway,
                          "P0 - patient has been discharged while on no discharge pathway, does have Adult Discharge Log Clinical Note (may have been on pathway previously, may not)" = "P0",
                          "Awaiting Referral" = "Other",
                          "No Longer Required" = "P0",
                          "Awaiting Decision" = "Other"
  )) 




prop_df <- pathway_df %>%
  filter(!is.na(age), age >= 0, pathway != "Other") %>%
  mutate(age_band = cut(age,
                        breaks = age_breaks, include.lowest = TRUE)) %>%
  bind_rows(mutate(., age_band = "total")) %>%
  group_by(age_band,pathway) %>%
  summarise(pathway_count = n()) %>%
  group_by(age_band) %>%
  mutate(pathway_prop = pathway_count/sum(pathway_count)) %>%
  mutate(pathway_prop = set_names(pathway_prop, pathway)) %>%
  select(-pathway_count, -pathway) %>%
  group_by(age_band) %>%
  nest(.key = "prop") %>%
  mutate(prop = map(prop, pluck, 1)) %>%
  mutate(prop = set_names(prop, age_band)) %>%
  expand_grid(site = c("bri", "weston"))

prop_split <- split(prop_df$prop, prop_df$site)


ggplot(prop_df, aes(x = age_band, y = pathway_prop, fill = pathway)) + geom_col()




# los_df <-
#   RODBC::sqlQuery(
#     con,
#     "SELECT
#      AIMTC_Pseudo_NHS AS nhs_number,
#      AIMTC_Age as age,
#      AIMTC_ProviderSpell_Start_Date AS start_date,
#      StartTime_HospitalProviderSpell AS start_time,
#      AIMTC_ProviderSpell_End_Date AS end_date,
#      DischargeTime_HospitalProviderSpell AS end_time,
#      AdmissionMethod_HospitalProviderSpell AS admission_method,
#      OrganisationCode_CodeOfProvider AS site,
#      SiteCode_ofTreatmentAtEpisodeStartDate AS site2
#      FROM ABI.dbo.vw_APC_SEM_Spell_001
#      WHERE
#      (AIMTC_ProviderSpell_Start_Date >= TRY_CAST('2022-01-01' AS DATE)) AND
#      (AIMTC_ProviderSpell_End_Date <= TRY_CAST('2022-12-31' AS DATE)) AND
#      (SiteCode_ofTreatmentAtEpisodeStartDate IN ('RVJ01', 'RA701', 'RA301', 'RA7C2'))"
#   )
# 
# 
# los_df <- los_df %>% 
#   na.omit() %>%
#   mutate(across(matches('date|time'), as.character)) %>%
#   mutate(across(matches('time'), ~stringr::str_extract(.x, '\\d{2}:\\d{2}:\\d{2}'))) %>%
#   # do parse_date_time as some timestamps are 'wrong' -- BST/GMT conversions?
#   mutate(start_dt = lubridate::parse_date_time(paste(start_date, start_time), orders = c("ymd HMS")), 
#          end_dt = lubridate::parse_date_time(paste(end_date, end_time), orders = c("ymd HMS")), 
#          los = as.numeric(difftime(end_dt, start_dt, units = 'hours'))) %>%
#   # remove negative LOS (wrong end timestamps?)
#   filter(los >= 0) %>%
#   filter(site2 %in% c('RVJ01', 'RA701', 'RA301', 'RA7C2')) %>%
#   mutate(site2 = case_when(site2 == 'RVJ01' ~ 'nbt', 
#                            site2 == 'RA701' ~ 'bri', 
#                            site2 %in% c('RA301', 'RA7C2') ~ 'weston', 
#                            TRUE ~ '')) %>%
#   mutate(age_band = cut(age,
#                         breaks = age_breaks, include.lowest = TRUE))