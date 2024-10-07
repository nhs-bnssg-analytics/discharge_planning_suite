validation_end <- ymd("2024-09-01")
validation_start <- ymd("2023-07-01")
weeks_testing <- 13
start_date <- validation_end - dweeks(weeks_testing ) 
# nctr_df <- nctr_df %>% filter(between(Census_Date, validation_start, validation_end-ddays(1)))
# 
# nctr_sum <- nctr_df %>%
#   filter(Person_Stated_Gender_Code %in% 1:2) %>%
#   mutate(nhs_number = as.character(NHS_Number),
#          nhs_number = if_else(is.na(nhs_number), glue::glue("unknown_{1:n()}"), nhs_number),
#          sex = if_else(Person_Stated_Gender_Code == 1, "Male", "Female")) %>%
#   filter(Organisation_Site_Code %in% c('RVJ01', 'RA701', 'RA301', 'RA7C2')) %>%
#   mutate(
#     site = case_when(
#       Organisation_Site_Code == 'RVJ01' ~ 'nbt',
#       Organisation_Site_Code == 'RA701' ~ 'bri',
#       Organisation_Site_Code %in% c('RA301', 'RA7C2') ~ 'weston',
#       TRUE ~ 'other'
#     ),
#     Date_Of_Admission = as.Date(Date_Of_Admission)
#   ) %>%
#   group_by(Organisation_Site_Code) %>%
#   # filter(Census_Date == max(Census_Date)) %>%
#   filter(site != "nbt") %>%
#   ungroup() %>%
#   mutate(
#     der_los = (as.Date(Census_Date) - as.Date(Date_Of_Admission))/ddays(1),
#     der_ctr = case_when(
#       Criteria_To_Reside == "Y" | is.na(Criteria_To_Reside) ~ TRUE,
#       !is.na(Days_NCTR) ~ FALSE,
#       !is.na(Date_NCTR) ~ FALSE,
#       Criteria_To_Reside == "N" ~ FALSE
#     )) %>%
#   mutate(report_date = max(Census_Date)) %>%
#   mutate(los = (report_date - Date_Of_Admission) / ddays(1)) %>%
#   mutate(
#     pathway = recode(
#       Current_Delay_Code_Standard,
#       !!!pathway_recodes
#     ),
#     pathway = coalesce(pathway, "Other")
#   ) %>%
#   mutate(pathway = if_else(
#     !pathway %in% c("P1", "P2", "P3", "P3" , "Other"),
#     "Other",
#     pathway
#   )) %>%
#   dplyr::select(
#     report_date,
#     date_of_admission = Date_Of_Admission,
#     nhs_number,
#     sex,
#     age = Person_Age,
#     ctr = der_ctr,
#     site,
#     bed_type = Bed_Type,
#     los = der_los,
#     pathway
#   ) %>%
#   ungroup()
# 
# mortality_df <- local({
#   string_mortality <-"SELECT
#       [Derived_Pseudo_NHS]
#       ,[Dec_Age_At_Death]
#       ,[DEC_SEX]
#       ,[DEC_SEX_DESC]
#       ,[DEC_MARITAL_STATUS]
#       ,[DEC_MARITAL_STATUS_DESC]
#       ,[DEC_AGEC]
#       ,[DEC_AGECUNIT]
#       ,[DEC_AGECUNIT_DESC]
#       ,[REG_DATE_OF_DEATH]
#       ,[REG_DATE]
#   FROM [ABI].[Civil_Registration].[Mortality]"
#   con<-RODBC::odbcDriverConnect("driver={SQL Server};\n  server=Xsw-00-ash01;\n  trusted_connection=true")
#   
#   RODBC::sqlQuery(con, string_mortality) %>%
#     na.omit() %>%
#     mutate(Derived_Pseudo_NHS = as.character(Derived_Pseudo_NHS),
#            REG_DATE_OF_DEATH = lubridate::ymd(REG_DATE_OF_DEATH)) %>%
#     mutate(Derived_Pseudo_NHS = as.character(Derived_Pseudo_NHS)) %>%
#     select(nhs_number = Derived_Pseudo_NHS, date_death = REG_DATE_OF_DEATH)
# })
# 
# # remove patients who died
# 
# nctr_sum <- nctr_sum %>%
#   filter(!nhs_number %in% mortality_df$nhs_number)

# Lets use the los data instead as it has a few bespoke filters (~1k patient difference)

los_df <- readRDS("data/los_df.RDS")

# Number of admissions

los_df %>% 
  select(nhs_number, admission_date) %>% 
  distinct() %>%
  nrow()


# Number of patients

los_df %>% 
  select(nhs_number) %>% 
  distinct() %>%
  nrow()

# demographic table

# attr_df <-
#   RODBC::sqlQuery(
#     con,
#     "select * from (
# select a.*, ROW_NUMBER() over (partition by nhs_number order by attribute_period desc) rn from
# [MODELLING_SQL_AREA].[dbo].[New_Cambridge_Score] a) b where b.rn = 1"
#   )

attr_df <- readRDS("data/attr_df.RDS")

demo_tab <- los_df %>%
  ungroup() %>%
  mutate(grp = ifelse(Census_Date <= validation_end - dweeks(weeks_testing), yes = "Calibration Set", no = "Validation Set")) %>%
  mutate(grp = factor(grp, levels = c("Full Data", "Calibration Set", "Validation Set"))) %>%
  # mutate(s_id = cur_group_id(), .by = c(nhs_number, admission_date)) %>%
  # mutate(n_unique_adm = n_distinct(s_id), .by = grp) %>%
  rbind(mutate(., grp = "Full Data")) %>%
  group_by(nhs_number) %>%
  filter(admission_date == min(admission_date)) %>%
  select(grp, nhs_number, sex, age, bed_type) %>%
  distinct() %>%
  left_join(select(attr_df, nhs_number, cambridge_score) %>% 
              mutate(nhs_number = as.character(nhs_number))) %>%
  # filter(!str_detect(nhs_number, "unknown")) %>%
  ungroup() %>%
  select(-nhs_number)

n_adm <- los_df %>%
  ungroup() %>%
  mutate(grp = ifelse(Census_Date <= validation_end - dweeks(weeks_testing), yes = "Calibration Set", no = "Validation Set")) %>%
  rbind(mutate(., grp = "Full Data")) %>%
  mutate(grp = factor(grp, levels = c("Full Data", "Calibration Set", "Validation Set"))) %>%
  arrange(grp) %>%
  mutate(s_id = cur_group_id(), .by = c(nhs_number, admission_date)) %>%
  summarise(n_unique_adm = n_distinct(s_id), .by = grp) %>%
  pull(n_unique_adm) 


demo_tab_out <- demo_tab %>%
 gtsummary::tbl_summary(label = list(
                                     sex = "Sex",
                                     age = "Age",
                                     bed_type = "Bed Type",
                                     cambridge_score = "Cambridge Score"
                                     ),
                        missing_text = "Missing",
                        by = grp) %>%
  gtsummary::as_hux_table() 


# modify2(demo_tab_out, c(0, n_adm), .at = c("stat_1", "stat_2", "stat_3"), ~modify_at(.x, 1, \(x) str_c(x, number(.y, big.mark = ","))))


demo_tab_out[[1+1]][1] <- str_c(demo_tab_out[[1+1]][1], " (N admissions = ", number(n_adm[1], big.mark = ","), ")")
demo_tab_out[[1+2]][1] <- str_c(demo_tab_out[[1+2]][1], " (N admissions = ", number(n_adm[2], big.mark = ","), ")")
demo_tab_out[[1+3]][1] <- str_c(demo_tab_out[[1+3]][1], " (N admissions = ", number(n_adm[3], big.mark = ","), ")")


demo_tab_out %>%
huxtable::quick_html(file = "materials/demog_tab.html")

