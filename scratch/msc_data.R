

admissions_full <- nctr_df %>%
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



admissions_strata <- nctr_df %>%
  filter(!is.na(NHS_Number)) %>%
  filter(Organisation_Site_Code %in% c('RVJ01', 'RA701', 'RA301', 'RA7C2')) %>%
  mutate(site = case_when(Organisation_Site_Code == 'RVJ01' ~ 'nbt',
                          Organisation_Site_Code == 'RA701' ~ 'bri',
                          Organisation_Site_Code %in% c('RA301', 'RA7C2') ~ 'weston',
                          TRUE ~ '')) %>%
  group_by(nhs_number = NHS_Number) %>%
  distinct(Date_Of_Admission, .keep_all = TRUE) %>% 
  ungroup() %>%
  left_join(select(attr_df, nhs_number, cambridge_score, segment), by = join_by(nhs_number == nhs_number)) %>%
  mutate(age = cut(Person_Age, breaks = c(0, 60, Inf), right = FALSE,  labels = c(
    "18-59",
    "60+"
    )),
    Bed_Type = factor(Bed_Type),
    Bed_Type = fct_na_value_to_level(Bed_Type, level = "Other"),
    Bed_Type = fct_lump_n(Bed_Type, n = 5)) %>%
  select(site, date = Date_Of_Admission, age, bed_type = Bed_Type, segment) %>%
  group_by(site,
           date = as.Date(date),
           # age,
           bed_type#,
           # segment
           ) %>%
  count() %>%
  ungroup() %>%
  filter(date > ymd("2023-07-01")) # data before this are spurious 


admissions_strata %>%
  ggplot(aes(x = date, y = n)) +
  geom_line() + 
  facet_wrap(vars(site, bed_type))


wf <- readRDS("data/los_wf.RDS")

admissions_strata_dtree <- nctr_df %>%
  filter(!is.na(NHS_Number)) %>%
  filter(Organisation_Site_Code %in% c('RVJ01', 'RA701', 'RA301', 'RA7C2')) %>%
  mutate(site = case_when(Organisation_Site_Code == 'RVJ01' ~ 'nbt',
                          Organisation_Site_Code == 'RA701' ~ 'bri',
                          Organisation_Site_Code %in% c('RA301', 'RA7C2') ~ 'weston',
                          TRUE ~ '')) %>%
  group_by(nhs_number = NHS_Number) %>%
  distinct(Date_Of_Admission, .keep_all = TRUE) %>% 
  ungroup() %>%
  left_join(select(attr_df, nhs_number, cambridge_score, segment), by = join_by(nhs_number == nhs_number)) %>%
  mutate(sex = if_else(Person_Stated_Gender_Code == 1, "Male", "Female"),
         date = as.Date(parse_date_time(Date_Of_Admission, orders = c("ymd HMS")))) %>%
  select(nhs_number = NHS_Number,
         age = Person_Age,
         sex,
         cambridge_score,
         bed_type = Bed_Type,
         site,
         spec = Specialty_Code,
         date)



leaf <- admissions_strata_dtree %>%
  recipes::bake(workflows::extract_recipe(wf), .) %>%
  mutate(leaf = as.character(treeClust::rpart.predict.leaves(workflows::extract_fit_engine(wf), .))) %>%
  pull(leaf) 


admissions_strata_dtree <- admissions_strata_dtree %>%
  mutate(leaf = leaf) %>%
  group_by(site,
           date,
           leaf
  ) %>%
  count() %>%
  ungroup() %>%
  filter(date > ymd("2023-07-01")) # data before this are spurious 


admissions_strata_dtree %>%
  ggplot(aes(x = date, y = n)) +
  geom_line() + 
  facet_wrap(vars(leaf))


# make an excel output ----------------------------------------------------

library(openxlsx)
wb <- createWorkbook()

# make an about sheet
addWorksheet(wb, "About")
# add data
writeData(
  wb, "About",
  tibble::tribble(
    ~`Contact`,~`nick.howlett5@nhs.net`,
    "Data source","NCTR daily flow",
    "About", "This data was extracted from the NCTR daily flow - collated and aggregated by Nick Howlett",
    "Date Range", paste(min(admissions_full$date ),"to",max(admissions_full$date)),
    "Date run", format(Sys.Date(),"%Y-%m-%d")#,
    #"Code source", "https://github.com/nhs-bnssg-analytics/OPEL4-Frontier/tree/main/R/acute_occupancy.R"
  ))
# add the actual data
addWorksheet(wb, "Admissions aggregate")
writeData(wb, "Admissions aggregate", admissions_full)
addWorksheet(wb, "Admissions by bed type")
writeData(wb, "Admissions by bed type", admissions_strata)
addWorksheet(wb, "Admissions by leaf")
writeData(wb, "Admissions by leaf", admissions_strata_dtree)

# save appropriately
saveWorkbook(
  wb,
  here::here(
    "data",
    paste0(paste0(min(admissions_full$date),
                  "-to-",max(admissions_full$date),
                  "-acute-admissions.xlsx"))),
  overwrite = TRUE)  

