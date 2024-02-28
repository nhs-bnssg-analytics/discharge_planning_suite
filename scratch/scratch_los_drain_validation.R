nctr_sum <- nctr_df %>%
  filter(Person_Stated_Gender_Code %in% 1:2) %>%
  filter(Census_Date < max(Census_Date) -ddays(10)) %>%
  mutate(nhs_number = as.character(NHS_Number),
         nhs_number = if_else(is.na(nhs_number), glue::glue("unknown_{1:n()}"), nhs_number),
         sex = if_else(Person_Stated_Gender_Code == 1, "Male", "Female")) %>% 
  # mutate(nhs_number[is.na(nhs_number)] = glue::glue("unknown_{seq_along(nhs_number[is.na(nhs_number)])}"))
  mutate(Organisation_Site_Code = case_when(Organisation_Site_Code == 'RVJ01' ~ 'nbt',
                                            Organisation_Site_Code == 'RA701' ~ 'bri',
                                            Organisation_Site_Code %in% c('RA301', 'RA7C2') ~ 'weston',
                                            TRUE ~ '')) %>%
  group_by(nhs_number, Date_Of_Admission) %>%
  mutate(spell_id = cur_group_id()) 


dates_spells <- nctr_sum %>%
  group_by(Census_Date) %>%
  summarise(spell_id = unique(spell_id))





dates <- nctr_df %>%
  filter(Census_Date > ymd("2023-07-01"),
         Census_Date < max(Census_Date) - ddays(10)) %>%
  pull(Census_Date) %>%
  unique()


# take sample of dates
d_i <- sample(dates, 1)
# spell ids from this date:
sid_i <- dates_spells %>%
  filter(Census_Date == d_i) %>%
  pull(spell_id) %>% 
  unique()

# Calculate the drain for these patients

emp_drain <- nctr_sum %>%
  filter(Person_Stated_Gender_Code %in% 1:2) %>%
  mutate(nhs_number = as.character(NHS_Number),
         nhs_number = if_else(is.na(nhs_number), glue::glue("unknown_{1:n()}"), nhs_number),
         sex = if_else(Person_Stated_Gender_Code == 1, "Male", "Female"))  %>%
  filter(Criteria_To_Reside == "Y") %>%
  filter(Census_Date >= d_i, 
         spell_id %in% sid_i) %>%
  group_by(spell_id) %>%
  mutate(los = min(Current_LOS),
         los_dis_rdy = if_else(any(!is.na(Date_NCTR)), min(Current_LOS[!is.na(Date_NCTR)]), max(Current_LOS))) %>% 
  select(nhs_number = NHS_Number,
         spell_id,
         bed_type = Bed_Type,
         los,
         los_dis_rdy) %>%
  distinct() %>%
  mutate(days_until_rdy = los_dis_rdy - los) %>%
  group_by(day = days_until_rdy) %>%
  count()

los_wf <- readRDS("data/los_wf.RDS")

sim_drain <- nctr_sum %>%
  filter(Person_Stated_Gender_Code %in% 1:2) %>%
  mutate(nhs_number = as.character(NHS_Number),
         nhs_number = if_else(is.na(nhs_number), glue::glue("unknown_{1:n()}"), nhs_number),
         sex = if_else(Person_Stated_Gender_Code == 1, "Male", "Female"))  %>%
  filter(Criteria_To_Reside == "Y") %>%
  filter(Census_Date >= d_i, 
         spell_id %in% sid_i) %>%
  group_by(spell_id) %>%
  mutate(los = min(Current_LOS),
         los_dis_rdy = if_else(any(!is.na(Date_NCTR)), min(Current_LOS[!is.na(Date_NCTR)]), max(Current_LOS))) %>% 
  select(nhs_number,
         spell_id,
         site, 
         age = Person_Age
         sex = Person_Stated_Gender_Code
         bed_type = Bed_Type,
         los) %>%
  distinct() %>%
  left_join(select(attr_df, -sex, -age) %>% mutate(nhs_number = as.character(nhs_number)),
            by = join_by(nhs_number == nhs_number)) %>%
  bake(extract_recipe(los_wf), .) %>%
  mutate(leaf = as.character(treeClust::rpart.predict.leaves(extract_fit_engine(los_wf), .)))
  

  

# take maximum date we have data for each patient and admission
  filter(Census_Date == max(Census_Date)) %>%
  ungroup() %>%
  # keep only patients with either NCTR (we know they are ready for discharge)
  # OR whos max date recorded is before the latest data (have been discharged)
  filter(!is.na(Date_NCTR) | Census_Date != max(Census_Date)) %>%
  # Compute the fit for discharge LOS
  mutate(discharge_rdy_los = ifelse(!is.na(Date_NCTR), Current_LOS - Days_NCTR, Current_LOS + 1))  %>%
  # remove negative LOS (wrong end timestamps?)
  filter(discharge_rdy_los > 0) %>%
  filter(Organisation_Site_Code %in% c('RVJ01', 'RA701', 'RA301', 'RA7C2')) %>%
  dplyr::select(
    nhs_number = nhs_number,
    site = Organisation_Site_Code,
    sex,
    age = Person_Age,
    spec = Specialty_Code,
    bed_type = Bed_Type,
    los = discharge_rdy_los
  )
