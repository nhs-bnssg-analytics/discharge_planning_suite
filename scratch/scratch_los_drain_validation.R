source("utils.R")
n_rep <- 1E4

nctr_sum <- nctr_df %>%
  filter(Person_Stated_Gender_Code %in% 1:2) %>%
  mutate(nhs_number = as.character(NHS_Number),
         nhs_number = if_else(is.na(nhs_number), glue::glue("unknown_{1:n()}"), nhs_number),
         sex = if_else(Person_Stated_Gender_Code == 1, "Male", "Female"))  %>%
  filter(Census_Date < max(Census_Date) -ddays(10)) %>%
  # mutate(nhs_number[is.na(nhs_number)] = glue::glue("unknown_{seq_along(nhs_number[is.na(nhs_number)])}"))
  mutate(Organisation_Site_Code = case_when(Organisation_Site_Code == 'RVJ01' ~ 'nbt',
                                            Organisation_Site_Code == 'RA701' ~ 'bri',
                                            Organisation_Site_Code %in% c('RA301', 'RA7C2') ~ 'weston',
                                            TRUE ~ '')) %>%
  group_by(nhs_number, Date_Of_Admission) %>%
  mutate(spell_id = cur_group_id(),
         der_los = (as.Date(Census_Date) - as.Date(Date_Of_Admission))/ddays(1),
         der_ctr = case_when(
           Criteria_To_Reside == "Y" | is.na(Criteria_To_Reside) ~ TRUE,
           !is.na(Days_NCTR) ~ FALSE,
           !is.na(Date_NCTR) ~ FALSE,
           Criteria_To_Reside == "N" ~ FALSE
         )) 


dates_spells <- nctr_sum %>%
  group_by(Census_Date, Criteria_To_Reside, Days_NCTR, spell_id) %>%
  distinct()





dates <- nctr_df %>%
  filter(Census_Date > ymd("2023-07-01"),
         Census_Date < max(Census_Date) - ddays(10)) %>%
  pull(Census_Date) %>%
  unique()


# take sample of dates
d_i <- sample(dates, 1)
# spell ids with CTR from this date:
sid_i <- dates_spells %>%
  filter(Census_Date == d_i & (Criteria_To_Reside == "Y" & (is.na(Days_NCTR) | Days_NCTR == 0))) %>%
  pull(spell_id) %>% 
  unique()

# Calculate the drain for these patients

emp_drain <- nctr_sum %>%
  #first, only filter spells we are interested in
  filter(spell_id %in% sid_i) %>%
  filter(Census_Date >= d_i) %>%
  arrange(Census_Date) %>%
  group_by(spell_id) %>%
  mutate(los = min(Current_LOS),
         # los on index date is the minimum LOS
         los_dis_rdy = 
           # min(
           case_when(
             Current_LOS < Days_NCTR ~ Inf, # Edge case: where Days_NCTR seems to be incorrect, set to Inf so min will remove this row
             !is.na(Days_NCTR)  ~ (Current_LOS - Days_NCTR), # If recorded NCTR LOS is current - days NCTR
             Days_NCTR > 0 ~ (Current_LOS - Days_NCTR), # If recorded NCTR LOS is current - days NCTR
             Criteria_To_Reside == "N" & !is.na(Days_NCTR) ~ (Current_LOS - Days_NCTR),
             !any(is.na(Date_NCTR) | Days_NCTR[!is.na(Days_NCTR)] > 0) ~ max(Current_LOS, na.rm = TRUE) # IF never NCTR take the maximum recorded LOS
           ),
           # na.rm = TRUE)
         ) %>%
  select(
    Census_Date,
    spell_id,
    los,
    los_dis_rdy,
    Current_LOS,
    Criteria_To_Reside,
    Date_NCTR,
    Days_NCTR
  ) #%>%
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

los_dist <- readRDS("data/dist_split.RDS") %>%
  enframe() %>%
  unnest_wider(value)

sim_drain <- nctr_sum %>%
  filter(Criteria_To_Reside == "Y" & (is.na(Days_NCTR) | Days_NCTR == 0)) %>%
  filter(Census_Date == d_i, 
         spell_id %in% sid_i) %>%
  group_by(spell_id) %>%
  mutate(los = Current_LOS) %>% 
  select(nhs_number,
         age = Person_Age,
         sex = Person_Stated_Gender_Code,
         bed_type = Bed_Type,
         los) %>%
  left_join(select(attr_df, -sex, -age) %>% mutate(nhs_number = as.character(nhs_number)),
            by = join_by(nhs_number == nhs_number)) %>%
  bake(extract_recipe(los_wf), .) %>%
  ungroup() %>%
  mutate(id = 1:n(), 
         leaf = as.character(treeClust::rpart.predict.leaves(extract_fit_engine(los_wf), .))) %>%
  left_join(los_dist, by = join_by(leaf == name)) %>%
  mutate(los_remaining = pmap(
    list(los, meanlog, sdlog),
    ~
      rlnormt(
        n_rep,
        meanlog = ..2,
        sdlog = ..3,
        range = c(..1, Inf)
      ) - ..1
  )) %>%
  dplyr::select(id, los, los_remaining) %>%
  unnest(los_remaining) %>%
  mutate(los_remaining = ifelse(los_remaining < 0, 0, los_remaining)) %>%
  mutate(los_remaining = los_remaining %/% 1) %>%
  group_by(id) %>%
  mutate(rep = 1:n()) %>%
  group_by(rep, day = los_remaining) %>%
  count() %>% # compute CIs/mean over reps
  group_by(day) %>%
  summarise(across(n, list(mean = mean,
                           u95 = {\(x) quantile(x, 0.975)},
                           l95 = {\(x) quantile(x, 0.025)}
  )))
  

  

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
