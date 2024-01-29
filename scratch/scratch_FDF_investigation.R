foo <- nctr_df %>%
  mutate(spec = factor(ACTIVITY_TREATMENT_FUNCTION_CODE)) %>%
  # filter for our main sites / perhaps I shouldn't do this?
  filter(
    ORGANISATION_SITE_IDENTIFIER_OF_TREATMENT %in% c('RVJ01', 'RA701', 'RA301', 'RA7C2')
  ) %>%
  # filter for CTR, we wont predict the NCTR outcome for those already NCTR/on a queue
  # filter(Criteria_To_Reside == "N") %>%
  mutate(
    site = case_when(
      ORGANISATION_SITE_IDENTIFIER_OF_TREATMENT == 'RVJ01' ~ 'nbt',
      ORGANISATION_SITE_IDENTIFIER_OF_TREATMENT == 'RA701' ~ 'bri',
      ORGANISATION_SITE_IDENTIFIER_OF_TREATMENT %in% c('RA301', 'RA7C2') ~ 'weston',
      TRUE ~ 'other'
    ),
    Date_Of_Admission = as.Date(START_DATE_HOSPITAL_PROVIDER_SPELL),
    REPORTING_PERIOD_START_DATE = as.Date(parse_date_time(
      REPORTING_PERIOD_START_DATE, orders = c("ymd", "dmy HMS")
    ))
  )


bar <- foo %>%
group_by(HOSPITAL_PROVIDER_SPELL_IDENTIFIER,
         Date_Of_Admission) %>% filter(REPORTING_PERIOD_START_DATE == min(REPORTING_PERIOD_START_DATE)) %>% select(
           HOSPITAL_PROVIDER_SPELL_IDENTIFIER,
           Date_Of_Admission,
           REPORTING_PERIOD_START_DATE
         ) %>% mutate(diff = REPORTING_PERIOD_START_DATE - Date_Of_Admission) 


nctr_df %>%
  group_by(HOSPITAL_PROVIDER_SPELL_IDENTIFIER) %>% 
  filter(REPORTING_PERIOD_START_DATE == max(REPORTING_PERIOD_START_DATE)) %>% 
  pull(DISCHARGE_PATHWAY) %>%
  table()



foo %>%
  filter(REPORTING_PERIOD_START_DATE == max(REPORTING_PERIOD_START_DATE, na.rm = TRUE)) %>%
  pull(DISCHARGE_PATHWAY) %>%
  table()


foo <- nctr_df %>%
  mutate(spec = factor(ACTIVITY_TREATMENT_FUNCTION_CODE)) %>%
  # filter for our main sites / perhaps I shouldn't do this?
  filter(
    ORGANISATION_SITE_IDENTIFIER_OF_TREATMENT %in% c('RVJ01', 'RA701', 'RA301', 'RA7C2')
  ) %>%
  # filter for CTR, we wont predict the NCTR outcome for those already NCTR/on a queue
  # filter(Criteria_To_Reside == "N") %>%
  mutate(
    site = case_when(
      ORGANISATION_SITE_IDENTIFIER_OF_TREATMENT == 'RVJ01' ~ 'nbt',
      ORGANISATION_SITE_IDENTIFIER_OF_TREATMENT == 'RA701' ~ 'bri',
      ORGANISATION_SITE_IDENTIFIER_OF_TREATMENT %in% c('RA301', 'RA7C2') ~ 'weston',
      TRUE ~ 'other'
    ),
    Date_Of_Admission = as.Date(START_DATE_HOSPITAL_PROVIDER_SPELL),
    REPORTING_PERIOD_START_DATE = as.Date(parse_date_time(
      REPORTING_PERIOD_START_DATE, orders = c("ymd", "dmy HMS")
    )),
    REPORTING_PERIOD_END_DATE = as.Date(parse_date_time(
      REPORTING_PERIOD_END_DATE, orders = c("ymd", "dmy HMS")
    ))
  ) %>%
  group_by(site) %>%
  filter(REPORTING_PERIOD_START_DATE == max(REPORTING_PERIOD_START_DATE)) %>%
  # distinct(HOSPITAL_PROVIDER_SPELL_IDENTIFIER) %>%
  ungroup() %>%
  distinct() %>%
  # mutate(report_date = max(REPORTING_PERIOD_START_DATE)) %>%
  mutate(los = (REPORTING_PERIOD_START_DATE - Date_Of_Admission) / ddays(1)) %>%
  mutate(
    pathway = recode(
      DISCHARGE_PATHWAY,
      "iv" = "Other",
      "z" = "Other",
      "1C" = "P1",
      "ixc" = "Other",
      "viii" = "Other",
      "x" = "Other",
      "0A" = "P0",
      "v" = "Other",
      "vii" = "Other",
      "ii" = "Other",
      "iii" = "Other",
      "xv" = "Other",
      "xr" = "Other",
      "xii" = "Other",
      "ix" = "Other",
      "3J" = "P3",
      "xi" = "Other",
      "viiio" = "Other",
      "xiii" = "Other",
      "xvii" = "Other",
      "i" = "Other",
      "2G" = "P2",
      "2H" = "P2",
      "xviii" = "Other",
      "99" = "Other",
      "xiv" = "Other",
      "vi" = "Other",
      "xvi" = "Other",
      "1E" = "P1",
      "1A" = "P1",
      "3B" = "P3",
      "2E" = "P2",
      "0B" = "P0",
      "viiih" = "Other",
      "ixh" = "Other",
      "2F" = "P2",
      "1D" = "P1"
    ),
    pathway = coalesce(pathway, "Other")
  ) %>%
  dplyr::select(
    report_date = REPORTING_PERIOD_START_DATE,
    # this is a workaround for bad DQ / census date is not always consistent at time of running
    nhs_number = Pseudo_NHS_NUMBER,
    ctr = CRITERIA_TO_RESIDE,
    site,
    spec,
    # bed_type = Bed_Type,
    los,
    DISCHARGE_PATHWAY,
    pathway
  ) %>%
  ungroup()




foo <- nctr_df %>%
  mutate(spec = factor(ACTIVITY_TREATMENT_FUNCTION_CODE)) %>%
  # filter for our main sites / perhaps I shouldn't do this?
  filter(
    ORGANISATION_SITE_IDENTIFIER_OF_TREATMENT %in% c('RVJ01', 'RA701', 'RA301', 'RA7C2')
  ) %>%
  # filter for CTR, we wont predict the NCTR outcome for those already NCTR/on a queue
  # filter(Criteria_To_Reside == "N") %>%
  mutate(
    site = case_when(
      ORGANISATION_SITE_IDENTIFIER_OF_TREATMENT == 'RVJ01' ~ 'nbt',
      ORGANISATION_SITE_IDENTIFIER_OF_TREATMENT == 'RA701' ~ 'bri',
      ORGANISATION_SITE_IDENTIFIER_OF_TREATMENT %in% c('RA301', 'RA7C2') ~ 'weston',
      TRUE ~ 'other'
    ),
    Date_Of_Admission = as.Date(START_DATE_HOSPITAL_PROVIDER_SPELL),
    REPORTING_PERIOD_START_DATE = as.Date(parse_date_time(
      REPORTING_PERIOD_START_DATE, orders = c("ymd", "dmy HMS")
    )),
    REPORTING_PERIOD_END_DATE = as.Date(parse_date_time(
      REPORTING_PERIOD_END_DATE, orders = c("ymd", "dmy HMS")
    ))
  ) 


rep_dates <- foo$REPORTING_PERIOD_START_DATE %>% unique() %>% sort()



# what discharge pathways are recorded on each day?
bar <- map(rep_dates, ~{foo %>%
    filter(REPORTING_PERIOD_END_DATE == .x) %>%
    pull(DISCHARGE_PATHWAY) %>%
    table(useNA = "ifany")}) %>%
    reduce(rbind) %>%
    as.data.frame()
