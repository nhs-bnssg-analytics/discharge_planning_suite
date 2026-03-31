library(fitdistrplus)
library(tidyverse)
library(tidymodels)
library(dbplyr)


# con <- switch(.Platform$OS.type,
#               windows = RODBC::odbcConnect(dsn = "xsw"),
#               unix = xswauth::modelling_sql_area()
# )


con <- DBI::dbConnect(odbc::odbc(), "xsw")

nctr_tbl <- tbl(con,
  in_catalog(
    catalog = "analyst_sql_area",
    schema = "dbo",
    table = "vw_NCTR_Status_Report_Daily_JI"
  )
) 


 pds <- tbl(con,
  in_catalog(
    catalog = "analyst_sql_area",
    schema = "dbo",
    table = "tbl_bnssg_datasets_combined_PDS"
  )
) %>%
  mutate(nhs_number = as.character(Pseudo_NHS_Number))
  
pathway_recodes <- c(
  "Pathway 3 - Other" = "P3",
  "Awaiting confirmation MDT" = "Other",
  "Awaiting referral to SPA" = "Other",
  "Pathway 3 - D2A" = "P3",
  "Pathway 0" = "Other",
  "Pathway 1 - D2A" = "P1",
  "Awaiting confirmation Social" = "Other",
  "Pathway 2 - Other" = "P2",
  "Pathway 2 - D2A" = "P2",
  "Pathway 2" = "P2",
  "Pathway 2  Safeguarding concern" = "P2",
  "Pathway 2   Specialist  eg BIRU" = "P2",
  "Awaiting confirmation Other" = "Other",
  "Pathway 1 - Other" = "P1",
  "Pathway 1" = "P1",
  "P3 / Other Complex Discharge" = "P3",
  "Pathway 3 / Other Complex Discharge" = "P3",
  "Uncoded" = "Other",
  "Repatriation" = "Other",
  "NCTR Null" = "Other",
  "Not Set" = "Other",
  "18a  Infection  bxviii  Standard" = "Other",
  "xviii. Awaiting discharge to a care home but have not had a COVID 19 test (in 48 hrs preceding discharge)." = "Other",
  "15b  Repat  bxv  WGH" = "Other",
  "Meets Criteria to Reside" = "Other",
   "Uncoded" = "Other",
  "Pathway 2" = "P2",
  "Other"= "Other",
  "Pathway 1" = "P1",
  "Repatriation" = "Other",
  "NCTR Null"= "Other",
  "Awaiting referral to SPA" = "Other",
  "Awaiting confirmation MDT" = "Other",
  "Pathway 2 - Other" = "P2",
  "Pathway 1 - D2A" = "P1",
  "Pathway 3 - Other" = "P3",
  "Not Set" = "Other",
  "Pathway 0" = "P0",
  "Pathway 3 / Other Complex Discharge" = "P3",
  "Pathway 3 - D2A" = "P3",
  "Awaiting confirmation Other" = "Other",
  "Pathway 2 - D2A" = "P2",
  "Pathway 1 - Other" = "P1",
  "Awaiting confirmation Social" = "Other"
)


validation_end <- ymd("2024-09-01")
validation_start <- ymd("2023-07-01")
validation_end_excl <- validation_end-ddays(1)

pathway_df <- nctr_tbl %>%
  left_join(
    pds %>% 
      select(pds_nhs_number = Pseudo_NHS_Number, la_pds = Locality_Area) %>% 
      distinct(),
    by = join_by(NHS_Number == pds_nhs_number)
  ) %>%
  filter(Organisation_Site_Code %in% c('RVJ01', 'RA701', 'RA301', 'RA7C2')) %>%
  group_by(CDS_Unique_Identifier) %>%
  window_order(Census_Date) %>%
  mutate(la_raw = last(Local_Authority_grouped)) %>%
  ungroup() %>%
  mutate(
    la_clean = if_else(la_raw == "Other", replace(la_pds, " Area", ""), la_raw),
    la = case_when(
      la_clean == "NOT BNSSG" | la_clean == "Other" | is.na(la_clean) ~ "Other",
      TRUE ~ la_clean
    ),
    la = tolower(la) 
  ) %>%
  filter(between(Census_Date, !!validation_start, !!validation_end_excl)) %>%
  collect() 
# recode LA
nctr_df <- nctr_df %>%
  mutate(la = case_when(
    Local_Authority == "SOUTH GLOUCESTERSHIRE COUNCIL" ~ "south gloucestershire",
    Local_Authority == "South Glos" ~ "south gloucestershire",
    Local_Authority == "NORTH SOMERSET COUNCIL" ~ "north somerset",
    Local_Authority == "North Somerset" ~ "north somerset",
    Local_Authority == "BRISTOL CITY COUNCIL" ~ "bristol",
    Local_Authority == "Bristol" ~ "bristol",
    .default = "Other"
  )) 

pathway_recodes <- c(
  "Pathway 3 - Other" = "P3",
  "Awaiting confirmation MDT" = "Other",
  "Awaiting referral to SPA" = "Other",
  "Pathway 3 - D2A" = "P3",
  "Pathway 0" = "Other",
  "Pathway 1 - D2A" = "P1",
  "Awaiting confirmation Social" = "Other",
  "Pathway 2 - Other" = "P2",
  "Pathway 2 - D2A" = "P2",
  "Pathway 2" = "P2",
  "Pathway 2  Safeguarding concern" = "P2",
  "Pathway 2   Specialist  eg BIRU" = "P2",
  "Awaiting confirmation Other" = "Other",
  "Pathway 1 - Other" = "P1",
  "Pathway 1" = "P1",
  "P3 / Other Complex Discharge" = "P3",
  "Pathway 3 / Other Complex Discharge" = "P3",
  "Uncoded" = "Other",
  "Repatriation" = "Other",
  "NCTR Null" = "Other",
  "Not Set" = "Other",
  "18a  Infection  bxviii  Standard" = "Other",
  "xviii. Awaiting discharge to a care home but have not had a COVID 19 test (in 48 hrs preceding discharge)." = "Other",
  "15b  Repat  bxv  WGH" = "Other",
  "Meets Criteria to Reside" = "Other"
)

nctr_df <- nctr_df %>%
  filter(Person_Stated_Gender_Code %in% 1:2) %>%
  mutate(nhs_number = as.character(NHS_Number),
         nhs_number = if_else(is.na(nhs_number), glue::glue("unknown_{1:n()}"), nhs_number),
         sex = if_else(Person_Stated_Gender_Code == 1, "Male", "Female")) %>%
  filter(Organisation_Site_Code %in% c('RVJ01', 'RA701', 'RA301', 'RA7C2')) %>%
  mutate(
    site = case_when(
      Organisation_Site_Code == 'RVJ01' ~ 'nbt',
      Organisation_Site_Code == 'RA701' ~ 'bri',
      Organisation_Site_Code %in% c('RA301', 'RA7C2') ~ 'weston',
      TRUE ~ 'other'
    ),
    Date_Of_Admission = as.Date(Date_Of_Admission)
  ) %>%
  mutate(
    der_ctr = case_when(
      Criteria_To_Reside == "Y" | is.na(Criteria_To_Reside) ~ TRUE,
      !is.na(Days_NCTR) ~ FALSE,
      !is.na(Date_NCTR) ~ FALSE,
      Criteria_To_Reside == "N" ~ FALSE
    )) %>%
  mutate(
    pathway = recode(
      Current_Delay_Code_Standard,
      !!!pathway_recodes
    ),
    pathway = coalesce(pathway, "Other")
  ) %>%
  mutate(pathway = if_else(
    !pathway %in% c("P1", "P2", "P3", "P3" , "Other"),
    "Other",
    pathway
  )) %>%
  pivot_longer(
    cols = c(site, la),
    names_to = "grp_type",
    values_to = "grp",
  ) %>%
  dplyr::select(
    census_date = Census_Date,
    date_nctr = Date_NCTR,
    admission_date = Date_Of_Admission,
    nhs_number,
    sex,
    age = Person_Age,
    ctr = der_ctr,
    grp,
    spec = Specialty_Code,
    bed_type = Bed_Type,
    pathway
  ) %>%
  ungroup()
