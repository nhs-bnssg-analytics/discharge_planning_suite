library(fitdistrplus)
library(tidyverse)
library(tidymodels)
library(lubridate)
library(dbplyr)
library(dtplyr)
library(RMySQL)
library(fable)
library(fabletools)

con <- switch(.Platform$OS.type,
              windows = DBI::dbConnect(odbc::odbc(), dsn = "xsw"),
              unix = {
                conn_str <- readr::read_lines("/root/sql/sql_connect_string_linux")
                DBI::dbConnect(odbc::odbc(), .connection_string = conn_str)
              })

source("utils/utils.R")
source("utils/theme.R")
source("utils/colour_functions.R")

# plot intermediate results?
plot_int <- TRUE
seed <- FALSE

n_rep <- 1E3
n_days <- 10

nctr_tbl <- tbl(con,
                in_catalog(
                  catalog = "analyst_sql_area",
                  schema = "dbo",
                  table = "NCTR_Status_Report_Daily_DLP"
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
  "NA" = "Other",
  "10a  Pathway 3  bx  Standard" = "P3",
  "7c  Await ext decision bvii  Social Work In Reach" = "Other",
  "7a  Await ext decision bvii  Non CHCFT" = "Other",
  "8a  Pathway 1  bviii  Standard" = "P1",
  "9a  Pathway 2  bix  Standard" = "P2",
  "10b  Pathway 3  bx  CHCFT  8" = "P3",
  "11  Equip   Adaptations  bxi" = "Other",
  "9b  Pathway 2  bix  Specialist  eg BIRU  7" = "P2",
  "7b  Await ext decision bvii  CHCFT" = "Other",
  "19a  No longer required  Non CHCFT" = "Other",
  "3  Await therapy decision  biii" = "Other",
  "4b  Await SPA referral  biv  CHCFT  3" = "Other",
  "4c  Await SPA referral  biv  Await MCA BID  4" = "Other",
  "10c  Pathway 3  bx  Selffunding  9" = "P3",
  "4a  Await SPA referral  biv  Non CHCFT" = "Other",
  "12  Choice  bxii" = "Other",
  "13b  Homeless   no recourse  bxiii  Homeless  11" = "Other",
  "8b  Pathway 1  bviii  CHCFT  5" = "P1",
  "2b  Await med decision  bii  Inpatient specialist " = "Other",
  "19b  No longer required  CHCFT" = "Other",
  "5  Await meds  bv" = "Other",
  "14  Safeguarding  bxiv" = "Other",
  "vii. Awaiting confirmation from community hub/Single Point of Access referral received/actioned - SPA to decide pathway" = "Other",
  "viii. Pathway 1 Destination: Home: awaiting availability of resource for assessment and start of care at home." = "P1",
  "ix. Pathway 2 Community Rehab: awaiting availability of rehabilitation bed in community hospital or other bedded setting" = "P2",
  "x. Pathway 3 New Care Home Admission: awaiting bed in residential or nursing home that is likely to be a permanent placement" = "P3",
  "iv. Awaiting referral to community single point of access." = "Other",
  "xi. Awaiting community equipment and adaptations to housing." = "Other",
  "iii. Awaiting therapy decision - no acute medical/nursing intervention required; requires further rehab in acute hospital" = "Other",
  "xii. Individual/family not in agreement with discharge plans." = "Other",
  "xiii. Homeless/no right of recourse to public funds/no place to discharge to." = "Other",
  "xiv. Safeguarding concern preventing discharge or Court of Protection." = "Other",
  "xv. Repatriation/transfer to another acute trust for specialist treatment or ongoing treatment." = "Other",
  "viii. Pathway 1 Destination: Other Place: awaiting availability of resource for assessment and start of care at home." = "P1",
  "ii. Awaiting a medical decision/intervention including writing the discharge summary." = "Other",
  "x. Pathway 3 Return to Care Home: awaiting bed in a residential or nursing home that is likely to be a permanent placement" = "P3",
  "Pathway 3 - Non D2A - Local Authority" = "P3",
  "Pathway 3 - D2A" = "P3",
  "Pathway 3 - Appendix A" = "P3",
  "Pathway 3 - Non D2A - CHC FT" = "P3",
  "Pathway 1 - D2A" = "P1",
  "Pathway 2 - D2A" = "P2",
  "Awaiting Social Work Allocation for Care Act Assessment" = "Other",
  "Awaiting CHCFT Outcome" = "Other",
  "Await ToC DoC referral/Non BNSSG Equivalent" = "Other",
  "Pathway 3 - Self-funding" = "P3",
  "Pathway 0 - Patient experiencing homelessness" = "P0",
  "Pathway 2 - (Specialist (eg BIRU)" = "P2",
  "Awaiting ToCDoC/Non BNSSG Equivalent Outcome" = "Other",
  "Awaiting other acute MDT Decision" = "Other",
  "Returning to existing level of care at home" = "Other",
  "Awaiting home adaptations" = "Other",
  "Awaiting Social Work Completion of Care Act Assessment" = "Other",
  "Pathway 2 - (Awaiting SSARU)" = "P2",
  "Await CHCFT referral" = "Other",
  "Therapy Only Non-Discharge Dependent" = "Other",
  "Pathway 1 - Self Funding" = "P1",
  "Patient / family concerns about discharge readiness" = "Other",
  "Pathway 2 - (Sub Acute Needs not able to be met by local SSARU)" = "P2",
  "Patient / family concerns about discharge planning" = "Other",
  "Pathway 1 - Non D2A - CHC FT" = "P1",
  "Pathway 1 - Non D2A - Local Authority" = "P1",
  "Pathway 3 - Safeguarding concern" = "P3",
  "Not Set" = "Other",
  "Awaiting Social Work Completion (Duty tasks)" = "Other",
  "2a  Await med decision  bii  Standard" = "Other",
  "16  No plan  bxvi" = "Other",
  "ix. Pathway 2 Care Home: awaiting availability of rehabilitation bed in community hospital or other bedded setting" = "P2",
  "Pathway 1 - Patient experiencing homelessness" = "P1",
  "Await MCA/BID (Required for Discharge) Outcome" = "Other",
  "Pathway 2 - Awaiting Asylum / Immigration" = "P2",
  "Pathway 1 - Non D2A - CHC Complex" = "P1",
  "Awaiting Equipment" = "Other",
  "Pathway 2 - Mental health placement" = "P2",
  "2c  Await med decision  bii  Mental Health  2" = "Other",
  "v. Awaiting medicines to take home." = "Other",
  "i. Declared as not meeting criteria to reside at AM ward round then later in day meets criteria to reside so discharge stopped" = "Other",
  "8c  Pathway 1  bviii  Selffunding  6" = "P1",
  "xvii. Remains in hospital to avoid spread of non-Covid 19 infectious disease - no other suitable location to discharge to" = "Other",
  "xvi. Awaiting Diagnostic test." = "Other",
  "viii. Pathway 1 Destination: Hotel: awaiting availability of resource for assessment and start of care at home.," = "P1",
  "vi. Awaiting transport." = "Other",
  "ix. Pathway 2 Destination: Hospice: awaiting availability of rehabilitation bed in community hospital or other bedded setting." = "P2",
  "15a  Repat  bxv  Other Trusts" = "Other",
  "Awaiting CHC Complex Outcome" = "Other",
  "Returning to Existing level of care in a Care Home" = "Other",
  "Pathway 3 - Unable to discharge to external provider due to infection" = "P3",
  "Formal therapy or care no longer required" = "Other",
  "Pathway 3  Non D2A  Local Authority" = "P3",
  "Awaiting ToCDoC Non BNSSG Equivalent Outcome" = "Other",
  "Pathway 2  D2A" = "P2",
  "Meets Criteria to Reside" = "Other",
  "Pathway 1  Non D2A  Local Authority" = "P1",
  "Pathway 1  Patient experiencing homelessness" = "P1",
  "Awaiting Social Work Completion of Care Act Assess" = "Other",
  "Pathway 3  Non D2A  CHC FT" = "P3",
  "Pathway 1  D2A" = "P1",
  "Pathway 3  Non D2A  CHC Complex" = "P3",
  "Pathway 3  D2A" = "P3",
  "Awaiting Social Work Allocation for Care Act Asses" = "Other",
  "Await ToC DoC referral Non BNSSG Equivalent" = "Other",
  "Pathway 1  Non D2A  CHC FT" = "P1",
  "Await MCA BID  Required for Discharge Outcome" = "Other",
  "Pathway 3  Selffunding" = "P3",
  "Awaiting Social Work Completion  Duty tasks" = "Other",
  "Patient   family concerns about discharge planning" = "Other",
  "Pathway 1 - Safeguarding concern" = "P1",
  "Pathway 3  Appendix A" = "P3",
  "Pathway 1  Self Funding" = "P1",
  "Pathway 2  Unable to discharge to external provide" = "P2",
  "Pathway 0  Patient experiencing homelessness" = "P0",
  "Patient   family concerns about discharge readines" = "Other",
  "Pathway 2 - (Hospice)" = "P2",
  "Pathway 0 - Unable to discharge to external provider due to infection" = "P0",
  "Pathway 0  Safeguarding concern" = "P0",
  "Pathway 2  Hospice" = "P2",
  "18a  Infection  bxviii  Standard" = "Other",
  "18b  Infection  bxviii  Req isolation  infected in" = "Other",
  "Pathway 2  Specialist  eg BIRU" = "P2",
  "Pathway 0 - Safeguarding concern" = "P0",
  "Await CHC Complex referral" = "Other",
  "Pathway 3 - Awaiting Asylum / Immigration" = "P3",
  "Pathway 2  Mental health placement" = "P2",
  "Pathway 1  Safeguarding concern" = "P1",
  "13a  Homeless   no recourse  bxiii  Asylum Immigra" = "Other",
  "Pathway 3 - Non D2A - CHC Complex" = "P3",
  "Pathway 1  Non D2A  CHC Complex" = "P1",
  "Pathway 3  Safeguarding concern" = "P3",
  "Pathway 1 - Awaiting Asylum / Immigration" = "P1",
  "Therapy Only NonDischarge Dependent" = "Other",
  "Pathway 2  Awaiting SSARU" = "P2",
  "xviii. Awaiting discharge to a care home but have not had a COVID 19 test (in 48 hrs preceding discharge)." = "Other",
  "Pathway 2  Safeguarding concern" = "P2",
  "15b  Repat  bxv  WGH" = "Other",
  "Pathway 3  Unable to discharge to external provide" = "P3",
  "Pathway 0  Awaiting Asylum Immigration" = "P0",
  "Pathway 0 - Awaiting Asylum/Immigration" = "P0",
  "Pathway 1  Unable to discharge to external provide" = "P1",
  "Pathway 0  Unable to discharge to external provide" = "P0",
  "Pathway 1  Awaiting Asylum   Immigration" = "P1",
  "Pathway 2 - Safeguarding concern" = "P2",
  "Pathway 2  Sub Acute Needs not able to be met by " = "P2",
  "Pathway 3  Unable to discharge to external provider due to infection" = "P3",
  "Pathway 2  Awaiting SSARU" = "P2",
  "Await MCA/BID Required for Discharge Outcome" = "Other",
  "Pathway 2  Sub Acute Needs not able to be met by local SSARU" = "P2",
  "Pathway 2  Unable to discharge to external provider due to infection" = "P2",
  "Awaiting Social Work Completion Duty tasks" = "Other",
  "Pathway 2  Specialist eg BIRU" = "P2",
  "6  Await transport  bvi" = "Other",
  "xv. Repatriation/transfer to another acute trust f" = "Other",
  "vii. Awaiting confirmation from community hub/Sing" = "Other",
  "viii. Pathway 1 Destination: Home: awaiting availa" = "P1",
  "iii. Awaiting therapy decision - no acute medical/" = "Other",
  "iv. Awaiting referral to community single point of" = "Other",
  "ii. Awaiting a medical decision/intervention inclu" = "Other",
  "ix. Pathway 2 Community Rehab: awaiting availabili" = "P2",
  "x. Pathway 3 New Care Home Admission: awaiting bed" = "P3",
  "ix. Pathway 2 Care Home: awaiting availability of " = "P2",
  "i. Declared as not meeting criteria to reside at A" = "Other",
  "x. Pathway 3 Return to Care Home: awaiting bed in " = "P3",
  "xiii. Homeless/no right of recourse to public fund" = "Other",
  "xiv. Safeguarding concern preventing discharge or " = "Other",
  "xi. Awaiting community equipment and adaptations t" = "Other",
  "viii. Pathway 1 Destination: Other Place: awaiting" = "P1",
  "xii. Individual/family not in agreement with disch" = "Other",
  "xvii. Remains in hospital to avoid spread of non-C" = "Other"
)

la_recode = c(
  "North Somerset" = "North Somerset",
  "Somerset" = "Other",
  "Bristol" = "Bristol",
  "South Glos" = "South Gloucestershire",
  "Other" = "Other",
  "BaNES" = "Other",
  "SOUTH GLOUCESTERSHIRE COUNCIL" = "South Gloucestershire",
  "BRISTOL CITY COUNCIL" = "Bristol",
  "WILTSHIRE COUNCIL" = "Other",
  "NORTH SOMERSET COUNCIL"= "North Somerset",
  "GLOUCESTERSHIRE COUNTY COUNCIL" = "Other",
  "SWINDON BOROUGH COUNCIL" = "Other",
  "DEVON COUNTY COUNCIL" = "Other",
  "SOMERSET COUNTY COUNCIL" = "Other",
  "SOMERSET COUNCIL" = "Other",
  "BATH AND NORTH EAST SOMERSET COUNCIL" = "Other",
  "WAKEFIELD METROPOLITAN DISTRICT COUNCIL" = "Other",
  "NORFOLK COUNTY COUNCIL" = "Other",
  "BCP COUNCIL" = "Other",
  "CORNWALL COUNCIL" = "Other",
  "BLAENAU GWENT UA" = "Other",
  "HERTFORDSHIRE COUNTY COUNCIL" = "Other",
  "TORBAY COUNCIL" = "Other",
  "DORSET COUNCIL" = "Other",
  "WORCESTERSHIRE COUNTY COUNCIL" = "Other",
  "DURHAM COUNTY COUNCIL" = "Other",
  "ISLE OF WIGHT COUNCIL" = "Other",
  "STOCKPORT METROPOLITAN BOROUGH COUNCIL" = "Other",
  "MONMOUTHSHIRE UA" = "Other",
  "LONDON BOROUGH OF WALTHAM FOREST" = "Other",
  "PEMBROKESHIRE UA" = "Other",
  "HEREFORDSHIRE COUNCIL" = "Other",
  "BRIDGEND UA" = "Other",
  "LONDON BOROUGH OF REDBRIDGE" = "Other",
  "CARDIFF UA" = "Other",
  "NEATH PORT TALBOT UA" = "Other",
  "SWANSEA UA" = "Other",
  "SHEFFIELD CITY COUNCIL" = "Other",
  "OXFORDSHIRE COUNTY COUNCIL" = "Other",
  "TORFAEN UA" = "Other",
  "NEWPORT UA" = "Other",
  "BUCKINGHAMSHIRE COUNCIL" = "Other",
  "LONDON BOROUGH OF LAMBETH" = "Other",
  "BOLTON METROPOLITAN BOROUGH COUNCIL" = "Other",
  "CITY OF WOLVERHAMPTON COUNCIL" = "Other",
  "MEDWAY COUNCIL" = "Other",
  "WARWICKSHIRE COUNTY COUNCIL" = "Other",
  "VALE OF GLAMORGAN UA" = "Other",
  "WEST SUSSEX COUNTY COUNCIL" = "Other",
  "SURREY COUNTY COUNCIL" = "Other",
  "BEDFORD BOROUGH COUNCIL" = "Other",
  "MERTHYR TYDFIL UA" = "Other",
  "MANCHESTER CITY COUNCIL" = "Other",
  "CARMARTHENSHIRE UA" = "Other",
  "LEICESTERSHIRE COUNTY COUNCIL" = "Other",
  "RHONDDA, CYNON, TAFF UA" = "Other",
  "COVENTRY CITY COUNCIL" = "Other",
  "LUTON BOROUGH COUNCIL" = "Other",
  "NORTHUMBERLAND COUNTY COUNCIL" = "Other",
  "WIRRAL BOROUGH COUNCIL" = "Other",
  "DERBYSHIRE COUNTY COUNCIL" = "Other",
  "HAMPSHIRE COUNTY COUNCIL" = "Other",
  "MILTON KEYNES COUNCIL" = "Other",
  "PLYMOUTH CITY COUNCIL" = "Other",
  "CAERPHILLY UA" = "Other",
  "LANCASHIRE COUNTY COUNCIL" = "Other",
  "ISLE OF ANGLESEY UA" = "Other",
  "CALDERDALE METROPOLITAN BOROUGH COUNCIL" = "Other",
  "LONDON BOROUGH OF CAMDEN" = "Other",
  "NORTH EAST LINCOLNSHIRE COUNCIL" = "Other",
  "NOTTINGHAMSHIRE COUNTY COUNCIL" = "Other",
  "SOLIHULL METROPOLITAN BOROUGH COUNCIL" = "Other",
  "KENT COUNTY COUNCIL" = "Other",
  "POWYS UA" = "Other",
  "CHESHIRE WEST AND CHESTER COUNCIL" = "Other",
  "LONDON BOROUGH OF HARINGEY" = "Other",
  "DARLINGTON BOROUGH COUNCIL" = "Other",
  "NORTH YORKSHIRE COUNCIL" = "Other",
  "DUDLEY METROPOLITAN BOROUGH COUNCIL" = "Other",
  "CHESHIRE EAST COUNCIL" = "Other",
  "PORTSMOUTH CITY COUNCIL" = "Other",
  "LONDON BOROUGH OF ISLINGTON" = "Other",
  "BLACKPOOL BOROUGH COUNCIL" = "Other",
  "WALSALL METROPOLITAN BOROUGH COUNCIL" = "Other",
  "STAFFORDSHIRE COUNTY COUNCIL" = "Other",
  "CEREDIGION UA" = "Other",
  "WOKINGHAM BOROUGH COUNCIL" = "Other",
  "LIVERPOOL CITY COUNCIL" = "Other",
  "BIRMINGHAM CITY COUNCIL" = "Other",
  "LINCOLNSHIRE COUNTY COUNCIL" = "Other",
  "CAMBRIDGESHIRE COUNTY COUNCIL" = "Other",
  "EAST RIDING OF YORKSHIRE COUNCIL" = "Other",
  "LONDON BOROUGH OF HARROW" = "Other",
  "BURY METROPOLITAN BOROUGH COUNCIL" = "Other",
  "CITY OF WESTMINSTER" = "Other",
  "SOUTHAMPTON CITY COUNCIL" = "Other",
  "SANDWELL METROPOLITAN BOROUGH COUNCIL" = "Other",
  "SHROPSHIRE COUNCIL" = "Other",
  "LONDON BOROUGH OF BRENT" = "Other",
  "WARRINGTON BOROUGH COUNCIL" = "Other",
  "LONDON BOROUGH OF HOUNSLOW" = "Other",
  "SOUTHEND-ON-SEA BOROUGH COUNCIL" = "Other",
  "ROYAL BOROUGH OF KENSINGTON AND CHELSEA" = "Other",
  "WEST BERKSHIRE COUNCIL" = "Other",
  "BRIGHTON AND HOVE CITY COUNCIL" = "Other",
  "ROYAL BOROUGH OF KINGSTON UPON THAMES" = "Other",
  "LONDON BOROUGH OF EALING" = "Other",
  "LONDON BOROUGH OF RICHMOND UPON THAMES" = "Other",
  "LONDON BOROUGH OF BARNET" = "Other",
  "KIRKLEES COUNCIL" = "Other",
  "SLOUGH BOROUGH COUNCIL" = "Other",
  "READING BOROUGH COUNCIL" = "Other",
  "EAST SUSSEX COUNTY COUNCIL" = "Other",
  "LONDON BOROUGH OF LEWISHAM" = "Other",
  "LONDON BOROUGH OF HACKNEY" = "Other",
  "SUFFOLK COUNTY COUNCIL" = "Other",
  "SOUTH TYNESIDE COUNCIL" = "Other",
  "CITY OF BRADFORD METROPOLITAN DISTRICT COUNCIL" = "Other",
  "ESSEX COUNTY COUNCIL" = "Other",
  "SALFORD CITY COUNCIL" = "Other",
  "TELFORD & WREKIN COUNCIL" = "Other",
  "ST HELENS COUNCIL" = "Other",
  "CONWY UA" = "Other",
  "OLDHAM METROPOLITAN BOROUGH COUNCIL" = "Other",
  "LONDON BOROUGH OF TOWER HAMLETS" = "Other",
  "WEST NORTHAMPTONSHIRE COUNCIL" = "Other",
  "LONDON BOROUGH OF BARKING AND DAGENHAM" = "Other",
  "GATESHEAD METROPOLITAN BOROUGH COUNCIL" = "Other",
  "SEFTON METROPOLITAN BOROUGH COUNCIL" = "Other",
  "NOTTINGHAM CITY COUNCIL" = "Other",
  "GWYNEDD UA" = "Other",
  "NORTH NORTHAMPTONSHIRE COUNCIL" = "Other",
  "LONDON BOROUGH OF CROYDON" = "Other",
  "HARTLEPOOL BOROUGH COUNCIL" = "Other",
  "PETERBOROUGH CITY COUNCIL" = "Other",
  "ROYAL BOROUGH OF WINDSOR AND MAIDENHEAD" = "Other",
  "SUNDERLAND CITY COUNCIL" = "Other",
  "CITY OF LONDON CORPORATION" = "Other",
  "BRACKNELL FOREST COUNCIL" = "Other",
  "BLACKBURN WITH DARWEN BOROUGH COUNCIL" = "Other",
  "WREXHAM UA" = "Other",
  "NEWCASTLE CITY COUNCIL" = "Other",
  "CUMBRIA COUNTY COUNCIL" = "Other",
  "DONCASTER METROPOLITAN BOROUGH COUNCIL" = "Other",
  "REDCAR AND CLEVELAND BOROUGH COUNCIL" = "Other",
  "LONDON BOROUGH OF BROMLEY" = "Other",
  "HULL CITY COUNCIL" = "Other",
  "LEICESTER CITY COUNCIL" = "Other",
  "ROTHERHAM METROPOLITAN BOROUGH COUNCIL" = "Other",
  "LEEDS CITY COUNCIL" = "Other",
  "RUTLAND COUNTY COUNCIL" = "Other",
  "TAMESIDE METROPOLITAN BOROUGH COUNCIL" = "Other",
  "LONDON BOROUGH OF NEWHAM" = "Other",
  "LONDON BOROUGH OF MERTON" = "Other",
  "LONDON BOROUGH OF SOUTHWARK" = "Other",
  "THURROCK COUNCIL" = "Other",
  "NORTH TYNESIDE COUNCIL" = "Other",
  "LONDON BOROUGH OF WANDSWORTH" = "Other",
  "WIGAN METROPOLITAN BOROUGH COUNCIL" = "Other",
  "LONDON BOROUGH OF HAMMERSMITH & FULHAM" = "Other",
  "DERBY CITY COUNCIL" = "Other",
  "TRAFFORD METROPOLITAN BOROUGH COUNCIL" = "Other",
  "ROYAL BOROUGH OF GREENWICH" = "Other",
  "LONDON BOROUGH OF HILLINGDON" = "Other",
  "MIDDLESBROUGH BOROUGH COUNCIL" = "Other",
  "LONDON BOROUGH OF BEXLEY" = "Other",
  "BARNSLEY METROPOLITAN BOROUGH COUNCIL" = "Other",
  "COUNCIL OF THE ISLES OF SCILLY" = "Other",
  "LONDON BOROUGH OF SUTTON" = "Other",
  "STOCKTON-ON-TEES BOROUGH COUNCIL" = "Other",
  "STOKE-ON-TRENT CITY COUNCIL" = "Other",
  "HALTON BOROUGH COUNCIL" = "Other",
  "SOUTHEND-ON-SEA CITY COUNCIL" = "Other",
  "LONDON BOROUGH OF ENFIELD" = "Other",
  "ROCHDALE METROPOLITAN BOROUGH COUNCIL" = "Other",
  "FLINTSHIRE UA" = "Other",
  "KNOWSLEY METROPOLITAN BOROUGH COUNCIL" = "Other",
  "DENBIGHSHIRE UA" = "Other",
  "CITY OF YORK COUNCIL" = "Other"
)

nctr_df <- nctr_tbl %>%
  left_join(
    pds %>% 
      select(pds_nhs_number = Pseudo_NHS_Number, la_pds = Locality_Area) %>% 
      distinct(),
    by = join_by(NHS_Number == pds_nhs_number)
  ) %>%
  filter(Organisation_Site_Code %in% c('RVJ01', 'RA701', 'RA301', 'RA7C2', 'J4C3O')) %>%
  mutate(
    site = case_when(
      Organisation_Site_Code %in% c('RVJ01', 'J4C3O') ~ 'nbt',
      Organisation_Site_Code == 'RA701' ~ 'bri',
      Organisation_Site_Code %in% c('RA301', 'RA7C2') ~ 'weston',
      TRUE ~ 'other'
    )) %>%
  filter(!is.na(NHS_Number)) %>% # cant join attributes otherwise
  window_order(Census_Date, .by = CDS_Unique_Identifier) %>%
  collect()  %>%
  mutate(la = recode(Local_Authority, !!!la_recode)) %>%
  mutate(la = last(la), .by = CDS_Unique_Identifier) %>%
  mutate(la_pds = stringr::str_replace(la_pds, " Area", "")) %>%
  mutate(la_pds = case_when(is.na(la_pds) ~ "Other", la_pds == "NOT BNSSG" ~ "Other", .default = la_pds)) %>%
  ungroup() %>%
  mutate(
    la = case_when(
      la_pds != "Other" | la == "Other" ~ la_pds,
      is.na(la) ~ "Other",
      TRUE ~ la
    ),
    la = tolower(la) 
  )


# max census date
max_date <- nctr_df %>%
  filter(!is.na(NHS_Number)) %>%
  filter(site %in% c("bri","weston", "nbt")) %>%
  slice_max(order_by = Census_Date, by = site) %>%
  pull(Census_Date) %>%
  as.Date() %>%
  min() 



# NCTR data summary

nctr_sum <- nctr_df %>%
  filter(Person_Stated_Gender_Code %in% 1:2) %>%
  mutate(nhs_number = as.character(NHS_Number),
         nhs_number = if_else(is.na(nhs_number), glue::glue("unknown_{1:n()}"), nhs_number),
         sex = if_else(Person_Stated_Gender_Code == 1, "Male", "Female")) %>%
  mutate(
    Date_Of_Admission = as.Date(Date_Of_Admission)
  ) %>%
  group_by(site) %>%
  filter(Census_Date == max_date) %>%
  ungroup() %>%
  mutate(
    der_los = (as.Date(Census_Date) - as.Date(Date_Of_Admission))/ddays(1),
    der_ctr = case_when(
      Criteria_To_Reside == "Y" | is.na(Criteria_To_Reside) ~ TRUE,
      !is.na(Days_NCTR) ~ FALSE,
      !is.na(Date_NCTR) ~ FALSE,
      Criteria_To_Reside == "N" ~ FALSE
    )) %>%
  mutate(report_date = max_date) %>%
  mutate(los = (report_date - Date_Of_Admission) / ddays(1)) %>%
  mutate(
    pathway = recode(
      Current_Delay_Code,
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
  # we only count patients in 'other' LA when considering accute occupancy
  filter(grp != "other") %>%
  dplyr::select(
    report_date,
    nhs_number,
    sex,
    age = Person_Age,
    ctr = der_ctr,
    grp,
    spec = Specialty_Code,
    bed_type = Bed_Type,
    los = der_los,
    pathway
  ) %>%
  ungroup()

# (DEPRECATED) report start (i.e. date we start reporting new D2A - 1 day after max_date)
# report start (i.e. date we start reporting new D2A - day of latest census)
report_start <- max_date
report_end <- report_start + ddays(n_days)



attr_df <- DBI::dbGetQuery(con,  "select * from (
select a.*, ROW_NUMBER() over (partition by nhs_number order by attribute_period desc) rn from
[MODELLING_SQL_AREA].[dbo].[New_Cambridge_Score] a) b where b.rn = 1")


nctr_df <- nctr_df %>%
  filter(Census_Date <= report_start)

source("code_admits_fcast.R")
source("code_new_admits.R")
source("code_curr_admits.R")

if(plot_int){
  bind_rows(df_curr_admits, df_new_admit) %>%
    group_by(grp, day, pathway, source) %>%
    summarise(across(count, list(
      mean = mean,
      u85 = {\(x) quantile(x, 0.925)},
      l85 = {\(x) quantile(x, 0.075)}
    ))) %>% 
    filter(between(day, 1, n_days))  %>%
    ggplot(aes(x = day, y = count_mean, fill = source)) +
    geom_col() +
    facet_grid(pathway ~ grp, scales = "free") +
    labs(title = "New additions to D2A queue, by forecast source")
}


df_pred <- bind_rows(df_curr_admits, df_new_admit) %>%
  group_by(grp, rep, day, pathway) %>%
  summarise(n = sum(count)) %>% # aggregate over source (current/new admits)
  group_by(grp, day, pathway) %>% # compute CIs/mean over reps
  summarise(across(n, list(mean = mean,
                           u85 = {\(x) quantile(x, 0.925)},
                           l85 = {\(x) quantile(x, 0.075)}
  ))) %>% 
  filter(day <= n_days) %>%
  rename(n = n_mean,
         u85 = n_u85,
         l85 = n_l85)

# Now simulate the queue evolution
source("code_queue_sim.R")


# dataset for plotting (and storing on SQL)

plot_df_pred <- df_pred %>%
  mutate(ctr = "Y",
         source = "model_pred",
         report_date = max_date) %>%
  pivot_longer(cols = c(n, u85, l85),
               names_to = "metric",
               values_to = "value")

plot_df_current <- nctr_sum %>%
  filter(!is.na(nhs_number), !is.na(ctr)) %>%
  group_by(grp, ctr, pathway) %>%
  count() %>%
  mutate(ctr = if_else(ctr, "Y", "N"),
         source = "current_ctr_data",
         report_date = max_date,
         day = 0) %>%
  pivot_longer(cols = c(n),
               names_to = "metric",
               values_to = "value")

plot_df_fcast <- df_admit_fcast %>% dplyr::select(-date)


plot_df <- bind_rows(plot_df_pred, 
                     plot_df_current,
                     plot_df_queue_sim,
                     plot_df_fcast) %>%
  mutate(pathway = factor(pathway, levels = (c("Other", "P1", "P2", "P3"))),
         report_date = as.character(report_date)) # convert date to character because RODBC/R/SQL can't handle writing this in a consistent way

# # create the table
# con<-RODBC::odbcDriverConnect("driver={SQL Server};\n  server=Xsw-00-ash01;\n  trusted_connection=true")
# RODBC::sqlQuery(con,
#                 query = 'USE modelling_sql_area CREATE TABLE dbo.discharge_pathway_projections
#                 (
#                 "site" varchar(255),
#                 "pathway" varchar(255),
#                 "day" float,
#                 "ctr" varchar(255),
#                 "source" varchar(255),
#                 "report_date" varchar(255),
#                 "metric" varchar(255),
#                 "value" float)'
#                 )
# RODBC::odbcClose(con)


# # change con to write to modelling sql area
# con <- switch(
#   .Platform$OS.type,
#   windows = DBI::dbConnect(odbc::odbc(), dsn = "xsw"),
#   unix = {
#     conn_str <- readr::read_lines("/root/sql/sql_modelling_connect_string_linux")
#     DBI::dbConnect(odbc::odbc(), .connection_string = conn_str)
#   }
# )
# 
# 
# dbWriteTable(
#   con,
#   name = Id(db = "modelling_SQL_AREA", schema = "dbo", table = "discharge_pathway_projections"),
#   value = plot_df,
#   overwrite = TRUE
# )


con <- switch(.Platform$OS.type,
              windows = {
                "driver={SQL Server};server=Xsw-00-ash01;
                 database=MODELLING_SQL_AREA;
                 trusted_connection=true" |>
                  RODBC::odbcDriverConnect()
              },
              unix = {
                "/root/sql/sql_modelling_connect_string_linux" |>
                  readr::read_lines() |>
                  RODBC::odbcDriverConnect()
              }
)
# delete old data
query_delete <- "DELETE FROM MODELLING_SQL_AREA.dbo.discharge_pathway_projections"
RODBC::sqlQuery(con, query_delete)
RODBC::sqlSave(con,
               plot_df,
               tablename = 'dbo.discharge_pathway_projections',
               rownames = FALSE,
               append = TRUE)


# Write to ICS MySQL db

host <- Sys.getenv("DB_HOST")
dbname <- Sys.getenv("DB_NAME")
user <- Sys.getenv("DB_USER")
password <- Sys.getenv("DB_CRED")

# Create the connection
conn <- DBI::dbConnect(DBI::dbDriver("MySQL"),
                  dbname = dbname,
                  host = host,
                  port = 3306,
                  user = user,
                  password=password)


# delete old data
query_delete <- str_c("DELETE FROM discharge_pathway_projections")
DBI::dbGetQuery(conn, query_delete)
DBI::dbWriteTable(conn, "discharge_pathway_projections", value = plot_df, overwrite = TRUE, row.names = FALSE)


message("==========================================")
message(paste("Forecast completed successfully at", Sys.time()))
message("==========================================")
