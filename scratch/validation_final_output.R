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

# con <- RODBC::odbcDriverConnect(readr::read_lines("/root/sql/sql_connect_string_linux"))

source("utils/utils.R")
source("utils/theme.R")
source("utils/colour_functions.R")

validation_end <- ymd("2024-09-01")
validation_start <- ymd("2023-07-01")
start_date <- validation_end - dweeks(13) 


plot_int <- FALSE

n_rep <- 1E3

run_date <- today()
n_days <- 10

# latest nctr data
nctr_df_full <-
  RODBC::sqlQuery(
    con,
    glue::glue("SELECT
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
  FROM Analyst_SQL_Area.dbo.vw_NCTR_Status_Report_Daily_JI
  Where Census_Date <= '{run_date}'"
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


# max census date

max_date <- nctr_df_full %>%
  filter(!is.na(NHS_Number)) %>%
  filter(Organisation_Site_Code %in% c('RVJ01', 'RA701', 'RA301', 'RA7C2')) %>%
  pull(Census_Date) %>%
  max()

# attr_df <-
#   RODBC::sqlQuery(
#     con,
#     "select * from (
# select a.*, ROW_NUMBER() over (partition by nhs_number order by attribute_period desc) rn from
# [MODELLING_SQL_AREA].[dbo].[New_Cambridge_Score] a) b where b.rn = 1"
#   )
# 
# saveRDS(attr_df, "data/attr_df.RDS")
attr_df <- readRDS("data/attr_df.RDS")

# validation testing dates

dates <- nctr_df_full %>%
  filter(Census_Date >= start_date,
         Census_Date < validation_end,
         Census_Date > ymd("2023-07-01"),
         Census_Date < max(Census_Date) - ddays(n_days),
         # Data not submitted for UHBW on this day
         Census_Date != ymd("2024-07-17"),
         # remove dates near Christmas
         abs(lubridate::interval(Census_Date, ymd("2023-12-25"))/ddays(1)) > 15
  ) %>%
  pull(Census_Date) %>%
  unique()

set.seed(123)

output_valid_fn <- function(d) {
  require(tidyverse)
  require(tidymodels)
  require(lubridate)

report_start <- d
report_end <- report_start + ddays(n_days)

# run the separate codes

nctr_df <- nctr_df_full %>% filter(Census_Date <= report_start,
                                   Organisation_Site_Code != 'RVJ01') # remove NBT for validation
# NCTR data summary
nctr_sum <- nctr_df %>%
  filter(Person_Stated_Gender_Code %in% 1:2) %>%
  mutate(nhs_number = as.character(NHS_Number),
         nhs_number = if_else(is.na(nhs_number), glue::glue("unknown_{1:n()}"), nhs_number),
         sex = if_else(Person_Stated_Gender_Code == 1, "Male", "Female")) %>%
  # filter for our main sites / perhaps I shouldn't do this?
  filter(Organisation_Site_Code %in% c('RVJ01', 'RA701', 'RA301', 'RA7C2')) %>%
  # filter for CTR, we wont predict the NCTR outcome for those already NCTR/on a queue
  # filter(Criteria_To_Reside == "N") %>%
  mutate(
    site = case_when(
      Organisation_Site_Code == 'RVJ01' ~ 'nbt',
      Organisation_Site_Code == 'RA701' ~ 'bri',
      Organisation_Site_Code %in% c('RA301', 'RA7C2') ~ 'weston',
      TRUE ~ 'other'
    ),
    Date_Of_Admission = as.Date(Date_Of_Admission)
  ) %>%
  group_by(Organisation_Site_Code) %>%
  filter(Census_Date == max(Census_Date)) %>%
  filter(site != "nbt") %>%
  mutate(site = fct_drop(site)) %>%
  ungroup() %>%
  mutate(
    der_los = (as.Date(Census_Date) - as.Date(Date_Of_Admission))/ddays(1),
    der_ctr = case_when(
      Criteria_To_Reside == "Y" | is.na(Criteria_To_Reside) ~ TRUE,
      !is.na(Days_NCTR) ~ FALSE,
      !is.na(Date_NCTR) ~ FALSE,
      Criteria_To_Reside == "N" ~ FALSE
    )) %>%
  mutate(report_date = max(Census_Date)) %>%
  mutate(los = (report_date - Date_Of_Admission) / ddays(1)) %>%
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
  dplyr::select(
    report_date,
    # this is a workaround for bad DQ / census date is not always consistent at time of running
    nhs_number,
    sex,
    age = Person_Age,
    ctr = der_ctr,
    site,
    bed_type = Bed_Type,
    los = der_los,
    pathway
  ) %>%
  ungroup()

discharges_ts <- nctr_df_full %>%
  filter(Census_Date <= d) %>%
  filter(!is.na(NHS_Number)) %>%
  # filter(Organisation_Site_Code %in% c('RVJ01', 'RA701', 'RA301', 'RA7C2')) %>%
  filter(Organisation_Site_Code %in% c('RA701', 'RA301', 'RA7C2')) %>%
  # filter for CTR, we wont predict the NCTR outcome for those already NCTR/on a queue
  # filter(Criteria_To_Reside == "N") %>%
  mutate(
    site = case_when(
      Organisation_Site_Code == 'RVJ01' ~ 'nbt',
      Organisation_Site_Code == 'RA701' ~ 'bri',
      Organisation_Site_Code %in% c('RA301', 'RA7C2') ~ 'weston',
      TRUE ~ 'other'
    )) %>%
  filter(site != "nbt") %>%
  mutate(site = fct_drop(site)) %>%
  mutate(pathway = recode(Current_Delay_Code_Standard,
                          !!!pathway_recodes),
         pathway = coalesce(pathway, "Other")) %>% 
  ungroup() %>%
  mutate(pathway = if_else(
    !pathway %in% c("P1", "P2", "P3", "P3" , "Other"),
    "Other",
    pathway
  )) %>%
  dplyr::select(nhs_number = NHS_Number, site, pathway, date = Census_Date, Date_Of_Admission) %>%
  distinct() %>%
  # filter(pathway != "Other") %>%
  group_by(nhs_number, Date_Of_Admission) %>%
  mutate(id = cur_group_id()) %>%
  group_by(id) %>%
  # take the first non-other pathway
  mutate(pathway_date = ifelse(any(pathway != "Other"), min(date[pathway != "Other"]), max(date))) %>%
  mutate(pathway_date = as_date(pathway_date)) %>%
  filter(date == pathway_date) %>%
  group_by(site, pathway, date = date + ddays(1)) %>%
  count() %>%
  group_by(site, pathway) %>%
  filter(date < max(date)) %>%
  arrange(date) %>%
  slice(-1) %>%
  mutate(pathway = recode(pathway, 
                          "Other" = "Not D2A service",
                          "P1" = "For P1 service",
                          "P2" = "For P2 service",
                          "P3" = "For P3 service")) %>%
  mutate(site = recode(site, 
                       "bri" = "BRI",
                       "nbt" = "NBT",
                       "weston" = "WGH"))


# mean number of discharges per day/site
discharge_sum <- discharges_ts %>%
  filter(date >= (max(date) - dweeks(4))) %>%
  group_by(site, pathway) %>% 
  summarise(naive_bl = mean(n),
            naive_bl_sd = sd(n))

source("code_admits_fcast.R", local = TRUE)
source("code_new_admits.R", local = TRUE)
source("code_curr_admits.R", local = TRUE)


df_pred <- bind_rows(df_curr_admits, df_new_admit) %>%
  group_by(site, rep, day, pathway) %>%
  summarise(n = sum(count)) %>% # aggregate over source (current/new admits)
  group_by(site, day, pathway) %>% # compute CIs/mean over reps
  summarise(across(n, list(mean = mean,
                           u85 = {\(x) quantile(x, 0.925)},
                           l85 = {\(x) quantile(x, 0.075)}
  ))) %>% 
  filter(day <= n_days) %>%
  rename(n = n_mean,
         u85 = n_u85,
         l85 = n_l85)

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
  group_by(site, ctr, pathway) %>%
  count() %>%
  mutate(ctr = if_else(ctr, "Y", "N"),
         source = "current_ctr_data",
         report_date = max_date,
         day = 1) %>%
  pivot_longer(cols = c(n),
               names_to = "metric",
               values_to = "value")

plot_df_fcast <- df_admit_fcast %>% dplyr::select(-date)


plot_df <- bind_rows(plot_df_pred, 
                     plot_df_current,
                     # plot_df_queue_sim,
                     plot_df_fcast) %>%
  mutate(pathway = factor(pathway, levels = (c("Other", "P1", "P2", "P3"))),
         report_date = as.character(report_date)) # convert date to character because RODBC/R/SQL can't handle writing this in a consistent way


# now empirical values


nctr_df_emp <- nctr_df_full %>% filter(Census_Date >= report_start)

# NCTR data summary

nctr_sum_emp <- nctr_df_emp %>%
  filter(Person_Stated_Gender_Code %in% 1:2) %>%
  mutate(nhs_number = as.character(NHS_Number),
         nhs_number = if_else(is.na(nhs_number), glue::glue("unknown_{1:n()}"), nhs_number),
         sex = if_else(Person_Stated_Gender_Code == 1, "Male", "Female")) %>%
  # filter for our main sites / perhaps I shouldn't do this?
  filter(Organisation_Site_Code %in% c('RVJ01', 'RA701', 'RA301', 'RA7C2')) %>%
  # filter for CTR, we wont predict the NCTR outcome for those already NCTR/on a queue
  # filter(Criteria_To_Reside == "N") %>%
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
    der_los = (as.Date(Census_Date) - as.Date(Date_Of_Admission))/ddays(1),
    der_ctr = case_when(
      Criteria_To_Reside == "Y" | is.na(Criteria_To_Reside) ~ TRUE,
      !is.na(Days_NCTR) ~ FALSE,
      !is.na(Date_NCTR) ~ FALSE,
      Criteria_To_Reside == "N" ~ FALSE
    )) %>%
  mutate(los = (report_start - Date_Of_Admission) / ddays(1)) %>%
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
  dplyr::select(
    Census_Date,
    Date_Of_Admission,
    # this is a workaround for bad DQ / census date is not always consistent at time of running
    nhs_number,
    sex,
    age = Person_Age,
    ctr = der_ctr,
    Date_NCTR,
    site,
    bed_type = Bed_Type,
    los = der_los,
    pathway
  ) %>%
  ungroup()

source("shinyApp/colour_functions.R")
source("shinyApp/theme.R")


nctr_sum_emp %>%
  arrange(Census_Date) %>%
  # remove anyone currently NCTR  
  group_by(nhs_number, Date_Of_Admission) %>%
  mutate(spell_id = cur_group_id()) %>%
  group_by(spell_id) %>%
  filter(Date_Of_Admission > report_start | ctr[1]) %>%
  mutate(der_date_nctr = as.Date(if_else(any(!ctr),min(Census_Date[!ctr]) - ddays(1),max(Census_Date)))) %>%
  ungroup() %>%
  mutate(der_date_nctr = pmin(der_date_nctr, Date_NCTR, na.rm = TRUE),
         der_pathway = ifelse(length(pathway[pathway != "Other"]) > 0, head(pathway[pathway != "Other"], 1), "Other")) %>% 
  group_by(spell_id) %>%
  dplyr::select(site, spell_id, der_date_nctr, pathway) %>%
  distinct() %>% 
  group_by(site, date = der_date_nctr, pathway) %>%
  count() %>%
  ungroup() %>%
  complete(site, date, pathway, fill = list(n = 0)) %>%
  mutate(day = (date - report_start)/ddays(1)) %>%
  left_join(plot_df %>%
              filter(source == "model_pred") %>%
              pivot_wider(names_from = "metric", values_from = "value") %>%
              dplyr::select(site, day, pathway, n_pred = n, u85, l85)) %>%
  filter(between(day, 1, 10)) %>%
  mutate(pathway = recode(pathway, 
                          "Other" = "Not D2A service",
                          "P1" = "For P1 service",
                          "P2" = "For P2 service",
                          "P3" = "For P3 service")) %>%
  mutate(site = recode(site, 
                       "bri" = "BRI",
                       "nbt" = "NBT",
                       "weston" = "WGH")) %>%
  left_join(discharge_sum)

}


output_valid_fn_safe <- safely(output_valid_fn)

options(future.globals.maxSize = 16000 * 1024^2)
future::plan(future::multisession, workers = parallel::detectCores() - 2)
out <- furrr::future_map(dates, output_valid_fn_safe,
                         .options = furrr::furrr_options(
                           globals = c(
                             "run_date",
                             "n_days",
                             "nctr_df",
                             "nctr_df_full",
                             "pathway_recodes",
                             "plot_int",
                             "n_rep",
                             "get_sd_from_ci",
                             "attr_df",
                             "max_date"
                           )))


saveRDS(out, "data/final_validation_out_latest.RDS")
saveRDS(out, "S:/Finance/Shared Area/BNSSG - BI/8 Modelling and Analytics/working/nh/projects/discharge_pathway_projections/data/final_validation_out_latest.RDS")

out %>%
  map("result") %>%
  map_lgl(is.null) %>%
  which()

out %>%
  map("result") %>%
  bind_rows(.id = "id") %>% 
    filter(site != "NBT") %>%
  mutate(day = 1 + lubridate::interval(min(date), date)/ddays(1), .by = id) %>%
  mutate(sim_error = mae_vec(n, n_pred),
         bl_error = mae_vec(n, naive_bl), .by = c(site, date, pathway, id)) %>%
  summarise(sim_error = mean(sim_error),
            bl_error = mean(bl_error),
            .by = c(site, day, pathway)) %>%
    select(site, day, pathway, sim_error, bl_error) %>%
    mutate(diff = bl_error - sim_error) %>%
    # pivot_longer(cols = c(sim_error, bl_error), names_to = "error", values_to = "value") %>%
    ggplot(aes(x = day, y = diff)) +
    geom_line() +
    facet_grid(pathway~site, scales = "free")
  

out %>%
  map("result") %>%
  bind_rows(.id = "id") %>% 
    filter(site != "NBT") %>%
  mutate(day = 1 + lubridate::interval(min(date), date)/ddays(1), .by = id) %>%
  mutate(sim_error = mae_vec(n, n_pred),
         bl_error = mae_vec(n, naive_bl), .by = c(site, date, pathway, id)) %>%
  select(site, date, day, pathway, sim_error, bl_error) %>%
  mutate(diff = bl_error - sim_error) %>%
  nest(.by = site) %>%
  mutate(plot = map(data, \(x) x %>% ggplot(aes(x = date, y = diff)) +
                      ggforce::geom_link2(aes(colour = after_stat(ifelse(y > 0, "positve", "negative")))) +
                      facet_grid(pathway~day, scales = "free"))) %>%
  pull(plot)
  # pivot_longer(cols = c(sim_error, bl_error), names_to = "error", values_to = "value") %>%
  
out %>%
  map("result") %>%
  bind_rows(.id = "id") %>%
  filter(site != "NBT") %>%
  mutate(day = 1 + lubridate::interval(min(date), date) / ddays(1),
         .by = id) %>%
  mutate(
    sim_error = mae_vec(n, n_pred),
    bl_error = mae_vec(n, naive_bl),
    .by = c(site, date, pathway, id)
  ) %>%
  summarise(
    bl_error = mean(bl_error),
    sim_error = mean(sim_error),
    date = min(date),
    .by = c(id, site, pathway)
  ) %>%
select(site, date, pathway, sim_error, bl_error) %>%
  mutate(diff = bl_error - sim_error) %>%
  ggplot(aes(x = date, y = diff)) +
  ggforce::geom_link2(aes(colour = after_stat(ifelse(y > 0, "positve", "negative")))) +
  facet_grid(pathway ~ site, scales = "free") +
  theme_minimal()


out %>%
  map("result") %>%
  bind_rows(.id = "id") %>% 
    filter(site != "NBT") %>%
  mutate(day = 1 + lubridate::interval(min(date), date)/ddays(1), .by = id) %>%
  mutate(sim_error = mae_vec(n, n_pred),
         bl_error = mae_vec(n, naive_bl), .by = c(site, date, pathway, id)) %>%
  summarise(sim_error = mean(sim_error),
            bl_error = mean(bl_error),
            .by = c(site, day, pathway)) %>%
  select(site, day, pathway, sim_error, bl_error) %>%
  pivot_longer(cols = c(sim_error, bl_error), names_to = "metric", values_to = "value") %>%
    ggplot(aes(x = day, y = value, col = metric)) +
    geom_line() +
    facet_grid(pathway~site, scales = "free")




# out <- map(sample(dates, 1), output_valid_fn_safe)
# 
# 
# 
# 
# out[[1]]$result %>% 
#   filter(site != "NBT") %>%
#   mutate(sim_error = mae_vec(n, n_pred),
#          bl_error = mae_vec(n, naive_bl), .by = c(site, date, pathway)) %>%
#   select(site, date, pathway, sim_error, bl_error) %>%
#   pivot_longer(cols = c(sim_error, bl_error), names_to = "error", values_to = "value") %>%
#   ggplot(aes(x = date, y = value, fill = error)) +
#   geom_col(position = "dodge") +
#   facet_grid(pathway~site, scales = "free")

n_day <- 1

bind_rows(out, .id = "rep") %>%
  mutate(day = n_day*(day %/% n_day)) %>%
  group_by(rep, site, day, pathway) %>%
  summarise(n = sum(n), n_pred = sum(n_pred)) %>%
  group_by(site, day, pathway) %>%
  summarise(smpe = smpe_custom(n, n_pred)) %>%
  ggplot(aes(x = day, y = smpe)) + 
  geom_line() +
  facet_grid(pathway ~ site) +
  scale_x_continuous(breaks = 1:10) +
  theme_minimal() +
  labs(y = "Symmetric mean percentage error",
       x = "Day")

ggsave(
  last_plot(),
  filename = "./validation/scratch_validation_final_output_1.png",
  bg = "white",
  scale = 0.8,
  width = 10,
  height = 7.5
)

bind_rows(out, .id = "rep") %>%
  mutate(day = n_day*(day %/% n_day)) %>%
  group_by(rep, site, day) %>%
  summarise(n = sum(n), n_pred = sum(n_pred)) %>%
  group_by(site, day) %>%
  summarise(smpe = smpe_custom(n, n_pred)) %>%
  ggplot(aes(x = day, y = smpe)) + 
  geom_line() +
  facet_grid(~ site)

ggsave(
  last_plot(),
  filename = "./validation/scratch_validation_final_output_2.png",
  scale = 0.6,
  width = 20,
  height = 10
)

bind_rows(out, .id = "rep") %>%
  mutate(day = n_day*(day %/% n_day)) %>%
  group_by(rep, site) %>%
  summarise(n = sum(n), n_pred = sum(n_pred)) %>%
  group_by(site) %>%
  summarise(smpe = smpe_custom(n, n_pred)) %>%
  ggplot(aes(x = site, y = smpe)) + 
  geom_col()

ggsave(
  last_plot(),
  filename = "./validation/scratch_validation_final_output_3.png",
  scale = 0.6,
  width = 20,
  height = 10
)



