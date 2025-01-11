validation_end <- ymd("2024-09-01")
validation_start <- ymd("2023-07-01")
# weeks_testing <- 13
fc_train_length_wks <- 10

start_date <- validation_start + dweeks(fc_train_length_wks)

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

demo_tab <-  los_df %>%
  ungroup() %>%
  mutate(grp = "Calibration set") %>%
  rbind(mutate(filter(., Census_Date > start_date), grp = "Validation set")) %>%
  mutate(grp = factor(grp, levels = c("Calibration set", "Validation set"))) %>%
  # mutate(s_id = cur_group_id(), .by = c(nhs_number, admission_date)) %>%
  # mutate(n_unique_adm = n_distinct(s_id), .by = grp) %>%
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
  mutate(grp = "Calibration set") %>%
  rbind(mutate(filter(., Census_Date > start_date), grp = "Validation set")) %>%
  mutate(grp = factor(grp, levels = c("Calibration set", "Validation set"))) %>% 
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


demo_tab_out %>%
huxtable::quick_html(file = "materials/demog_tab.html")

