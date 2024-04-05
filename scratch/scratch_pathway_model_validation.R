source("utils.R")
n_rep <- 1E3

nctr_sum <- nctr_df %>%
  filter(Person_Stated_Gender_Code %in% 1:2) %>%
  mutate(nhs_number = as.character(NHS_Number),
         nhs_number = if_else(is.na(nhs_number), glue::glue("unknown_{1:n()}"), nhs_number),
         sex = if_else(Person_Stated_Gender_Code == 1, "Male", "Female"))  %>%
  filter(Census_Date < max(Census_Date) -ddays(10)) %>%
  # mutate(nhs_number[is.na(nhs_number)] = glue::glue("unknown_{seq_along(nhs_number[is.na(nhs_number)])}"))
  mutate(site = case_when(Organisation_Site_Code == 'RVJ01' ~ 'nbt',
                          Organisation_Site_Code == 'RA701' ~ 'bri',
                          Organisation_Site_Code %in% c('RA301', 'RA7C2') ~ 'weston',
                          TRUE ~ NA_character_)) %>%
  filter(!is.na(site)) %>%
  group_by(nhs_number, Date_Of_Admission) %>%
  mutate(spell_id = as.character(cur_group_id())) %>%
  ungroup() %>%
  mutate(
    der_los = (as.Date(Census_Date) - as.Date(Date_Of_Admission))/ddays(1),
    der_ctr = case_when(
      Criteria_To_Reside == "Y" | is.na(Criteria_To_Reside) ~ TRUE,
      !is.na(Days_NCTR) ~ FALSE,
      !is.na(Date_NCTR) ~ FALSE,
      Criteria_To_Reside == "N" ~ FALSE
    )) %>%
  mutate(
    pathway = recode(
      Current_Delay_Code_Standard,
      "P3 / Other Complex Discharge" = "P3",
      "Uncoded" = "Other",
      "Repatriation" = "Other",
      "NCTR Null" = "Other",
      "Not Set" = "Other",
      "18a  Infection  bxviii  Standard" = "Other",
      "xviii. Awaiting discharge to a care home but have not had a COVID 19 test (in 48 hrs preceding discharge)." = "Other",
      "15b  Repat  bxv  WGH" = "Other"
    ),
    pathway = coalesce(pathway, "Other")
  ) %>%
  mutate(pathway = if_else(
    !pathway %in% c("P1", "P2", "P3", "P3" , "Other"),
    "Other",
    pathway
  )) %>%
  dplyr::select(
    # this is a workaround for bad DQ / census date is not always consistent at time of running
    Census_Date,
    nhs_number,
    spell_id,
    sex,
    age = Person_Age,
    ctr = der_ctr,
    Days_NCTR,
    site,
    bed_type = Bed_Type,
    los = der_los,
    pathway
  ) %>%
  ungroup()

attr_df <-
  RODBC::sqlQuery(
    con,
    "select * from (
select a.*, ROW_NUMBER() over (partition by nhs_number order by attribute_period desc) rn from
[MODELLING_SQL_AREA].[dbo].[New_Cambridge_Score] a) b where b.rn = 1"
  )

rf_wf <- readRDS("data/rf_wf.RDS")


dates_spells <- nctr_sum %>%
  filter(ctr) %>%
  group_by(Census_Date, Days_NCTR, spell_id) %>%
  distinct() 

# dates_spells <- dates_spells %>% 
#   bind_rows(dates_spells %>% mutate(spell_id = glue::glue("{spell_id}_sys")))

dates <- nctr_df %>%
  filter(Census_Date > ymd("2023-07-01"),
         Census_Date < max(Census_Date) - ddays(50)) %>%
  pull(Census_Date) %>%
  unique()

# take sample of dates
d_i <- sample(dates, 9)

out <- map(d_i, ~{
  
  
  sid_i <- dates_spells %>%
    filter(Census_Date ==.x) %>%
    pull(spell_id) %>% 
    unique()
  
  
  los_df <- nctr_sum %>%
    filter(spell_id %in% sid_i, Census_Date == .x) %>%
    left_join(select(attr_df, -sex, -age) %>% mutate(nhs_number = as.character(nhs_number)),
              by = join_by(nhs_number == nhs_number)) %>%
    dplyr::select(spell_id, age, sex, cambridge_score, bed_type, site, los) #%>%
  
  
  pathway_pred <- bind_cols(los_df, predict(rf_wf, los_df, type = "prob")) %>%
    select(spell_id, starts_with(".pred_")) %>%
    distinct()
  
  pathway_df <- nctr_sum %>%
    filter(spell_id %in% sid_i, Census_Date >= .x) %>%
    arrange(Census_Date) %>%
    group_by(spell_id) %>%
    # mutate(pathway = tail(pathway, 1)) %>%
    # take first non-other pathway, else pathway is other
    mutate(pathway = ifelse(length(pathway[pathway != "Other"]) > 0, head(pathway[pathway != "Other"], 1), "Other")) %>%
    select(spell_id, pathway) %>%
    distinct() %>%
    group_by(pathway) %>%
    count()
  
  # full_join(pathway_df, pathway_pred)  
  
  
  pathway_pred <- pathway_pred %>%
    mutate(pathways = pmap(list(.pred_Other,
                                .pred_P1,
                                .pred_P2,
                                .pred_P3), 
                           ~factor(sample(c("Other", "P1", "P2", "P3"),
                                          n_rep,
                                          prob = c(..1, ..2, ..3, ..4),
                                          replace = TRUE)),
                           levels = c("Other", "P1", "P2", "P3"))) %>%
    unnest(pathways) %>%
    group_by(spell_id) %>%
    mutate(rep = 1:n()) %>%
    group_by(rep, pathways) %>%
    count() %>%
    group_by(pathway = pathways) %>%
    summarise(mean = mean(n), u95 = quantile(n, 0.975), l95 = quantile(n, 0.225))
  
  full_df <-
    full_join(pathway_df, pathway_pred) %>%
    mutate(pval = map2(n, mean, ~
                         tidy(chisq.test(x = c(
                           .x, .y
                         ))))) %>%
    unnest()
  
  
  full_df %>%
    ggplot(aes(x = pathway)) +
    geom_errorbar(aes(ymin = l95, ymax = u95), width = 0.2) +
    geom_point(aes(y = mean, col = "pred"), shape = 2) +
    geom_point(aes(y = n, col = "actual")) +
    geom_text(aes(y = n, label = glue::glue("{round(p.value, 3)}")), hjust = -0.4) +
    theme_bw()
  
})

(ptc <- patchwork::wrap_plots(out, axes = "collect") + patchwork::plot_layout(guides = "collect"))

ggsave(
  ptc,
  filename = "./validation/validation_plot_pathway_agg.png",
  scale = 0.6,
  width = 20,
  height = 10
)

