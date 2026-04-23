


plot_df_queue_sim <- local({
  
  # Create distributions for discharge capacity, based empirically on the last
  # 4-weeks of actuals
  
  
  safe_min <- function(x) {
    if (length(x) == 0 || all(is.na(x))) return(as.Date(NA))
    min(x, na.rm = TRUE)
  }
  
  nctr_lazy <- lazy_dt(nctr_df)
  
  discharges_ts <- nctr_df %>%
    filter(Census_Date >= (max(Census_Date) - dweeks(4))) %>%
    filter(Person_Stated_Gender_Code %in% 1:2) %>%
    mutate(nhs_number = as.character(NHS_Number),
           nhs_number = if_else(is.na(nhs_number), CDS_Unique_Identifier, nhs_number),
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
    group_by(NHS_Number, Date_Of_Admission) %>%
    mutate(pathway = ifelse(length(pathway[pathway != "Other"]) > 0, head(pathway[pathway != "Other"], 1), "Other")) %>%
    mutate(keep_date = case_when(
      any(!der_ctr, na.rm = TRUE) ~ as.Date(safe_min(Census_Date[!der_ctr]) - ddays(1)),
      TRUE ~ as.Date(max(Census_Date, na.rm = TRUE))
    )) %>%
    filter(Census_Date < max(Census_Date)) %>%
    pivot_longer(
      cols = c(site, la),
      names_to = "grp_type",
      values_to = "grp",
    ) %>%
    dplyr::select(
      date = keep_date,
      nhs_number,
      grp,
      pathway
    ) %>%
    ungroup() %>%
    distinct()  %>%
    # group_by(site, pathway, date = date + ddays(1)) %>%
    filter(!is.na(date)) %>%
    group_by(grp, pathway, date = date) %>%
    count() %>%
    ungroup() %>%
    complete(nesting(grp, date), pathway, fill = list(n = 0)) %>%
    filter(date != max(date, na.rm = TRUE), date != min(date, na.rm = TRUE), pathway != "Other", grp != "other")
  
    capacity_dists <- discharges_ts %>%
      filter(date >= (max(date) - dweeks(4))) %>%
      nest(.by = c(grp, pathway)) %>%
      mutate(capacity_dist = map(data, ~partial(EnvStats::remp, obs = .x$n))) %>%
      select(-data)
    
  # Create empircal discharge demand distributions (from simulated results)
    
    dt_curr <- lazy_dt(df_curr_admits)
    dt_new  <- lazy_dt(df_new_admit)
    
    cols <- c("grp", "rep", "day", "pathway", "count")
    
    demand_dists <- dt_curr %>%
      select(all_of(cols)) %>%
      union_all(
        dt_new %>% select(all_of(cols))
      ) %>%
      filter(day <= 10) %>%
      group_by(grp, rep, day, pathway) %>%
      summarise(n = sum(count), .groups = "drop") %>%
      group_by(grp, day, pathway) %>%
      as_tibble() %>%
      # Arrange to ensure the 10-day vector is in chronological order
      arrange(grp, pathway, rep, day) %>% 
      # Group by grp, pathway, and rep to create the 10-day vectors
      group_by(grp, pathway, rep) %>%
      summarise(trajectory = list(n), .groups = "drop") %>%
      # Now nest the trajectories by grp and pathway
      nest(data = c(rep, trajectory)) %>%
      mutate(demand_dist = map(data, function(df) {
        # This returns a function that, when called, 
        # picks one random 10-day vector from the available reps
        return(function() {
          sample(df$trajectory, 1) %>% unlist()
        })
      })) %>%
      select(-data)
    


# current queue

pathway_queue <- nctr_sum %>%
  filter(!is.na(nhs_number), !is.na(ctr)) %>%
  filter(pathway != "Other", !ctr) %>% 
  dplyr::select(-ctr) %>%
  group_by(grp, pathway) %>%
  count() %>%
  mutate(source = "current_ctr_data",
         report_date = max_date,
         day = 1) %>%
  pivot_longer(cols = c(n),
               names_to = "metric",
               values_to = "value")


# Calculate queue evolution 

scenarios <- tibble(
  scenario_name = c("Capacity -10%", "Base case", "Capacity +10%"),
  stip_factor = c(0.9, 1.0, 1.1)
)

calculate_queue_evolution <- function(demand_vec, capacity_vec, initial_q) {
  # accumulate() passes the 'result of the last day' as 'prev_q'
  # and the 'current day's values' as 'current'
  accumulate2(demand_vec, capacity_vec, .init = initial_q, 
              \(prev_q, d, c) max(0, prev_q + d - c))[-1] # [-1] removes the .init value
}


df_sim <- left_join(capacity_dists, demand_dists, by = c("grp", "pathway")) %>% 
  left_join(pathway_queue %>% select(grp, pathway, initial_q = value), by = c("grp", "pathway")) %>% 
  cross_join(scenarios) %>% 
  mutate(sim_results = pmap(list(capacity_dist, demand_dist, stip_factor, initial_q), 
                            function(c_dist, d_dist, f, i_q) {
                              reps <- replicate(n_rep, {
                                c_traj <- round(c_dist(n_days) * f)
                                d_traj <- d_dist()
                                calculate_queue_evolution(d_traj, c_traj, i_q)
                              }) 
                              tibble(
                                day = 1:n_days,
                                avg = rowMeans(reps),
                                u85 = apply(reps, 1, quantile, 0.925),
                                l85 = apply(reps, 1, quantile, 0.075)
                              )
                            })) %>%
  select(grp, pathway, scenario_name, sim_results) %>%
  unnest(sim_results)  %>%
  pivot_wider(
    names_from = scenario_name, 
    values_from = c(avg, u85, l85)
  ) %>%
  rename(
    n = `avg_Base case`, 
    n_u85 = `u85_Base case`, 
    n_l85 = `l85_Base case`,
    n_u = `avg_Capacity +10%`, 
    n_u_u85 = `u85_Capacity +10%`, 
    n_u_l85 = `l85_Capacity +10%`,
    n_l = `avg_Capacity -10%`, 
    n_l_u85 = `u85_Capacity -10%`, 
    n_l_l85 = `l85_Capacity -10%`
  ) %>%
  pivot_longer(
    cols = starts_with("n"),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    ctr = "N",
    source = "queue_sim",
    report_date = max_date
  )


df_sim <- df_sim %>%
  # bind slot average values to plot baselines later on
  bind_rows(
    discharges_ts %>%
      filter(date >= (max(date) - dweeks(4))) %>%
      group_by(grp, pathway) %>% 
      summarise(mean = mean(n)) %>%
      pivot_longer(cols = -c(grp, pathway),
                   names_to = "metric", 
                   values_to = "value") %>%
      mutate(ctr = "N",
             metric = "slot_avg",
             source = "queue_sim",
             report_date = max_date)
  )

df_sim

})
