d_i <- sample(dates_samp, 25)

future::plan(future::multisession, workers = parallel::detectCores() - 6)


out <- furrr::future_map(d_i, ~{
  # out <- map(d_i, ~{
  
  sim <- expand_grid(site = sites[c(1,3)],
                     rep = seq_len(n_rep),
                     date = dates) %>%
    left_join(admits_ts, join_by(site, date == date)) %>%
    filter(site != "nbt") %>%
    filter(date >= .x, date < .x+ddays(12)) %>%
    mutate(arrivals = coalesce(n, 0),
           # coalesce in case we sample below zero
           day = rep(1:12, length(sites[c(1,3)]) * n_rep)) %>%
    filter(arrivals > 0) %>%
    mutate(los = map(arrivals, function(arr) round(
      rdist(arr)))) %>%
    unnest(los) %>%
    mutate(date_end = date + ddays(los),
           day_end = day + los) %>%
    filter(day_end <= 11) %>%
    mutate(day_end = factor(day_end, levels = 0:11)) %>%
    group_by(rep, day_end) %>%
    count()
  
  
  sim_out_df <- sim %>% 
    # filter(day_end <= 10) %>%
    group_by(day_end) %>%
    summarise(
      u95 = quantile(n, 0.975),
      l95 = quantile(n, 0.025),
      n = mean(n)
    ) %>%
    mutate(source = "sim")
  
  
  act <- nctr_df %>%
    ungroup() %>%
    filter(Date_Of_Admission >= .x) %>%
    filter(Person_Stated_Gender_Code %in% 1:2) %>%
    filter(Date_Of_Admission > min(Census_Date)) %>%
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
      Date_Of_Admission = as.Date(Date_Of_Admission),
      Date_NCTR = as.Date(Date_NCTR)
    ) %>%
    filter(site != "nbt") %>%
    mutate(
      der_los = (as.Date(Census_Date) - as.Date(Date_Of_Admission))/ddays(1),
      der_ctr = case_when(
        Criteria_To_Reside == "Y" | is.na(Criteria_To_Reside) ~ TRUE,
        !is.na(Days_NCTR) ~ FALSE,
        !is.na(Date_NCTR) ~ FALSE,
        Criteria_To_Reside == "N" ~ FALSE
      )) %>%
    group_by(nhs_number, Date_Of_Admission) %>%
    mutate(spell_id = cur_group_id(),
           der_date_nctr = as.Date(if_else(any(!der_ctr),min(Census_Date[!der_ctr]) - ddays(1),max(Census_Date)))) %>%
    # rowwise() %>%
    # mutate(der_date_nctr = min(der_date_nctr, Date_NCTR, na.rm = TRUE)) %>%
    ungroup() %>%
    # filter(Census_Date >= .x, Census_Date < .x + ddays(10)) %>%
    mutate(der_date_nctr = pmin(der_date_nctr, Date_NCTR, na.rm = TRUE)) %>%
    select(nhs_number, spell_id,  Date_Of_Admission, der_date_nctr) %>%
    distinct() 
  
  
  act_out_df <- act %>%
    mutate(day_end = (der_date_nctr - .x)/ddays(1)) %>%
    filter(day_end <= 11) %>%
    mutate(day_end = factor(day_end, levels = 0:11)) %>%
    group_by(day_end, .drop = FALSE) %>%
    count() %>%
    mutate(source = "actual") 

  
  plot_df <- bind_rows(sim_out_df, act_out_df) 
  
  p <- ggplot() +
    geom_errorbar(data = na.omit(plot_df), aes(x = day_end, ymin = l95, ymax = u95)) +
    geom_col(data = plot_df, aes(x = day_end, y = n, fill = source), position = "dodge") +
    theme_bw()
  
  res_out <- plot_df %>%
    select(day_end, n, source) %>%
    pivot_wider(names_from = source, values_from = n) %>% 
    mutate(residual = actual - sim) 
  
  # summarise(residual = mean(residual)) %>%
  # ggplot(aes(x = day_end, y = residual)) +
  # geom_col() +
  # facet_wrap(vars(site))
  
  list(p = p, plot_out = plot_df, res_out = res_out)
})


bind_rows(map(out, "plot_out"), .id = "id") %>%
  filter(!is.na(n)) %>%
  # filter(id %in% c(16, 24)) %>%
  ggplot(aes(x = day_end, y = n, fill = source)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(x = day_end, ymin = l95, ymax = u95)) +
  facet_wrap(vars(id))
