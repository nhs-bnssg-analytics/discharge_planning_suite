library(tidyverse)
library(lubridate)
source("utils.R")

con <- switch(.Platform$OS.type,
              windows = RODBC::odbcConnect(dsn = "xsw"),
              unix = xswauth::modelling_sql_area()
  )


n_rep <- 1E4
report_start <- today() - ddays(5)
sim_length <- 10
report_end <- report_start + dhours(sim_length)
params <- fit_lnorm_split
params_prop <- prop_split
hour(report_start) <- 10
  
  # LOS dist params
  # params <- readRDS('data/params_los_by_trust.rds')
  params <- fit_lnorm_split
  params_prop <- prop_split
  df_occ <- RODBC::sqlQuery(con, 
                            glue::glue("SELECT DISTINCT
              [Organisation_Code_Provider]
             ,[Organisation_Code_Commissioner]
             ,[Census_Date]
             ,[NHS_Number]
             ,[Person_Age]
             ,[CDS_Unique_Identifier]
             ,[Sub_ICB_Location]
             ,[Organisation_Site_Code]
             ,[Specialty_Code]
             ,[Bed_Type]
             ,[Date_Of_Admission]
             ,[BNSSG]
             ,[Date_NCTR]
             ,[Current_LOS]
             ,[Days_NCTR]
	         FROM [Analyst_SQL_Area].[dbo].[NCTR_Status_Report_Daily_DLP]
           WHERE [Census_Date] = '{as.character(report_start)}'"))
  
  tictoc::tic(msg = "Current occupancy drain code")
  # occupancy from NCTR daily status report (covering - All adults (18+), Stay
  # over 24 hours, Exclude Day case & regular attenders, Exclude maternity)
  df_occ_nctr <-df_occ %>%
    # # generate id because NHS_number sometimes is NA (but presumably still a real patient)
    # mutate(id = 1:n()) %>%
    # generate id because NHS_number sometimes is NA (but presumably still a real patient)
    mutate(source = 'nctr_report') %>%
    filter(Organisation_Site_Code %in% c('RVJ01', 'RA701', 'RA301', 'RA7C2')) %>%
    mutate(site = case_when(Organisation_Site_Code == 'RVJ01' ~ 'nbt', 
                            Organisation_Site_Code == 'RA701' ~ 'bri', 
                            Organisation_Site_Code %in% c('RA301', 'RA7C2') ~ 'weston', 
                            TRUE ~ '')) %>%
    mutate(age_band = cut(Person_Age, breaks = age_breaks, include.lowest = TRUE)) %>%
    # Note: Do we need to floor census date (I think not..)
    mutate(los = (Census_Date - Date_Of_Admission)/ddays(1)) %>%
    ungroup() %>%
    select(source, site, age_band, los)
  
  site_nctr_report_totals <- 
    df_occ_nctr %>%
    group_by(site) %>%
    count()
  
  df_occ_alamac <- RODBC::sqlQuery(con, glue::glue("
        SELECT * FROM
        [Analyst_SQL_Area].[dbo].[tbl_BNSSG_Datasets_UrgentCare_Daily]
        WHERE METRIC_ID IN (347351, 346203, 346117)
        AND report_date = '{as.character(as.Date(report_start))}'
                      ")) %>%
    mutate(site = case_when(Provider == 'NBT' ~ 'southmead',
                            Provider == 'BRI' ~ 'bri',
                            Provider == 'WGH' ~ 'weston')) %>%
    # for now, filter nbt
    filter(site != 'southmead') %>%
    select(site, count = Value) %>%
    left_join(site_nctr_report_totals) %>%
    group_by(site) %>%
    mutate(remainder = max(c(0, count - n))) %>%
    uncount(remainder) %>%
    mutate(los = 0.5,
           age_band = "total",
           source = 'alamac_kitbag') %>%
    ungroup() %>%
    select(source, site, age_band, los)
  
  
  # Stochastic
  df_occ <- bind_rows(df_occ_nctr, df_occ_alamac) %>%
    mutate(id = 1:n()) %>%
    mutate(los_remaining = pmap(list(site,
                                     age_band,
                                     los),
                                # draw remaining LOS from fitted distribution
                                ~rlnormt(n_rep,
                                         meanlog = params[[..1]][[..2]][['meanlog']],
                                         sdlog = params[[..1]][[..2]][['sdlog']],
                                         range = c(..3, Inf)) - ..3),
           pathway = map2(site, age_band, ~c("P0", "P1", "P2", "P3")[rmultinom(n_rep, size = 1, prob = params_prop[[..1]][[..2]])|> t() |> max.col()]),
           los_remaining = map2(los_remaining, pathway, set_names),
           los_remaining = map(los_remaining, ~split(.x, names(.x)))) %>%
    select(site, id, los_remaining) %>%
    unnest(cols =c(los_remaining)) %>%
    mutate(pathway = map_chr(los_remaining, ~unique(names(.x)))) %>%
    unnest(cols =c(los_remaining)) %>%
    mutate(los_remaining = ifelse(los_remaining < 0, 0, los_remaining)) %>%
    mutate(los_remaining = los_remaining %/% 1)  %>%
    group_by(id) %>%
    mutate(rep = 1:n())

  
  # # determinstic
  # df_occ <- bind_rows(df_occ_nctr, df_occ_alamac) %>% 
  #   mutate(id = 1:n()) %>%
  #   mutate(
  #          # los_remaining = pmap(list(site,
  #          #                           age_band,
  #          #                           los),
  #          #                      # draw remaining LOS from fitted distribution
  #          #                      ~rlnormt(n_rep,
  #          #                               meanlog = params[[..1]][[..2]][['meanlog']],
  #          #                               sdlog = params[[..1]][[..2]][['sdlog']],
  #          #                               range = c(..3, Inf)) - ..3),
  #          los_remaining = pmap(list(site,
  #                                    age_band,
  #                                    los),
  #                               # draw remaining LOS from fitted distribution
  #                               ~rep(rlnormt(1,
  #                                        meanlog = params[[..1]][[..2]][['meanlog']],
  #                                        sdlog = params[[..1]][[..2]][['sdlog']],
  #                                        range = c(..3, Inf)), n_rep) - ..3),
  #          # pathway = map2(site, age_band, ~c("P0", "P1", "P2", "P3")[rmultinom(n_rep, size = 1, prob = params_prop[[..1]][[..2]])|> t() |> max.col()]),
  #          pathway = map2(site, age_band, ~rep(c("P0", "P1", "P2", "P3")[rmultinom(1, size = 1, prob = params_prop[[..1]][[..2]])|> t() |> max.col()]), n_rep),
  #          los_remaining = map2(los_remaining, pathway, set_names),
  #          los_remaining = map(los_remaining, ~split(.x, names(.x)))) %>%
  #   select(site, id, los_remaining) %>%
  #   unnest(cols =c(los_remaining)) %>%
  #   mutate(pathway = map_chr(los_remaining, ~unique(names(.x)))) %>%
  #   unnest(cols =c(los_remaining)) %>%
  #   mutate(los_remaining = ifelse(los_remaining < 0, 0, los_remaining)) %>% 
  #   mutate(los_remaining = los_remaining %/% 1)  %>%
  #   group_by(id) %>%
  #   mutate(rep = 1:n())


    
    
foo <- df_occ %>%
  group_by(site, rep, pathway, los_remaining) %>% 
  summarise(n = n()) %>%
  ungroup() %>%
  # need to complete the observations where no 
  tidyr::complete(site, rep, pathway, los_remaining, fill = list(n = 0)) %>%
  group_by(site, pathway, los_remaining) %>%
  summarise(mean = mean(n),
            u_95 = quantile(n, 0.975),
            l_95 = quantile(n, 0.025),
            u_80 = quantile(n, 0.9),
            l_80 = quantile(n, 0.1),
            u_50 = quantile(n, 0.5),
            l_50 = quantile(n, 0.5),
  )
  
foo %>% filter(los_remaining <= 10) %>%
  ggplot(aes(x = factor(los_remaining), y = mean)) +
  geom_col() +
  geom_errorbar(aes(ymin = l_95, ymax = u_95)) +
  facet_grid(pathway ~ site,
             scales = "free"
             )


df_occ <- bind_rows(df_occ_nctr, df_occ_alamac) %>% 
  mutate(id = 1:n()) %>%
  mutate(pathway = map2(site, age_band, ~sample_prop(params_prop[[.x]][[.y]], size = n_rep))) %>%
  mutate(los_remaining = pmap(list(site,
                                    age_band,
                                    los),
                               # draw remaining LOS from fitted distribution
                               ~rlnormt(n_rep,
                                        meanlog = params[[..1]][[..2]][['meanlog']],
                                        sdlog = params[[..1]][[..2]][['sdlog']],
                                        range = c(..3, Inf)) - ..3)) %>%
  mutate(los_remaining = map2(los_remaining, pathway, ~set_names(.x, .y))) %>%
  mutate(los_remaining = map(los_remaining, ~split(.x, names(.x)))) %>%
  select(site, id, los_remaining) %>%
  unnest(cols =c(los_remaining)) %>%
  mutate(pathway = map_chr(los_remaining, ~unique(names(.x)))) %>%
  unnest(cols =c(los_remaining)) %>%
  mutate(los_remaining = ifelse(los_remaining < 0, 0, los_remaining)) %>% 
  mutate(los_remaining = los_remaining %/% 1)  %>%
  group_by(id) %>%
  mutate(rep = 1:n())
  
  
foo <- df_occ %>%
  group_by(site, rep, pathway, los_remaining) %>% 
  summarise(n = n()) %>%
  ungroup() %>%
  tidyr::complete(site, rep, pathway, los_remaining, fill = list(n = 0)) %>%
  group_by(site, pathway, los_remaining) %>%
  summarise(mean = mean(n),
            u_95 = quantile(n, 0.975),
            l_95 = quantile(n, 0.025),
            u_80 = quantile(n, 0.9),
            l_80 = quantile(n, 0.1),
            u_50 = quantile(n, 0.5),
            l_50 = quantile(n, 0.5),
  )

foo %>% filter(los_remaining <= 10, pathway != "P0") %>%
  ggplot(aes(x = factor(los_remaining), y = mean, fill = pathway)) +
  geom_col() +
  # geom_errorbar(aes(ymin = l_50, ymax = u_50)) +
  facet_grid(vars(site),
             scales = "free"
  )









# df_occ %>%
#     group_by(site, pathway) %>%
#     summarise(count = list(reduce(count, rbind))) %>%
#     mutate(
#       mean = map(count, colMeans),
#       u_80 = map(count, ~ map_dbl(array_branch(.x, margin = 2), ~ quantile(.x, .9))),
#       l_80 = map(count, ~ map_dbl(array_branch(.x, margin = 2), ~ quantile(.x, .1))),
#       u_95 = map(count, ~ map_dbl(array_branch(.x, margin = 2), ~ quantile(.x, .975))),
#       l_95 = map(count, ~ map_dbl(array_branch(.x, margin = 2), ~ quantile(.x, .025)))
#     ) %>%
#     mutate(frame = pmap(list(datetime = list(seq(report_start + dhours(1), report_end, by = "hours")),
#                              mean = mean,
#                              u_80 = u_80,
#                              l_80 = l_80,
#                              u_95 = u_95,
#                              l_95 = l_95
#     ),
#     bind_cols)) %>%
#     select(site, pathway, frame) %>%
#     unnest(cols = c(frame)) %>%
#   ggplot(aes(x = datetime, y = mean)) +
#   geom_ribbon(aes(ymin = l_95, ymax = u_95), alpha = 0.25) +
#   geom_line() +
#   facet_grid(site ~ pathway)
# 
# 
# %>%
#     pivot_longer(cols = -c(site, pathway, datetime),
#                  names_to = "metric",
#                  values_to = "value")

  # df_occ <- bind_rows(df_occ_nctr, df_occ_alamac) %>%
  #   mutate(id = 1:n()) %>%
  #   mutate(los_remaining = pmap(list(site,
  #                                    age_band,
  #                                    los),
  #                               # draw remaining LOS from fitted distribution
  #                               ~rlnormt(n_rep,
  #                                        meanlog = params[[..1]][[..2]][['meanlog']],
  #                                        sdlog = params[[..1]][[..2]][['sdlog']],
  #                                        range = c(..3, Inf)) - ..3),
  #          los_remaining = pmap(list(los_remaining,
  #                                    site,
  #                                    age_band),
  #                               # assign discharge pathway via weighted sample
  #                               ~set_names(..1, nm = sample_prop(params_prop[[..2]][[..3]], size = n_rep))),
  #          los_remaining = map(los_remaining, ~split(.x, names(.x)))) %>%
  #   select(site, id, los_remaining) %>%
  #   unnest(cols =c(los_remaining)) %>%
  #   mutate(pathway = map_chr(los_remaining, ~unique(names(.x)))) %>%
  #   unnest(cols =c(los_remaining)) %>%
  #   mutate(los_remaining = ifelse(los_remaining < 0, 0, los_remaining)) %>%
  #   mutate(los_remaining = los_remaining %/% 1)  %>%
  #   group_by(id) %>%
  #   mutate(rep = 1:n())