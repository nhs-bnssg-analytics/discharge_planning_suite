library(RODBC)
library(tidyverse)

data_folder_path <- switch(.Platform$OS.type,
                           windows = 'C:/Users/nick.howlett/repos/discharge_pathway_projections_local/shinyApp/data',#"z:/lho/data",
                           unix = '/samba/nowcast-data/dpp/data')

                            
if (.Platform$OS.type == "windows") {
 uid = "win_user"
 pwd = Sys.getenv("DB_CRED")
} else if (.Platform$OS.type == "unix") {
  uid = "ics_server"
  pwd = Sys.getenv("DB_CRED")
} else {
  stop("Not on windows or unix?")
}

con <- RMySQL::dbConnect(RMySQL::MySQL(),
                         dbname = "ics_db",
                         host="bns-000-as02",
                         username=uid,
                         password=pwd
                         )

## Use the lines below to read the predictions made using the server
query <- '
SELECT *
FROM discharge_pathway_projections;'

data_dpp <- RMySQL::dbSendQuery(con, query)
data_dpp <- DBI::dbFetch(data_dpp,n = -1)

report_date <- ymd(data_dpp$report_date)[1]

data_dpp <- data_dpp %>%
  select(-row_names) %>%
  mutate(across(matches('date'), ~ as.POSIXct(.x, tz = 'UTC'))) %>%
  mutate(pathway_add = factor(pathway, levels =
    c("Other", "P1", "P2", "P3"),
    labels = c("..not for D2A service",
               "..for P1 service",
               "..for P2 service",
               "..for P3 service")
  )) %>%
  mutate(pathway_q = factor(pathway, levels =
    c("Other", "P1", "P2", "P3"),
    labels = c("P0 queue or other",
               "P1 queue",
               "P2 queue",
               "P3 queue")
  )) %>%
  mutate(site = factor(recode(site, bri = "BRI", nbt = "NBT", weston = "Weston"), levels = c("NBT", "BRI", "Weston"))) %>% 
  # mutate(pathway = fct_recode(pathway,  !!!levels)) %>%
  # mutate(pathway = fct_recode(pathway,  "NTCR but not on D2A queue" = "Other")) %>%
  pivot_wider(names_from = metric,
              values_from = value) %>%
  mutate(
    tooltip_slot_avg = glue::glue("4-week mean discharges per day = {round(slot_avg, 1)}"),
    tooltip_q = glue::glue("{format(report_date + ddays(day+1), '%a %d %b')}<br/>{pathway_q} = {round(n, 0)} ({round(n_u85,0)}, {round(n_l85,0)})"),
    tooltip_q_u = glue::glue("{format(report_date + ddays(day+1), '%a %d %b')}<br/>{pathway_q} = {round(n_u, 0)} ({round(n_u_u85,0)}, {round(n_u_l85,0)})"),
    tooltip_q_l = glue::glue("{format(report_date + ddays(day+1), '%a %d %b')}<br/>{pathway_q} = {round(n_l, 0)} ({round(n_l_u85,0)}, {round(n_l_l85,0)})"),
    tooltip_n = glue::glue('{format(report_date + ddays(day+1), "%a %d %b")}<br/>{str_replace_all(pathway_add, "\\\\.|for", "")} = {round(n, 0)}'),
    tooltip_n_noqueue = glue::glue("{str_remove_all(pathway_add, 'queue')} = {round(n, 0)}"),
    tooltip_errorbar = glue::glue("({round(u85,0)}, {round(l85,0)})")
  ) #%>%
  # mutate(pathway = fct_relevel(pathway, names(levels), after = 0))
