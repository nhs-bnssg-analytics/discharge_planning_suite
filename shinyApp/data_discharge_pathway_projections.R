library(RODBC)
library(RMySQL)
library(tidyverse)


get_processed_data <- function() {
  # 1. Connection logic
  host <- Sys.getenv("DB_HOST")
  dbname <- Sys.getenv("DB_NAME")
  user <- Sys.getenv("DB_USER")
  password <- Sys.getenv("DB_CRED")
  
  # Create the connection
  con <- DBI::dbConnect(DBI::dbDriver("MySQL"),
                        dbname = dbname,
                        host = host,
                        port = 3306,
                        user = user,
                        password=password)
  on.exit(DBI::dbDisconnect(con)) 
  
  # 2. Fetch
  query <- 'SELECT * FROM discharge_pathway_projections'
  raw_data <- DBI::dbGetQuery(con, query)

  # 3. Process  
  processed_data <- raw_data %>%
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
    mutate(grp = factor(recode(grp, bri = "BRI", nbt = "NBT", weston = "Weston", "north somerset" = "NSC", "bristol" = "BCC", "south gloucestershire" = "SGC", "other" = "Other"),
                        levels = c("NBT", "BRI", "Weston", "NSC", "BCC", "SGC"))) %>% 
    filter(grp != "Other") %>%
    # mutate(pathway = fct_recode(pathway,  !!!levels)) %>%
    # mutate(pathway = fct_recode(pathway,  "NTCR but not on D2A queue" = "Other")) %>%
    pivot_wider(names_from = metric,
                values_from = value) %>%
    mutate(
      tooltip_slot_avg = glue::glue("4-week mean discharges per day = {round(slot_avg, 1)}"),
      tooltip_q = glue::glue("{format(report_date + ddays(day-1), '%a %d %b')}<br/>{pathway_q} = {round(n, 0)} ({round(n_u85,0)}, {round(n_l85,0)})"),
      tooltip_q_u = glue::glue("{format(report_date + ddays(day-1), '%a %d %b')}<br/>{pathway_q} = {round(n_u, 0)} ({round(n_u_u85,0)}, {round(n_u_l85,0)})"),
      tooltip_q_l = glue::glue("{format(report_date + ddays(day-1), '%a %d %b')}<br/>{pathway_q} = {round(n_l, 0)} ({round(n_l_u85,0)}, {round(n_l_l85,0)})"),
      tooltip_n = glue::glue('{format(report_date + ddays(day-1), "%a %d %b")}<br/>{str_replace_all(pathway_add, "\\\\.|for", "")} = {round(n, 0)}'),
      tooltip_n_noqueue = glue::glue("{str_remove_all(pathway_add, 'queue')} = {round(n, 0)}"),
      tooltip_errorbar = glue::glue("({round(u85,0)}, {round(l85,0)})")
    )
  
  return(processed_data)
}


