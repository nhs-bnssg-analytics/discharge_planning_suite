library(RODBC)
library(tidyverse)

data_folder_path <- switch(.Platform$OS.type,
                           windows = 'C:/Users/nick.howlett/repos/discharge_pathway_projections_local/shinyApp/data',#"z:/lho/data",
                           unix = '/samba/nowcast-data/dpp/data')

                            
if (.Platform$OS.type == "windows") {
  con_string <- c(
    "driver={SQL Server};server=Xsw-000-sp09;
                             database=ABI;
                    trusted_connection=true"
  )
} else if (.Platform$OS.type == "unix") {
  con_string <-
    readr::read_lines("/home/rshiny/sql_modelling_connect_string_linux")
} else {
  stop("Not on windows or unix?")
}

con <-
  odbcDriverConnect(con_string)

## Use the lines below to read the predictions made using the server
query <- '
SELECT *
FROM MODELLING_SQL_AREA.dbo.discharge_pathway_projections'

data_dpp <- sqlQuery(con, query)
report_date <- ymd(data_dpp$report_date)[1]

levels <- c("Not tomorrow")
levels <- set_names(levels, glue::glue("Still CTR {report_date + ddays(1)}"))

data_dpp <- data_dpp %>%
  mutate(across(matches('date'), ~ as.POSIXct(.x, tz = 'UTC'))) %>%
  mutate(pathway = factor(pathway, levels = 
    c("Other", "P1", "P2", "P3", "Not tomorrow"),
    labels = c("NTCR but not on D2A queue","P1 queue", "P2 queue", "P3 queue", names(levels))
  )) %>%
  # mutate(pathway = fct_recode(pathway,  !!!levels)) %>%
  # mutate(pathway = fct_recode(pathway,  "NTCR but not on D2A queue" = "Other")) %>%
  pivot_wider(names_from = metric,
              values_from = value) %>%
  mutate(
    tooltip_n = glue::glue("{pathway} = {round(n, 0)}"),
    tooltip_n_noqueue = glue::glue("{str_remove_all(pathway, 'queue')} = {round(n, 0)}"),
    tooltip_errorbar = glue::glue("({round(u95,0)}, {round(l95,0)})")
  ) %>%
  mutate(pathway = fct_relevel(pathway, names(levels), after = 0))
