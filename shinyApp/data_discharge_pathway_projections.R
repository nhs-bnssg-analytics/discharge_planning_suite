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

data_dpp <- sqlQuery(con, query) %>%
  mutate(across(matches('date'), ~as.POSIXct(.x, tz = 'UTC'))) %>%
  mutate(pathway = factor(pathway, levels = (c("Other", "P1", "P2", "P3", "Not tomorrow"))))

report_date <- data_dpp$report_date[1]
