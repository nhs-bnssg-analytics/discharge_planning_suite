
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(ggplot2)
library(magrittr)
library(scales)
library(bupaR)
library(patchwork)
library(stringr)
library(lubridate)
library(viridis)
library(RODBC)

# source("c:/Cancer Pathway/october files/prep_data.R")
# source("c:/Cancer Pathway/dashboard/dashboard_functions.R")


source("./header.R")
source("./sidebar.R")
source("./body.R")

global_data_list <- list()
global_data_list_d2 <- list()

names <- c("overview", "firstSeen", "mri", "biopsy")
# data <- purrr::map(1:6, ~readRDS(glue::glue('S:/Finance/Shared Area/BNSSG - BI/8 Modelling and Analytics/working/tb/nowcast_urgent_care/predictions/pred_{.x}.rds'))) %>%
#   bind_rows()
con <-
  odbcDriverConnect("driver={SQL Server};\n  server=Xsw-00-ash01;\n  trusted_connection=true")
query <- "SELECT VARIABLE, MAX(RUN_TIME) as max_run_time
FROM [MODELLING_SQL_AREA].[BNSSG\\Theresia.Budiman].Theresia_Forecast_Test
GROUP BY VARIABLE"
query <- 'SELECT * 
  FROM (SELECT *, MAX(RUN_TIME) OVER () AS q02
  FROM [MODELLING_SQL_AREA].[BNSSG\\Theresia.Budiman].[Theresia_Forecast_Test]) q01
WHERE RUN_TIME = q02'
data <- sqlQuery(con, query)

%>%
  mutate(site = str_extract(variable, "^[^_]*?(?=_)")) %>%
  mutate(date = as.POSIXct(date))
# ribbon <- data %>% filter(grepl('lower|upper', field)) %>% pivot_wider(names_from = field)

ui <- dashboardPage(header,
                    sidebar,
                    body)

server <- function(input,output,session){

  modalMessage <- function(x) {
    modalDialog(
      title = x,
      easyClose = FALSE,
      footer = NULL
    )
  }
  


  # lapply(1:3, function(y) {
  #     lapply(1:2, function(x) {
  #       
  #       output[[paste0("plot",y,x)]] <- renderPlotly(ggplotly(ggplot(mtcars, aes(x = cyl, y = mpg)) + geom_point() + facet_wrap(vars(gear)) + ggtitle(paste0(x,y))))
  #       
  #       # box(width = 2,
  #       #     plotlyOutput(paste0("plot",y,x))
  #       # )
  #     })
  # 
  # })
  
  observeEvent(input$buttona1, {
    print("Click Detected")
    #insert here some function to run a script that will take in either plots OR read plots from a folder, and eventually render a markdown.
    # markdownScriptFunction()
  })
  
  observeEvent(input$dropdown1, {
    input_site <- case_when(input$dropdown1 == 'bri' ~ 'BRI-UHBW', 
                      input$dropdown1 == 'weston' ~ 'W-UHBW',
                      input$dropdown1 == 'nbt' ~ 'NBT')
    plot <- data %>%
      filter(site == input_site) 
    ribbon <- plot %>%
      filter(grepl('lower|upper', field)) %>% pivot_wider(names_from = field) 
      
    output$tPlotOutput1 <- renderPlotly({
        ggplotly(ggplot() + 
                   geom_line(data = plot %>%
                               filter(!grepl('upper|lower', field)),
                             aes(x = date, y = value, color = field)) +
                   geom_ribbon(data = ribbon, aes(x = date, ymin = lower_95, ymax = upper_95), alpha = 0.2) + 
                   geom_ribbon(data = ribbon, aes(x = date, ymin = lower_80, ymax = upper_80), alpha = 0.2) + 
                   facet_wrap(vars(variable), scales = 'free', ncol = 3) +
                   theme_bw())
      })

  })  
  
  # observeEvent(input$filterDatesoverview, {
  #   print("Working")
  #   showModal(modalMessage("Filtering Data To Selected Dates"))
  #   data <- data_origin %>% 
  #     filter(max(Stage_Date) >= ymd(input$new_dates[1]) & max(Stage_Date) <= input$new_dates[2]) %>% ungroup()
  #   ref_levels <- unique(data$Pathway_Stage_Description)
    
   
    
    # data2 <- data %>% group_by(NHSNumber, CareID) %>% 
    #   mutate(month = month(max(Stage_Date)),
    #          year = year(max(Stage_Date)),
    #          month_name = month.abb[month])

    # l_data <- data2 %>% arrange(NHSNumber, CareID, Stage_Date) %>% group_by(NHSNumber, CareID) %>% group_split()
    # d2_base <- lapply(l_data, function(x) {
    #   if(nrow(x) > 1) {
    #     data.frame(id=x$NHSNumber[1],
    #                CareID = x$CareID[1],
    #                activity = x$Pathway_Stage_Description,
    #                wtime = c(0, sapply(2:nrow(x), function(y) {interval(x$Stage_Date[y-1],x$Stage_Date[y])%/% days(1)})),
    #                date = c(as.character(x$Stage_Date[1]), sapply(2:nrow(x), function(y) {as.character(x$Stage_Date[y])})),
    #                month = x$month[1],
    #                year = x$year[1],
    #                month_name = x$month_name[1]
    #     )
    #   }
    # })
    # do.call("rbind",.) %>% group_by(id, CareID) %>% mutate(days = wtime, 
    #                                                                NHSNumber = id,
    #                                                                total_wtime = cumsum(days)) %>% ungroup() %>%
    #   mutate(x = factor(paste0(month.abb[month], ", ", year), ordered = TRUE, levels = unique(paste0(month.abb[month], ", ", year))))
    # 

    
    # 
    # output$pMonthlyPerformance <- renderPlotly({
    #   # Patients starting in that month
    #   ggplotly({
    #     data2 %>% 
    #       group_by(year, month) %>% summarise(on_target = round(100*length(which(total_days < 28))/length(total_days)), month_name = unique(month_name)) %>% 
    #       ungroup() %>%
    #       mutate(x = factor(paste0(month.abb[month], ", ", year), ordered = TRUE, levels = paste0(month.abb[month], ", ", year)),
    #              text = paste("In ", x,", ", on_target, "% of patients received their diagnosis by day 28.")) %>%
    #       ggplot(aes(x = x, y = on_target, text = text)) + geom_col() + ylim(c(0,100)) + ylab("% Meeting Target") + xlab("Date") + ggtitle("Performance By Month")
    #     }, tooltip = 'text')
    # })
    # 
    # # browser()
    # 
    # output$pMonthlyMedian <- renderPlotly({
    #   ggplotly({
    #     data2 %>%
    #       group_by(year, month) %>% summarise(median = median(total_days)) %>%
    #       ungroup() %>%
    #       mutate(x = factor(paste0(month.abb[month], ", ", year), ordered = TRUE, levels = paste0(month.abb[month], ", ", year)),
    #              text = paste("In ", x,", the median pathway was ", median, ", days long.")) %>%
    #       ggplot(aes(x = x, y = median, text = text)) + geom_col() + ylim(c(0,100)) + 
    #       ylab("Pathway Length (Days)") + xlab("Date") + ggtitle("Pathway Length By Month") +
    #       coord_flip()
    #   }, tooltip = 'text')
    # })
    # 
    # output$pMonthlyMedianPath <- renderPlotly({
    #   ggplotly({
    #     d2_base %>% group_by(year, month, activity) %>%
    #       summarise(median = median(days)) %>% ungroup() %>%
    #       mutate(id = factor(paste0(month.abb[month], ", ", year), ordered = TRUE, levels = unique(paste0(month.abb[month], ", ", year)))) %>%
    #       mutate(Activity = factor(activity, ordered = TRUE, levels = rev(ref_levels))) %>%
    #       arrange(Activity, id) %>% group_by(year, month) %>%
    #       mutate(l_pos = cumsum(median), l_pos = l_pos*(-1)+max(l_pos)+median*(1/2),
    #              text = paste("Event: ", activity, "\nMedian Days Waited: ", median)) %>%
    #       ggplot(aes(x = factor(id), y = median, fill = Activity, text = text)) + geom_col() +
    #       geom_text(aes(x = factor(id), y = l_pos, label = median), vjust=-0.25) +
    #       coord_flip() + xlab("") + ylab("Date")  +
    #       scale_fill_viridis_d(begin = 1, end = 0) + ggtitle("Median Waittimes By Activity")
    #   }, tooltip = 'text')
    # })
    # 
    # output$pMonthlyMedianPathBreaching <- renderPlotly({
    #   ggplotly({
    #     d2_base %>% group_by(NHSNumber, CareID) %>% filter(max(total_wtime) > 28) %>% group_by(year, month, activity) %>%
    #       summarise(median = median(days)) %>% ungroup() %>%
    #       mutate(id = factor(paste0(month.abb[month], ", ", year), ordered = TRUE, levels = unique(paste0(month.abb[month], ", ", year)))) %>%
    #       mutate(Activity = factor(activity, ordered = TRUE, levels = rev(ref_levels))) %>%
    #       arrange(Activity, id) %>% group_by(year, month) %>%
    #       mutate(l_pos = cumsum(median), l_pos = l_pos*(-1)+max(l_pos)+median*(1/2),
    #              text = paste("Event: ", activity, "\nMedian Days Waited: ", median)) %>%
    #       ggplot(aes(x = factor(id), y = median, fill = Activity, text = text)) + geom_col() +
    #       geom_text(aes(x = factor(id), y = l_pos, label = median), vjust=-0.25) +
    #       coord_flip() + xlab("") + ylab("Date")  +
    #       scale_fill_viridis_d(begin = 1, end = 0) + ggtitle("Breaching Patients' Median Waittimes By Activity")
    #   }, tooltip = 'text')
    # })
    
    # data_monthly <- data2 %>% filter(Pathway_Stage_Description == "Referral Receipt") %>% 
    #   group_by(year, month) %>% summarise(on_target = round(100*length(which(total_days < 28))/length(total_days)), month_name = unique(month_name)) %>% 
    #   ungroup() %>%
    #   mutate(x = factor(paste0(month.abb[month], ", ", year), ordered = TRUE, levels = paste0(month.abb[month], ", ", year)))
    # 
    
    
    
    # output$monthlyTabsUI <- renderUI({
    #   tabsetPanel(type = "tabs",
    #               do.call(tabsetPanel, c(id='t',lapply(1:length(data_monthly$x), function(i) {
    #                 tabPanel(
    #                   title=data_monthly$x[i], 
    #                   plotlyOutput(paste0("montlyMap",i), height = paste0((d2_base %>% filter(x == data_monthly$x[i]) %>% 
    #                                                                         group_by(NHSNumber, CareID) %>% 
    #                                                                         filter(max(total_wtime) > 28) %>% summarise(n = n()) %>% nrow()) * 19 + 50, "px"))
    #                 )
    #               })))
    #   )
    # })
    
    
    # lapply(1:length(data_monthly$x), function(i) {
    #   output[[paste0("montlyMap",i)]] <- renderPlotly({
    #     ggplotly({
    #       d2_base %>% filter(x == data_monthly$x[i]) %>% 
    #         group_by(NHSNumber, CareID) %>% 
    #         filter(max(total_wtime) > 28) %>% # filter() %>% sample_frac(., ifelse(nrow(.) > 20, 0.1, 1)) %>%
    #         mutate(Activity = factor(activity, ordered = TRUE, levels = rev(ref_levels)),
    #                names = as.character(paste0(id, CareID))) %>%
    #         arrange(id, CareID, ymd(date)) %>%
    #         mutate(bar_length = cumsum(days),
    #                l_pos = bar_length - (days*(1/2)), # l_pos*(-1)+max(l_pos)+days*(1/2) #  
    #                text = paste("Event: ", activity, "\nDays Waited: ", days)) %>% ungroup() %>%
    #         ggplot(aes(x = names, y = days, fill = Activity, text = text)) + geom_col() +
    #         geom_text(aes(x = names, y = l_pos, label = days)) +
    #         geom_hline(yintercept = 28, linetype = 'dotted', col = 'red') +
    #         coord_flip() + xlab("") + ylab("Time (Days)")  +
    #         scale_fill_viridis_d(begin = 1, end = 0) + ggtitle("Patient Pathways")
    #     }, tooltip = 'text')
    # 
    #   })
    # })

    removeModal()
    
  }

shinyApp(ui = ui, server = server)

