options(shiny.autoreload = TRUE)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(magrittr)
library(scales)
library(stringr)
library(lubridate)
library(viridis)
library(RODBC)
library(ggiraph)
library(ggh4x)

# functions/data
source("./data_discharge_pathway_projections.R")
source("./functions.R")
source("./colour_functions.R")
source("./theme.R")


ui <- shinyUI(
  # fix the font
  
    dashboardPage(
    tags$head( tags$style(type="text/css", "text {font-family: sans-serif}")),
    header = dashboardHeader(  titleWidth="30vw",
                               title="BNSSG Discharge Pathway Projections"),
    sidebar = dashboardSidebar(disable = TRUE),
    
    body = dashboardBody(
      box(width = 12,
          HTML(glue::glue("<h5>These data were last updated: <b>{format(report_date, '%a %d %b')}</b>. For comments and suggestions, email Nick Howlett (Modelling and Analytics) by clicking <a href='mailto:nick.howlett5@nhs.net'>here.</a></h5>"))),
      box(width = 12,
          tabsetPanel(id = "tabset",
                      tabPanel("Discharges", girafeOutput("dpp_plot", width = "90%", height = "80%")),
                      tabPanel("Queuing", girafeOutput("queue_fc", width = "90%", height = "80%"))
                      )
      )
  )
))


server <- shinyServer(function(input, output) {
  
  cols_curr <- c(
    "NCTR but not on D2A queue" = "#999999",
    "P3 queue" = "#853358",
    "P2 queue" = "#003087",
    "P1 queue" = "#8d488d"
  )

  cols_add <- c(
    "NCTR but not\non D2A queue" = "#999999",
    "Additional P1" = "#8d488d",
    "Additional P2" = "#003087",
    "Additional P3" = "#853358"
  )
  
  cols_q <- c(
    "P0 queue or other" = "#999999",
    "P3 queue" = "#853358",
    "P2 queue" = "#003087",
    "P1 queue" = "#8d488d"
  )
  
  output$dpp_plot <- renderGirafe({
    p_pred <- data_dpp %>%
      # mutate(pathway = fct_relevel(pathway, names(levels), after = Inf)) %>%
      mutate(day = report_date + ddays(day+1)) %>%
      filter(ctr == "Y", source == "model_pred") %>%
      # ggplot(aes(x = fct_recode(ctr, "NCTR (with NCTR status)" = "N", "CTR (with CTR status)" = "Y"), y = n, fill = fct_rev(pathway))) +
      ggplot(aes(x = day, y = n, fill = pathway_add, group = pathway_add)) +
      geom_col_interactive(aes(tooltip = tooltip_n)) +
      geom_hline_interactive(data = {data_dpp %>%
          filter(source == "queue_sim") %>%
          select(site, pathway_add, slot_avg, tooltip_slot_avg) %>%
          distinct() %>%
          na.omit()}, aes(yintercept = slot_avg, tooltip = tooltip_slot_avg), linetype = 2, size = 1, col = "#333333") + 
      geom_errorbar_interactive(aes(ymin = l95, ymax = u95, tooltip = tooltip_errorbar), width = 1, size = 0.8, col = "#333333")  +
      ggh4x::facet_grid2(site~pathway_add, independent = "y", scales = "free_y", switch = "y") +
      bnssgtheme() +
      scale_x_datetime(date_breaks = "5 days", labels = date_format('%a\n%d %b')) +
      # this is a ugly hack to get the right labels/colours for the legend
      scale_fill_manual(values = cols_add, labels = (str_replace_all(names(cols_add), r"(\n)", " "))) +
      theme(strip.placement = "outside", legend.position = "bottom") +
      labs(title = "Prediction of new patients becoming ready for discharge on future days**",
           x = "",
           y = "")
    
    
    
    p_curr <- data_dpp %>%
      mutate(pathway_add = fct_recode(pathway_add,
                                  "P1 queue" = "Additional P1",
                                  "P2 queue" = "Additional P2",
                                  "P3 queue" = "Additional P3"
      )) %>%
      mutate(pathway_add = fct_relevel(pathway_add, "P2 queue", "P1 queue", after = Inf)) %>%
      # remake tooltip as levels changed
      mutate(tooltip_n = glue::glue("{pathway_add} = {round(n, 0)}")) %>%
      filter(source == "current_ctr_data", ctr == "N") %>%
      ggplot(aes(x = site, y = round(n, 0), 
                 fill = pathway_add, 
                 group = pathway_add)) +
      geom_col_interactive(aes(tooltip = tooltip_n))  +
      bnssgtheme() +
      theme(legend.position = "off") +
      scale_fill_manual(values = cols_curr, limits = rev(names(cols_curr))) +
      labs(title = "NCTR patients* today",
           #subtitle = str_wrap(glue::glue("CTR is broken down into those who we predict will be NCTR {report_date + ddays(1)} and not", 50)),
           fill = "D2A queue",
           x = "",
           y = ""
      )
    
    
    ptc <- patchwork::wrap_plots(p_curr, p_pred, widths = c(0.2, 0.8)) +
      patchwork::plot_layout(guides = "collect") &
      patchwork::plot_annotation(caption = "*Meant to include all patients with LOS over 24 hrs.\n**Dashed line represents 6-week average number of patients discharged to D2A pathway") &
      theme(legend.position = 'bottom',
            plot.caption = element_text(hjust = 0, size = rel(1.1)))
    
    # hack the first legend off
    ptc[[1]] <- ptc[[1]] + theme(legend.position = "off", axis.ticks.x =  element_blank(), axis.text.x = element_text(vjust = +15)) 
    
    girafe(ggobj = ptc, 
           width_svg = 16, 
           height_svg = 7.5,
           options = list(
             opts_hover(css = "fill: black;"),
             opts_hover_inv(css = "opacity: 0.1;")
           ))
  })
  
  output$queue_fc <- renderGirafe({

    
    
    p <- data_dpp %>%
      filter(source == "queue_sim") %>%
      mutate(day = report_date + ddays(day+1)) %>%
      ggplot(aes(x = day, y = n, fill = pathway_q, group = pathway_q)) +
      geom_ribbon_interactive(aes(ymin = l95, ymax = u95), alpha = 0.33)  +
      geom_line_interactive(aes(col = pathway_q)) +
      geom_point_interactive(aes(tooltip = tooltip_q, col = pathway_q), size = 1.5) +
      ggh4x::facet_grid2(site~pathway_q, independent = "y", scales = "free_y", switch = "y") +
      bnssgtheme() +
      scale_x_datetime(date_breaks = "3 days", labels = date_format('%a\n%d %b')) +
      scale_fill_manual(values = cols_q) +
      scale_colour_manual(values = cols_q) +
      theme(strip.placement = "outside",
            legend.position = "off") +
      labs(title = "D2A queue forecasts, per site/pathway",
           x = "",
           y = "") 
    
    girafe(ggobj = p, 
           width_svg = 12, 
           height_svg = 5,
           options = list(
             opts_hover(css = "fill: black;"),
             opts_hover_inv(css = "opacity: 0.1;")
           ))
    })
  
})


shinyApp(ui = ui, server = server)