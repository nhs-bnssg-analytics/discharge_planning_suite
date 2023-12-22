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

# functions/data
source("./data_discharge_pathway_projections.R")
source("./functions.R")
source("./colour_functions.R")
source("./theme.R")


ui <- shinyUI(fluidPage(
  # fix the font
  tags$head( tags$style(type="text/css", "text {font-family: sans-serif}")),
  
    dashboardPage(
    header = dashboardHeader(  titleWidth="30vw",
                               title="BNSSG Discharge Pathway Projections"),
    sidebar = dashboardSidebar(disable = TRUE),
    
    body = dashboardBody(
      box(width = 12,
          HTML(glue::glue("<h5>Here you can visualise currently admitted patients by CTR status. Shown on the left is the count of patients broken down by CTR status and coloured by the pathway queue they are on. Included in the count of patients with CTR are predictions for those likely to be NTCR tomorrow. More detail on these predictions are shown in the chart on the right.<br>
                                For comments and suggestions, email Nick Howlett (Modelling and Analytics) by clicking <a href='mailto:nick.howlett5@nhs.net'>here.</a><br><br>", 
                          "These data were last updated: <b>{report_date}</b></h5>"))),
      box(width = 12, girafeOutput("dpp_plot"))
      )
  )
))


server <- shinyServer(function(input, output) {

  
  output$dpp_plot <- renderGirafe({
    p_tot <- data_dpp %>%
      filter(!(ctr == "Y" & source == "current_ctr_data")) %>%
      pivot_wider(names_from = metric,
                  values_from = value) %>%
      mutate(pathway = fct_reorder(pathway, n),
             pathway = fct_relevel(pathway, rev(c("Other", "P1", "P2", "P3", "Not tomorrow")), after = 0),
             tooltip_n = round(n, 1)) %>%
      ggplot(aes(x = fct_recode(ctr, "NCTR" = "N", "CTR" = "Y"), y = n, fill = pathway)) +
      geom_col_interactive(aes(tooltip = tooltip_n)) +
      bnssgtheme() +
      theme(legend.position = "bottom") +
      scale_fill_bnssg() +
      labs(title = "Current patients by CTR status",
           #subtitle = str_wrap("CTR is broken down into those who we predict will be NCTR tomorrow and not", 50),
           x = "",
           y = ""
      )
    
    
    
    p_pred <- data_dpp %>%
      pivot_wider(names_from = metric,
                  values_from = value) %>%
      filter(source == "model_pred") %>%
      mutate(tooltip_n = round(n, 1),
             tooltip_errorbar = glue::glue("({round(u95,1)}, {round(l95,1)})")) %>%
      # mutate(pathway = factor(pathway, levels = (c("Other", "P1", "P2", "P3", "Not tomorrow")))) %>%
      # filter(los_remaining == 0) %>%
      filter(pathway %in% c("P1", "P2", "P3")) %>%
      ggplot(aes(x = pathway, y = n, fill = pathway)) +
      geom_col_interactive(aes(tooltip = tooltip_n)) + 
      geom_errorbar_interactive(aes(ymin = l95, ymax = u95, tooltip = tooltip_errorbar), width = 0.5)  +
      bnssgtheme() +
      theme(legend.position = "off") +
      scale_fill_manual(values = c("Other" = "#853358", "P3" = "#8d488d", "P2" = "#8AC0E5", "P1" = "#003087")) +
      labs(title = "Predicted NCTR tomorrow",
           x = "",
           y = "")
    
    
    ptc <- patchwork::wrap_plots(p_tot, p_pred) +
      patchwork::plot_layout(guides = "collect") &
      # patchwork::plot_annotation(title = "Current patients by NCTR status") &
      theme(legend.position = 'bottom')
    
    
    ptc[[2]] <- ptc[[2]] +
      theme(legend.position = "off")
    
    girafe(ggobj = ptc , 
           width_svg = 10, 
           height_svg = 4,
           options = list(
             opts_hover(css = "fill: black;"),
             opts_hover_inv(css = "opacity: 0.1;")
           ))
  })
  
})


shinyApp(ui = ui, server = server)