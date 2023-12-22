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
          HTML(glue::glue("<h5>Here you can visualise currently admitted patients by CTR status. Shown on the left is the count of patients broken down by CTR status and coloured by the pathway queue they are on. Included in the count of patients with CTR are predictions for those likely to be NTCR by {report_date + ddays(1)}. More detail on these predictions are shown in the chart on the right.<br>
                                For comments and suggestions, email Nick Howlett (Modelling and Analytics) by clicking <a href='mailto:nick.howlett5@nhs.net'>here.</a><br><br>", 
                          "These data were last updated: <b>{report_date}</b></h5>"))),
      box(width = 12, girafeOutput("dpp_plot"))
  )
)))


server <- shinyServer(function(input, output) {
  
  cols <- c("#853358", "#003087", "#8AC0E5", "#8d488d", "#999999")
  cols <- set_names(cols, c("NTCR but not on D2A queue","P1 queue", "P2 queue", "P3 queue", names(levels)))
  
  x_labs <- c("N", "Y")
  x_labs <- set_names(x_labs, c("NCTR\n(with NCTR status)", glue::glue("CTR\n(with CTR status and\npredicted status {report_date+ddays(1)})")))
  
  
  output$dpp_plot <- renderGirafe({
    p_tot <- data_dpp %>%
      mutate(pathway = fct_relevel(pathway, names(levels), after = Inf)) %>%
      filter(!(ctr == "Y" & source == "current_ctr_data")) %>%
      ggplot(aes(x = fct_recode(ctr, !!!x_labs), y = round(n, 0), 
      # ggplot(aes(x = fct_recode(ctr, "NCTR\n(with NCTR status)" = "N", "CTR\n(with CTR status)" = "Y"), y = round(n, 0), 
                 fill = fct_rev(pathway), 
                 group = fct_rev(pathway))) +
      geom_col_interactive(aes(tooltip = tooltip_n))  +
      bnssgtheme() +
      theme(legend.position = "bottom") +
      scale_fill_manual(values = unname(cols),
                        breaks = names(cols),
                        labels = c("NTCR but not on D2A queue","P1 queue", "P2 queue", "P3 queue", names(levels))) +
      labs(title = "Acute patients* in BNSSG system**\nby CTR status",
           #subtitle = str_wrap(glue::glue("CTR is broken down into those who we predict will be NCTR {report_date + ddays(1)} and not", 50)),
           fill = "D2A queue",
           x = "",
           y = ""
      )
    
    
    
    p_pred <- data_dpp %>%
      filter(source == "model_pred") %>%
      filter(pathway %in% c("P1 queue", "P2 queue", "P3 queue")) %>%
      ggplot(aes(x = fct_recode(pathway, "P1" = "P1 queue", "P2" = "P2 queue", "P3" = "P3 queue"), y = round(n, 0), fill = pathway)) +
      geom_col_interactive(aes(tooltip = tooltip_n_noqueue)) + 
      geom_errorbar_interactive(aes(ymin = l95, ymax = u95, tooltip = tooltip_errorbar), width = 0.5)  +
      bnssgtheme() +
      theme(legend.position = "off") +
      scale_fill_manual(values = c("NTCR but not on D2A queue" = "#853358", "P3 queue" = "#8d488d", "P2 queue" = "#8AC0E5", "P1 queue" = "#003087")) +
      labs(title = glue::glue("Current CTR population\npredicted NCTR by {report_date + ddays(1)}"),
           x = "",
           y = "")
    
    
    ptc <- patchwork::wrap_plots(p_tot, p_pred) +
      patchwork::plot_layout(guides = "collect") &
      patchwork::plot_annotation(caption = "*Meant to include all patients with LOS over 24 hrs. Data supplied by trusts in daily flows, however we are aware of DQ issues and some patients\nwill be missing.\n
                          **BRI, Southmead, & Weston.") &
      theme(legend.position = 'bottom',
            plot.caption = element_text(hjust = 0, size = rel(1.1)))
    
    
    ptc[[2]] <- ptc[[2]] +
      theme(legend.position = "off")
    
    girafe(ggobj = ptc , 
           width_svg = 12, 
           height_svg = 6,
           options = list(
             opts_hover(css = "fill: black;"),
             opts_hover_inv(css = "opacity: 0.1;")
           ))
  })
  
})


shinyApp(ui = ui, server = server)