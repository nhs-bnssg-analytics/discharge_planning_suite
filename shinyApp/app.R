library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(magrittr)
library(scales)
library(stringr)
library(lubridate)
library(RODBC)
library(ggiraph)
library(ggh4x)

# functions/data
source("./data_discharge_pathway_projections.R")
source("./functions.R")
source("./colour_functions.R")
source("./theme.R")

dpp_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    box(width = 12,
        tabsetPanel(id = ns("tabset"),
                    tabPanel("D2A Demand", 
                             girafeOutput(ns("dpp_plot"), width = "100%", height = "75vh")),
                    tabPanel("Acute Queueing",
                             fluidRow(
                               box(width = 3,
                                   HTML("<h2>Overview</h2><br><h5>Projections for the P1-3 queue size based on forecasted demand and capacity.</h5>"),
                                   radioButtons(
                                     ns("capacity"),
                                     label = h3("D2A daily discharge capacity"),
                                     choices = list("+10%" = 1, "4-week mean" = 2, "-10%" = 3),
                                     selected = 2
                                   )
                               ),
                               box(width = 9, 
                                   girafeOutput(ns("queue_fc"), width = "100%", height = "600px"))
                             )
                    )
        )
    )
  )
}


dpp_module_server <- function(id, data_subset, report_date) {
  moduleServer(id, function(input, output, session) {
    
    cols_curr <- c("NCTR but not on D2A queue" = "#999999", "P3 queue" = "#853358", "P2 queue" = "#003087", "P1 queue" = "#8d488d")
    cols_add  <- c("..for P1 service" = "#8d488d", "..for P2 service" = "#003087", "..for P3 service" = "#853358", "..not for D2A service" = "#999999")
    cols_q    <- c("P0 queue or other" = "#999999", "P3 queue" = "#853358", "P2 queue" = "#003087", "P1 queue" = "#8d488d")
    
    output$dpp_plot <- renderGirafe({
      # We call the reactive data: data_subset()
      p_pred <- data_subset() %>%
        mutate(pathway_add = fct_relevel(pathway_add, "..not for D2A service", after = Inf)) %>%
        mutate(day = report_date + ddays(day-1)) %>%
        filter(ctr == "Y", source == "model_pred") %>%
        ggplot(aes(x = day, y = n, fill = pathway_add, group = pathway_add)) +
        geom_col_interactive(aes(tooltip = tooltip_n), alpha = 0.75) +
        geom_hline_interactive(data = {data_subset() %>% filter(source == "queue_sim") %>% select(grp, pathway_add, slot_avg, tooltip_slot_avg) %>% distinct() %>% na.omit()}, 
                               aes(yintercept = slot_avg, tooltip = tooltip_slot_avg), linetype = 2, size = 1, col = "#333333") +
        geom_errorbar_interactive(aes(ymin = l85, ymax = u85, tooltip = tooltip_errorbar), width = 1, size = 0.8, col = "#333333")  +
        ggh4x::facet_grid2(grp~pathway_add, independent = "y", scales = "free_y", switch = "y") +
        bnssgtheme() +
        scale_x_datetime(date_breaks = "1 days", labels = date_format('%a %d')) +
        scale_fill_manual(values = cols_add, labels = (str_replace(str_replace_all(names(cols_add), r"(\.\.|for)", ""),"^\\w{1}", toupper))) +
        theme(strip.placement = "outside", legend.position = "bottom", axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
        labs(title = "Prediction of new patients becoming ready for discharge on future days**..", x = "", y = "")
      
      p_curr <- data_subset() %>%
        mutate(pathway_add = fct_recode(pathway_add, 
                                        "P1 queue" = "..for P1 service", 
                                        "P2 queue" = "..for P2 service", 
                                        "P3 queue" = "..for P3 service")) %>%
        mutate(pathway_add = fct_relevel(pathway_add, "P2 queue", "P1 queue", after = Inf)) %>%
        mutate(tooltip_n = glue::glue("{pathway_add} = {round(n, 0)}")) %>%
        filter(source == "current_ctr_data", ctr == "N") %>%
        ggplot(aes(x = grp, y = round(n, 0), fill = pathway_add, group = pathway_add)) +
        geom_col_interactive(aes(tooltip = tooltip_n), width = 0.7)  +
        coord_flip() +  # <--- THIS flips it to horizontal
        bnssgtheme() +
        scale_fill_manual(values = cols_curr, limits = rev(names(cols_curr))) +
        theme(legend.position = "none") +
        labs(title = "NCTR patient* queues today", x = "", y = "Patient Count")
      
      ptc <- (p_curr / p_pred) + 
        plot_layout(heights = c(2, 4), guides = "collect") & 
        patchwork::plot_annotation(caption = "*Meant to include all patients with LOS over 24 hrs.\n**Dashed line represents 4-week mean number of patients discharged to D2A pathway") &
        theme(legend.position = 'bottom')
      
      girafe(ggobj = ptc, width_svg = 16, height_svg = 10)
    })
    
    output$queue_fc <- renderGirafe({
      rename_vec <- switch(input$capacity,
                           "1" = c("n" = "n_u", "n_l85" = "n_u_l85", "n_u85" = "n_u_u85", "tooltip_q" = "tooltip_q_u"),
                           "2" = vector(mode = "character"),
                           "3" = c("n" = "n_l", "n_l85" = "n_l_l85", "n_u85" = "n_l_u85", "tooltip_q" = "tooltip_q_l"))
      
      select_vec <- if(input$capacity == "2") character(0) else c("n", "n_l85", "n_u85", "tooltip_q")
      
      p <- data_subset() %>%
        filter(source == "queue_sim") %>%
        select(-any_of(select_vec)) %>%
        mutate(day = report_date + ddays(day-1)) %>%
        rename(!!!rename_vec) %>%
        ggplot(aes(x = day, y = n, fill = pathway_q, group = pathway_q)) +
        geom_ribbon_interactive(aes(ymin = n_l85, ymax = n_u85), alpha = 0.33)  +
        geom_line_interactive(aes(col = pathway_q)) +
        geom_point_interactive(aes(tooltip = tooltip_q, col = pathway_q), size = 1.5) +
        ggh4x::facet_grid2(grp~pathway_q, scales = "free_y", switch = "y") +
        bnssgtheme() +
        scale_fill_manual(values = cols_q) +
        scale_colour_manual(values = cols_q) +
        theme(legend.position = "none") +
        labs(title = "D2A queue forecasts", x = "", y = "")
      
      girafe(ggobj = p, width_svg = 10, height_svg = 5)
    })
  })
}

ui <- dashboardPage(
  
  tags$style(HTML("
  .content-wrapper { background-color: #ffffff; }
  /* Force the box to be seamless */
  .box { border-top: none; box-shadow: none; margin-bottom: 0px; }
  /* Allow the SVG to scale nicely */
  .girafe_container_std { width: 100% !important; }
")),
  
  header = dashboardHeader(title = "BNSSG Discharge Suite", titleWidth = "250px"),
  
  # 1. Put the controls in the actual sidebar
  sidebar = dashboardSidebar(
    width = "250px",
    div(style = "padding: 20px;",
        # Date Display
        div(style = "margin-bottom: 25px;",
            HTML(glue::glue(
              "<label style= text-transform: uppercase; font-size: 10px; letter-spacing: 0.5px;'>Report updated</label><br>
               <span style='font-size: 18px; font-weight: bold;'>{format(report_date, '%a %d %b')}</span>"
            ))
        ),
        # The Selector
        radioButtons(
          "view_toggle",
          label = "Reporting Group:",
          choices = c("Local Authority" = "la", "Acute Hospital" = "acute"),
          inline = FALSE
        )
    )
  ), 
  
  body = dashboardBody(
    # 2. Add custom CSS to make the sidebar look like a clean panel
    tags$head(
      tags$style(HTML("
        /* Make sidebar background light instead of dark (optional) */
        .main-sidebar { background-color: #f4f4f4 !important; }
        .main-sidebar .sidebar { color: #333 !important; }
        .main-sidebar .sidebar label { color: #333 !important; }
        
        /* Adjust the body height and background */
        .content-wrapper { min-height: 100vh !important; background-color: #ecf0f5; }
        
        /* Fix radio button text color for light sidebar */
        .shiny-input-container { color: #333 !important; }
      "))
    ),

    # Main Content
    fluidRow(
      # The module UI now just lives here and fills the remaining space
      dpp_module_ui("main_dashboard")
    )
  )
)
server <- function(input, output, session) {
  
  # Reactive expression to determine which data to show
  filtered_data <- reactive({
    acute_grps <- c("NBT", "BRI", "Weston")
    la_grps    <- c("NSC", "BCC", "SGC")
    
    if (input$view_toggle == "acute") {
      data_dpp %>% filter(grp %in% acute_grps)
    } else {
      data_dpp %>% filter(grp %in% la_grps)
    }
  })
  
  # Call the module once, passing the reactive data
  dpp_module_server("main_dashboard", filtered_data, report_date)
}

shinyApp(ui, server)
