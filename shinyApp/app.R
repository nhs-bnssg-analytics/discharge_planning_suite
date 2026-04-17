library(shiny)
library(bslib)       
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

options(gfonts.cache = tempdir())

# functions/data
source("./data_discharge_pathway_projections.R")
source("./functions.R")
source("./colour_functions.R")
source("./theme.R")

dpp_module_ui <- function(id) {
  ns <- NS(id)
  
  card(
    full_screen = TRUE,
    class = "no-border",
    navset_card_underline(
      id = ns("tabset"),
      
      nav_panel(
        "D2A Demand",
        girafeOutput(ns("dpp_plot"), width = "100%", height = "75vh")
      ),
      
      nav_panel(
        "Acute Queueing",
        layout_columns(
          col_widths = c(8, 4),
          
          div(
            h2("Overview", style = "margin-top: 0;"),
            p(
              "Projections for the queue sizes for P1-3 based on the current position,
               the demand forecasted by the model, and a given capacity scenario
               (relative to currently outflow over 4-weeks).",
              style = "color: #666; font-size: 1.1em;"
            )
          ),
          
          div(
            style = "background: #f9f9f9; padding: 15px; border-radius: 8px;
                     border: 1px solid #ddd;",
            radioButtons(
              ns("capacity"),
              label = tags$span(
                "D2A daily discharge capacity",
                style = "font-weight: bold;"
              ),
              choices = list("+10%" = 1, "4-week mean" = 2, "-10%" = 3),
              selected = 2,
              inline = TRUE
            )
          )
        ),
        
        girafeOutput(ns("queue_fc"), width = "100%", height = "70vh")
      )
    )
  )
}

ui <- page_sidebar(
  
  title = "BNSSG Discharge Suite",
  
  theme = bs_theme(
    version = 5,
    bg        = "#ffffff",
    fg        = "#333333",
    primary   = "#003087",
    base_font = font_google("Inter", wght = c(400, 500))
  ),
  
  tags$head(
    tags$style(HTML("
      /* Sidebar */
      .bslib-sidebar-layout > .sidebar {
        background-color: #f4f4f4 !important;
        border-right: 1px solid #e9ecef !important;
      }

      /* Flat, borderless cards */
      .no-border {
        border: none !important;
        box-shadow: none !important;
      }
      .card {
        border: 0.5px solid #e0e0e0 !important;
        box-shadow: none !important;
        border-radius: 8px !important;
      }

      /* Tidy up radio buttons in sidebar */
      .shiny-input-container label { font-weight: 500; }
      .radio label { font-size: 14px; color: #333; }

      /* girafe fills its container */
      .girafe_container_std { width: 100% !important; }
    "))
  ),
  
  sidebar = sidebar(
    width = 250,
    bg = "#f4f4f4",
    padding = "20px",
    
    div(
      style = "margin-bottom: 25px;",
      tags$label(
        "Report updated",
        style = "text-transform: uppercase; font-size: 10px;
                 letter-spacing: 0.5px; color: #888;"
      ),
      br(),
      tags$span(
        format(report_date, "%a %d %b"),
        style = "font-size: 18px; font-weight: 500;"
      )
    ),
    
    radioButtons(
      "view_toggle",
      label = "Reporting group",
      choices = c("Local Authority" = "la", "Acute Hospital" = "acute"),
      inline = FALSE
    )
  ),
  
  dpp_module_ui("main_dashboard")
)

dpp_module_server <- function(id, data_subset, report_date) {
  moduleServer(id, function(input, output, session) {
    
    cols_curr <- c("NCTR but not on D2A queue" = "#999999", "P3 queue" = "#853358", "P2 queue" = "#003087", "P1 queue" = "#8d488d")
    cols_add  <- c("..for P1 service" = "#8d488d", "..for P2 service" = "#003087", "..for P3 service" = "#853358", "..not for D2A service" = "#999999")
    cols_q    <- c("P0 queue or other" = "#999999", "P3 queue" = "#853358", "P2 queue" = "#003087", "P1 queue" = "#8d488d")
    
    output$dpp_plot <- renderGirafe({
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
        coord_flip() +
        bnssgtheme() +
        scale_fill_manual(values = cols_curr, limits = rev(names(cols_curr))) +
        theme(legend.position = "none") +
        labs(title = "NCTR patient* queues today", x = "", y = "Patient Count")
      
      ptc <- (p_curr / p_pred) + 
        plot_layout(heights = c(2, 4), guides = "collect") & 
        patchwork::plot_annotation(caption = "*Meant to include all patients with LOS over 24 hrs.\n**Dashed line represents 4-week mean number of patients discharged to D2A pathway") &
        theme(legend.position = 'bottom')
      
      girafe(ggobj = ptc, width_svg = 16, height_svg = 10, fonts = list(sans = "Roboto, Arial, sans-serif"))
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
        ggplot(aes(x = day, y = n, fill = pathway_q)) +
        geom_col_interactive(aes(tooltip = tooltip_q), alpha = 0.8) +
        geom_errorbar_interactive(
          aes(ymin = n_l85, ymax = n_u85), 
          width = 0.3, 
          linewidth = 0.8
        ) +
        ggh4x::facet_grid2(grp ~ pathway_q, scales = "free_y", axes = "y", switch = "y") +
        ggh4x::facetted_pos_scales(
          y = list(
            pathway_q == "P1" ~ scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))),
            pathway_q == "P2" ~ scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))),
            pathway_q == "P3" ~ scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1)))
          )
        ) +
        bnssgtheme() +
        scale_fill_manual(values = cols_q) +
        scale_colour_manual(values = cols_q) +
        theme(
          legend.position = "none", 
          strip.placement = "outside",
          panel.grid.major.x = element_blank()
        ) +
        labs(title = "D2A queue forecasts", x = "", y = "Queue size")
      
      girafe(ggobj = p, width_svg = 16, height_svg = 6, fonts = list(sans = "Roboto, Arial, sans-serif"))
    })
  })
}

# server is unchanged
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    acute_grps <- c("NBT", "BRI", "Weston")
    la_grps    <- c("NSC", "BCC", "SGC")
    
    if (input$view_toggle == "acute") {
      data_dpp %>% filter(grp %in% acute_grps)
    } else {
      data_dpp %>% filter(grp %in% la_grps)
    }
  })
  
  dpp_module_server("main_dashboard", filtered_data, report_date)
}

shinyApp(ui, server)