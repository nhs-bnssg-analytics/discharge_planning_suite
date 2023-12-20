


# Load tabs

pTabUI <- function(x) {
  fluidRow(
    column(width = 12,
           textOutput(paste0(x,"tU1")),
           checkboxInput(paste0(x,"TraceMap"), "Show Trace Map"),
           uiOutput(paste0(x,"TraceUI")),
           h4("What are does a median pathway look like?"),
           fluidRow(
             column(width = 8,
                    plotlyOutput(paste0(x,"MedianTimes"), height = "120px")
                    
                    ),
             column(width = 4,
                    plotOutput(paste0(x,"MedianTimesLegend"))
                    
             )
             ),
           h4("10 example pathways of patients who breach."),
           plotlyOutput(paste0(x,"TopPatients"), height = "500px"),
           br(),
           h4("10 example pathways of patients who meet the 28 day target"),
           plotlyOutput(paste0(x,"BPatients"), height = "500px")
           )
  )
}


montlyTabUI <- function(x) {
  fluidRow(
    column(width = 12,
           uiOutput(paste0(x,"TraceUI")),
           h4("10 example pathways of patients who breach."),
           plotlyOutput(paste0(x,"TopPatients"), height = "500px"),
    )
  )
}




body <- dashboardBody(tabItems(
                        # tabItem(tabName="landing",
                        #         fluidRow(
                        #           box(width = 12,
                        #               fluidRow(
                        #                 column(width = 8,
                        #                        h4("Montly Performance")
                        #                 ),
                        #                 column(width = 4,
                        #                        div(img(src='nbt logo.png', height = '70px'),
                        #                            style="text-align: right;")
                        #                 )
                        #               )
                        #           )
                        #         ),
                        #         selectInput(inputId = 'dropdown1', label = 'Site', choices = c('BRI - UHBW'='bri', 'Southmead Hospital - North Bristol Trust' = 'nbt', 'WGH - UHBW'= 'weston', 'all' = 'all')),
                        #         selectInput(inputId = 'dropdown2', label = 'Variable', choices = c('var1'='var1_val', 'var2' = 'var2_val', 'var3'= 'var3_val', 'var4' = 'var4_val')),
                        #         fluidRow(
                        #           box(h4("Old Data"), 
                        #               "Disabled For Now",
                        #               dateRangeInput("old_dates", "Old Date Period"),
                        #               width = 4,
                        #           ),
                        #           
                        #           box(h4("New Data"),
                        #               dateRangeInput("new_dates", "New Date Period", start = "2020-10-01", end = "2021-09-30"),
                        #               width = 4
                        #           ),
                        #           box(width = 4, title = "Actions",
                        #               actionButton("filterDatesoverview", "Generate Output")
                        #           ),
                        #         ),
                        #         fluidRow(
                        #           box(width = 12,
                        #               plotlyOutput("pMonthlyPerformance"),
                        #               br(),
                        #               h3("The plots below show the median (average) pathway times."),
                        #               plotlyOutput("pMonthlyMedian"),
                        #               plotlyOutput("pMonthlyMedianPath"),
                        #               plotlyOutput("pMonthlyMedianPathBreaching"),
                        #               h3("Click the tabs to show patients breaching by month."),
                        #               uiOutput("monthlyTabsUI")
                        #           )
                        #         ),
                        # ),
                        tabItem(tabName="landing",
                                selectInput(inputId = 'dropdown1', label = 'Site', choices = c('BRI - UHBW'='bri', 'Southmead Hospital - North Bristol Trust' = 'nbt', 'WGH - UHBW'= 'weston', 'all' = 'all')),
                                # fluidRow(
                                #   column(width = 12,
                                #          "Lorem ipsum dolor sit amet, consectetur adipiscing elit, 
                                #          sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud 
                                #          exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in 
                                #          voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
                                #          ),
                                #   box(width = 12,
                                #       fluidRow(
                                #         column(width = 8,
                                #                h4("Montly Performance")
                                #         ),
                                #         column(width = 4,
                                #                div(img(src='nbt logo.png', height = '70px'),
                                #                    style="text-align: right;")
                                #         )
                                #       ),
                                #   ),
                                # ),
                                fluidRow(
                                  box(width = 12, 
                                      plotlyOutput("tPlotOutput1", height = "800px"),
                                      # plotlyOutput("tPlotOutput2"),
                                      br(),
                                      br(),
                                      br()
                                  )
                                ),
                                # lapply(1:3, function(y) {
                                #   fluidRow(
                                #     # fluidRow(
                                #     # box(width = 2),
                                #     # box(width = 2),
                                #     # box(width = 2),
                                #     # box(width = 2),
                                #     # box(width = 2),
                                #     # box(width = 2)
                                #     lapply(1:2, function(x) {
                                #       box(width = 2,
                                #           plotlyOutput(paste0("plot",y,x))
                                #           )
                                #     })
                                    
                                    # )
                                #   )
                                # }),
                                actionButton("buttona1", "Save Report")
                                # fluidRow(
                                #   # fluidRow(
                                #     # box(width = 2),
                                #     # box(width = 2),
                                #     # box(width = 2),
                                #     # box(width = 2),
                                #     # box(width = 2),
                                #     # box(width = 2)
                                #     lapply(1:6, function(x) {
                                #              box(width = 2)
                                #     })
                                #     
                                #   # )
                                # ),
                                # fluidRow(
                                #   column(width = 2,
                                #          box(width = 12)
                                #   ),
                                #   column(width = 2,
                                #          box(width = 12)
                                #   ),
                                #   column(width = 2,
                                #          box(width = 12)
                                #   ),
                                #   column(width = 2,
                                #          box(width = 12)
                                #   ),
                                #   column(width = 2,
                                #          box(width = 12)
                                #   ),
                                #   column(width = 2,
                                #          box(width = 12)
                                #   )
                                # ),
                                # fluidRow(
                                #   column(width = 2,
                                #          box(width = 12)
                                #   ),
                                #   column(width = 2,
                                #          box(width = 12)
                                #   ),
                                #   column(width = 2,
                                #          box(width = 12)
                                #   ),
                                #   column(width = 2,
                                #          box(width = 12)
                                #   ),
                                #   column(width = 2,
                                #          box(width = 12)
                                #   ),
                                #   column(width = 2,
                                #          box(width = 12)
                                #   )
                                # )
                                
                        )
                        #,
                        # tabItem(tabName="landing",
                        #         fluidRow(
                        #           box(width = 12,
                        #             fluidRow(
                        #               column(width = 8,
                        #                             h4("Prostate Cancer")
                        #                      ),
                        #             column(width = 4,
                        #                    div(img(src='nbt logo.png', height = '70px'),
                        #                        style="text-align: right;")
                        #             )
                        #             )
                        #           )
                        #         ),
                        # 
                        #         fluidRow(
                        #           box(h4("Old Data"), 
                        #               "Disabled For Now",
                        #               dateRangeInput("old_dates", "Old Date Period"),
                        #               width = 4,
                        #           ),
                        #           
                        #           box(h4("New Data"),
                        #               dateRangeInput("new_dates", "New Date Period", start = "2020-10-01", end = "2021-10-01"),
                        #               width = 4
                        #           ),
                        #           box(width = 4, title = "Actions",
                        #               actionButton("filterDatesoverview", "Generate Output")
                        #           ),
                        #         ),
                        #         
                        #         fluidRow(
                        #           #   # Performance
                        #           #   # Key numbers
                        #           ## Trace Map
                        #           ## Transition Times
                        #           ## Activity Count
                        #           ## Monthly activity count
                        #           ## Median Bar Chart
                        #           #   # Bar chart of mean times
                        #           #   # Trace map
                        #           # FDS reference
                        #           box(width = 12,
                        #               verbatimTextOutput("ptU1"),
                        #               tableOutput("ppertable"),
                        #               tabsetPanel(type = "tabs",
                        #                           id = "ptabs",
                        #                           tabPanel("Overview", pTabUI("overview")),
                        #                           tabPanel("First Seen", pTabUI("firstSeen")),
                        #                           tabPanel("MRI", pTabUI("mri")),
                        #                           tabPanel("Biopsy", pTabUI("biopsy"))
                        #                           #,
                        #                           # pTabUI("overview", "Overview"),
                        #                           # pTabUI("firstSeen", "First Seen"),
                        #                           # pTabUI("mri", "MRI"),
                        #                           # pTabUI("biopsy", "Biopsy")
                        #               )
                        #           )
                        #         )
                        # )
                        # tabItem(tabName="landing3",
                        #         fluidRow(
                        #           box(width = 12,
                        #               fluidRow(
                        #                 column(width = 8,
                        #                        h4("Prostate Cancer")
                        #                 ),
                        #                 column(width = 4,
                        #                        div(img(src='nbt logo.png', height = '70px'),
                        #                            style="text-align: right;")
                        #                 )
                        #               )
                        #           )
                        #         ),
                        #         fluidRow(
                        #           box(h4("Old Data"), 
                        #               dateRangeInput("old_dates", "Old Date Period"),
                        #               width = 4,
                        #           ),
                        #           box(h4("New Data"),
                        #               dateRangeInput("new_dates", "New Date Period", start = "2020-10-01", end = "2021-02-01"),
                        #               width = 4
                        #           ),
                        #           box(width = 4, title = "Actions",
                        #               actionButton("filterDatesoverview", "Filter")
                        #           )
                        #         ),
                        #         fluidRow(
                        #               tabsetPanel(type = "tabs",
                        # pTabUI("overView", "Overview"),
                        # pTabUI("firstSeen", "First Seen"),
                        # pTabUI("mri", "MRI"),
                        # pTabUI("biopsy", "Biopsy")
                        #               )
                        #           )
                        #         )
                        # )
                      ) 
)