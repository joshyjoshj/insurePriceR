
ui <- fluidPage(
  titlePanel("Analysis Dashboard"),

  sidebarLayout(
    sidebarPanel(
      # JB 25/01/2025 07:50:55 plot type selection
      selectInput("plot_type", "Select Analysis:",
                  choices = c("Premium Distribution" = "premium_dist",
                              "Risk Score Analysis" = "risk_analysis",
                              "Claims Analysis" = "claims_analysis")),
      # JB 25/01/2025 08:08:35 if distribution plot selected display premium type
      conditionalPanel(
        condition = "input.plot_type == 'premium_dist'",
        selectInput("premium_type", "Premium Type:",
                    choices = c("Annual Premium" = "annual_premium",
                                "Conservative" = "conservative_premium",
                                "Moderate" = "moderate_premium",
                                "Aggressive" = "aggressive_premium"))
      ),
      # JB 25/01/2025 07:55:00 if irsk plot selected display risk type
      conditionalPanel(
        condition = "input.plot_type == 'risk_analysis'",
        selectInput("risk_type", "Risk Score Type:",
                    choices = c("Conservative" = "conservative_risk_score",
                                "Moderate" = "moderate_risk_score",
                                "Aggressive" = "aggressive_risk_score"))
      ),
      # JB 25/01/2025 08:12:22 filter coverage type
      checkboxGroupInput("coverage_filter", "Coverage Level:",
                         choices = c("basic", "standard", "premium"),
                         selected = c("basic", "standard", "premium"))
    ),

    mainPanel(
      tabsetPanel(
        # JB 25/01/2025 08:16:02 tab panel for plot
        tabPanel("Visualizations",
                 plotlyOutput("main_plot", height = "500px"),
                 verbatimTextOutput("summary_stats")
        ),
        # JB 25/01/2025 08:18:25 tab panel for table
        tabPanel("Data Viewer",
                 DTOutput("data_table")
        )
      )
    )
  )
)
