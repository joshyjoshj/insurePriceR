server <- function(input, output, session) {

  prem_scenarios <- getOption("app_data")
  filtered_data <- reactive({
    prem_scenarios %>%
      filter(coverage_level %in% input$coverage_filter)
  })
  output$main_plot <- renderPlotly({
    data <- filtered_data()
    # JB 25/01/2025 08:45:53 render plots based on plot input
    if(input$plot_type == "premium_dist") {
      p <- ggplot(data) +
        aes(x = .data[[input$premium_type]], fill = coverage_level) +
        geom_histogram(bins = 30) +
        theme_minimal() +
        labs(title = paste("Distribution of", gsub("", " ", input$premium_type)),
             x = "Amount",
             y = "Count")
    } else if(input$plot_type == "risk_analysis") {
      p <- ggplot(data) +
        aes(x = age, y = .data[[input$risk_type]], color = coverage_level) +
        geom_point() +
        geom_smooth(method = "lm") +
        theme_minimal() +
        labs(title = paste("Risk Score vs Age -", gsub("", " ", input$risk_type)),
             x = "Age",
             y = "Risk Score")
    } else {
      p <- ggplot(data) +
        aes(x = coverage_level, y = total_claims_amount, fill = coverage_level) +
        geom_boxplot() +
        theme_minimal() +
        labs(title = "Claims Amount by Coverage Level",
             x = "Coverage Level",
             y = "Total Claims Amount")
    }
    ggplotly(p)
  })
  # JB 25/01/2025 09:52:28 calculate summary stats from filtered data
  output$summary_stats <- renderPrint({
    data <- filtered_data()
    if(input$plot_type == "premium_dist") {
      summary_col <- input$premium_type
    } else if(input$plot_type == "risk_analysis") {
      summary_col <- input$risk_type
    } else {
      summary_col <- "total_claims_amount"
    }
    cat("Summary Statistics:\n")
    summary(data[[summary_col]])
  })
  # JB 25/01/2025 09:57:27 redner the datatable
  output$data_table <- renderDT({
    datatable(filtered_data(),
              options = list(pageLength = 10,
                             scrollX = TRUE),
              filter = "top")
  })
}
