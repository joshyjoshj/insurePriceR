#' Launch Scenario Dashboard
#'
#' @param data A data frame containing insurance premium scenarios data.
#'   Must contain columns: age, coverage_level, annual_premium,
#'   conservative_premium, moderate_premium, aggressive_premium,
#'   conservative_risk_score, moderate_risk_score, aggressive_risk_score,
#'   total_claims_amount
#' @export
#' @import shiny
#' @import plotly
#' @import DT
#' @import tidyverse
run_dashboard <- function(data) {
  # JB 25/01/2025 10:17:26 check df and columns + return errors
  if (missing(data)) {
    stop("Please provide a data frame containing premium scenarios data")
  }

  required_cols <- c("age", "coverage_level", "annual_premium",
                     "conservative_premium", "moderate_premium", "aggressive_premium",
                     "conservative_risk_score", "moderate_risk_score",
                     "aggressive_risk_score", "total_claims_amount")

  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  # JB 25/01/2025 10:49:30 sav data in global options
  options(app_data = data)

  # JB 25/01/2025 11:04:52 get directory of app or return error
  appDir <- system.file("shiny/dashboard", package = "insurePriceR")
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing `insurePriceR`.", call. = FALSE)
  }

  # JB 25/01/2025 11:18:35 remove data from options on exit and load packages
  on.exit(options(insurance_app_data = NULL))

  require(plotly)
  require(DT)
  require(tidyverse)

  shiny::runApp(appDir)
}
