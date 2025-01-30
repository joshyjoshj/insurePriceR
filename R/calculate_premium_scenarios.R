#' Calculate Premiums for Multiple Risk Scenarios
#'
#' @description
#' Calculates insurance premiums and risk scores for multiple predefined scenarios
#' (conservative, moderate, and aggressive) using different risk factor combinations.
#' Each scenario applies different weightings to age, BMI, smoking status, and
#' pre-existing conditions.
#'
#' @param df A data frame containing the following required columns:
#'   \itemize{
#'     \item age: Numeric vector of customer ages
#'     \item bmi: Numeric vector of customer BMI values
#'     \item smoking_status: Character vector with "y" for smokers, "n" for non-smokers
#'     \item pre_existing_conditions: Character vector of conditions (one of: "arthritis",
#'           "diabetes_type_2", "heart_disease", "asthma", "hypertension", "none")
#'     \item annual_premium: Numeric vector of current annual premiums
#'   }
#' @param scenarios Optional list of custom scenarios. If NULL, uses default scenarios.
#'   Each scenario should be a list containing the following elements:
#'   \itemize{
#'     \item age_base: Base age for calculations
#'     \item age_factor: Factor to adjust for age differences
#'     \item bmi_base: Base BMI for calculations
#'     \item bmi_factor: Factor to adjust for BMI differences
#'     \item smoker_penalty: Additional risk factor for smokers
#'     \item arthritis_factor: Risk factor for arthritis
#'     \item diabetes_factor: Risk factor for diabetes
#'     \item heart_disease_factor: Risk factor for heart disease
#'     \item asthma_factor: Risk factor for asthma
#'     \item hypertension_factor: Risk factor for hypertension
#'     \item none_factor: Risk factor (usually negative) for no conditions
#'   }
#'
#' @return A data frame containing all original columns plus additional columns for each scenario:
#'   \itemize{
#'     \item conservative_premium: Premium calculated using conservative risk factors
#'     \item conservative_risk_score: Risk score under conservative scenario
#'     \item moderate_premium: Premium calculated using moderate risk factors
#'     \item moderate_risk_score: Risk score under moderate scenario
#'     \item aggressive_premium: Premium calculated using aggressive risk factors
#'     \item aggressive_risk_score: Risk score under aggressive scenario
#'   }
#'
#' @examples
#' \dontrun{
#' # Create sample data
#' test_data <- data.frame(
#'   age = c(25, 45, 35),
#'   bmi = c(24, 28, 22),
#'   smoking_status = c("n", "y", "n"),
#'   pre_existing_conditions = c("none", "diabetes_type_2", "asthma"),
#'   annual_premium = c(1000, 1500, 1200)
#' )
#'
#' # Calculate premiums using default scenarios
#' results <- calculate_premium_scenarios(test_data)
#'
#' # View results
#' head(results)
#' }
#'
#' @export
calculate_premium_scenarios <- function(df, scenarios = NULL) {
  # JB 21/01/2025 16:19:36 check columns + return errors
  required_cols <- c("age", "bmi", "smoking_status",
                     "pre_existing_conditions", "annual_premium")

  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }

  # JB 21/01/2025 16:25:53 cehck data types + return errord
  if (!is.numeric(df$age)) stop("age must be numeric")
  if (!is.numeric(df$bmi)) stop("bmi must be numeric")
  if (!is.numeric(df$annual_premium)) stop("annual_premium must be numeric")


  valid_conditions <- c("arthritis", "diabetes_type_2", "heart_disease",
                        "asthma", "hypertension", "none")
  if (!all(df$pre_existing_conditions %in% valid_conditions)) {
    stop("Invalid pre_existing_conditions found. Valid values are: ",
         paste(valid_conditions, collapse = ", "))
  }

  # JB 21/01/2025 16:32:09 default scenarios
  if (is.null(scenarios)) {
    scenarios <- list(
      conservative = list(
        age_base = 35,
        age_factor = 0.02,  # JB 21/01/2025 16:37:12 2% increase per year from base
        bmi_base = 25,      # JB 21/01/2025 16:37:31 Normal BMI reference point
        bmi_factor = 0.03,  # JB 21/01/2025 16:38:09 3% increase per BMI point from base
        smoker_penalty = 0.5,  # JB 21/01/2025 16:39:48 50% increase for smokers
        arthritis_factor = 0.1,
        diabetes_factor = 0.2,
        heart_disease_factor = 0.3,
        asthma_factor = 0.15,
        hypertension_factor = 0.25,
        none_factor = 0
      ),
      moderate = list(
        age_base = 30,
        age_factor = 0.025,
        bmi_base = 23,
        bmi_factor = 0.04,
        smoker_penalty = 0.75,
        arthritis_factor = 0.15,
        diabetes_factor = 0.3,
        heart_disease_factor = 0.4,
        asthma_factor = 0.2,
        hypertension_factor = 0.35,
        none_factor = -0.05
      ),
      aggressive = list(
        age_base = 25,
        age_factor = 0.03,
        bmi_base = 22,
        bmi_factor = 0.05,
        smoker_penalty = 1.0,
        arthritis_factor = 0.2,
        diabetes_factor = 0.4,
        heart_disease_factor = 0.5,
        asthma_factor = 0.25,
        hypertension_factor = 0.45,
        none_factor = -0.1
      )
    )
  }

  # JB 21/01/2025 16:55:47 check scenarios + errors
  required_scenario_params <- c("age_base", "age_factor", "bmi_base",
                                "bmi_factor", "smoker_penalty")

  for (scenario_name in names(scenarios)) {
    scenario <- scenarios[[scenario_name]]
    missing_params <- setdiff(required_scenario_params, names(scenario))
    if (length(missing_params) > 0) {
      stop("Scenario '", scenario_name, "' is missing required parameters: ",
           paste(missing_params, collapse = ", "))
    }
  }

  # JB 21/01/2025 17:02:14 loop through and calc scenarios
  results <- df

  for (scenario_name in names(scenarios)) {
    scenario <- scenarios[[scenario_name]]

    # JB 21/01/2025 17:24:19 set defaults for null params
    scenario$arthritis_factor <- scenario$arthritis_factor %||% 0
    scenario$diabetes_factor <- scenario$diabetes_factor %||% 0
    scenario$heart_disease_factor <- scenario$heart_disease_factor %||% 0
    scenario$asthma_factor <- scenario$asthma_factor %||% 0
    scenario$hypertension_factor <- scenario$hypertension_factor %||% 0
    scenario$none_factor <- scenario$none_factor %||% 0

    # JB 21/01/2025 17:38:06 calc premium
    scenario_results <- calcBasePremiumCpp(
      df = df,
      age_base = scenario$age_base,
      age_factor = scenario$age_factor,
      bmi_base = scenario$bmi_base,
      bmi_factor = scenario$bmi_factor,
      smoker_penalty = scenario$smoker_penalty,
      arthritis_factor = scenario$arthritis_factor,
      diabetes_factor = scenario$diabetes_factor,
      heart_disease_factor = scenario$heart_disease_factor,
      asthma_factor = scenario$asthma_factor,
      hypertension_factor = scenario$hypertension_factor,
      none_factor = scenario$none_factor
    )

    # JB 21/01/2025 17:44:26 add into results
    results[[paste0(scenario_name, "_premium")]] <- scenario_results$base_premium_rcpp
    results[[paste0(scenario_name, "_risk_score")]] <- scenario_results$risk_score
  }

  # JB 21/01/2025 17:47:38 define columns
  original_cols <- setdiff(names(df),
                           c("base_premium_rcpp", "risk_score"))
  scenario_cols <- grep("_(premium|risk_score)$",
                        names(results), value = TRUE)

  scenario_cols <- setdiff(scenario_cols, "annual_premium")


  results[c(original_cols, scenario_cols)]
}

# JB 21/01/2025 17:19:20 func to return y if x null else x
`%||%` <- function(x, y) if (is.null(x)) y else x
