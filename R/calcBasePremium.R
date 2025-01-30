#' Calculate Base Insurance Premium Using C++
#'
#' This function calculates insurance premiums based on risk factors including age,
#' BMI, smoking status, and pre-existing conditions. It uses C++ for computation
#' of risk scores and adjustments.
#'
#' @name calcBasePremiumCpp
#' @param df A data frame containing the following required columns:
#'   \itemize{
#'     \item age: Numeric vector of customer ages
#'     \item bmi: Numeric vector of customer BMI values
#'     \item smoking_status: Character vector with "y" for smokers, "n" for non-smokers
#'     \item pre_existing_conditions: Character vector of conditions (one of: "arthritis",
#'           "diabetes_type_2", "heart_disease", "asthma", "hypertension", "none")
#'     \item annual_premium: Numeric vector of current annual premiums
#'   }
#' @param age_base Base age value used as reference point for age adjustments (e.g., 30)
#' @param age_factor Multiplicative factor for age difference from base (e.g., 0.02)
#' @param bmi_base Base BMI value used as reference point for BMI adjustments (e.g., 25)
#' @param bmi_factor Multiplicative factor for BMI difference from base (e.g., 0.05)
#' @param smoker_penalty Additional risk factor applied to smokers (e.g., 0.5)
#' @param arthritis_factor Risk factor for arthritis condition (default: 0.0)
#' @param diabetes_factor Risk factor for type 2 diabetes condition (default: 0.0)
#' @param heart_disease_factor Risk factor for heart disease condition (default: 0.0)
#' @param asthma_factor Risk factor for asthma condition (default: 0.0)
#' @param hypertension_factor Risk factor for hypertension condition (default: 0.0)
#' @param none_factor Risk factor for no pre-existing conditions (default: 0.0)
#'
#' @return A data frame containing all original columns plus:
#'   \itemize{
#'     \item base_premium_rcpp: Calculated premium after risk adjustments
#'     \item risk_score: Overall risk score for each customer
#'   }
#'
#' @examples
#' \dontrun{
#' insurance_data <- data.frame(
#'   age = c(25, 45, 35),
#'   bmi = c(24, 28, 22),
#'   smoking_status = c("n", "y", "n"),
#'   pre_existing_conditions = c("none", "diabetes_type_2", "asthma"),
#'   annual_premium = c(1000, 1500, 1200)
#' )
#'
#' result <- calcBasePremiumCpp(
#'   df = insurance_data,
#'   age_base = 30,
#'   age_factor = 0.02,
#'   bmi_base = 25,
#'   bmi_factor = 0.05,
#'   smoker_penalty = 0.5,
#'   diabetes_factor = 0.3,
#'   asthma_factor = 0.2
#' )
#' }
#'
#' @export
#' @importFrom Rcpp sourceCpp
#' @useDynLib insurePriceR, .registration = TRUE
calcBasePremiumCpp
