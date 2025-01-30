#' Constants used across prprocessing functions
#' @keywords internal
UNKNOWN_VALUES <- c(NA_character_, "NA", "na", "", "Unknown") # JB 18/01/2025 08:49:09 define paramters for key dasta and unkown params
MIN_AGE <- 18
MAX_AGE <- 100
MIN_BMI <- 16
MAX_COMPANY_NAME_LENGTH <- 12

#' Clean and standardize strings
#' @param x Character vector to clean
#' @return Cleaned character vector
#' @keywords internal
clean_string <- function(x) {
  stringr::str_trim(stringr::str_to_lower(as.character(x))) # JB 18/01/2025 08:53:19 to char -> to lowercase -> reomve blankspace
}

#' Replace unknown values with standardized "unknown"
#' @param x Character vector to process
#' @return Character vector with standardized unknown values
#' @keywords internal
replace_unknown <- function(x) {
  dplyr::if_else(x %in% UNKNOWN_VALUES, "unknown", clean_string(x)) # JB 18/01/2025 08:59:21 clean string or define as unknown
}

#' Impute median values with bounds checking
#' @param x Numeric vector to impute
#' @param min_val Minimum allowed value
#' @param max_val Maximum allowed value
#' @return Imputed numeric vector
#' @keywords internal
impute_median <- function(x, min_val = -Inf, max_val = Inf) {
  median_val <- stats::median(x[x >= min_val & x <= max_val], na.rm = TRUE) # JB 18/01/2025 09:18:42 median function with bounds
  dplyr::if_else(is.na(x) | x < min_val | x > max_val, median_val, x) # JB 18/01/2025 09:27:02 median or x if out of bounds or Na
}

#' Preprocess customer data
#'
#' @description
#' Preprocesses customer data by cleaning, standardizing, and imputing values.
#' Handles age, BMI, gender, smoking status, and pre-existing conditions.
#'
#' @param customers A data frame containing customer information with columns:
#'   \itemize{
#'     \item age: numeric or character, customer age
#'     \item bmi: numeric or character, body mass index
#'     \item gender: character, customer gender
#'     \item smoking_status: character, smoking status
#'     \item pre_existing_conditions: character, health conditions
#'   }
#'
#' @return A preprocessed data frame with standardized and cleaned customer data
#'
#' @details
#' The function performs the following operations:
#' \itemize{
#'   \item Removes duplicate records
#'   \item Standardizes text fields
#'   \item Imputes missing values
#'   \item Converts categorical variables to factors
#'   \item Filters age to valid range (18-100)
#' }
#'
#' @examples
#' \dontrun{
#' customers_df <- data.frame(
#'   age = c("25", "NA", "45"),
#'   bmi = c("22.5", "NA", "25.0"),
#'   gender = c("Male", "NA", "Female"),
#'   smoking_status = c("Non-smoker", "Unknown", "Smoker"),
#'   pre_existing_conditions = c("None", "NA", "Diabetes")
#' )
#' processed_customers <- pre_process_customers(customers_df)
#' }
#'
#' @importFrom magrittr %>%
#'
#' @export
pre_process_customers <- function(customers) {
  # JB 18/01/2025 09:31:45 verify dataframe inout
  if (!is.data.frame(customers)) {
    stop("Input must be a data frame", call. = FALSE)
  }
  # JB 18/01/2025 09:35:54 verify column names present
  required_cols <- c("age", "bmi", "gender", "smoking_status", "pre_existing_conditions")
  missing_cols <- setdiff(required_cols, names(customers))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  customers %>%
    dplyr::distinct() %>% # JB 18/01/2025 09:37:53 remove duplicates
    dplyr::mutate(
      dplyr::across(c(age, bmi), ~ readr::parse_number(as.character(.))), # JB 18/01/2025 09:43:52 characters to numbers
      dplyr::across(c(customer_id, policy_id, gender, smoking_status, pre_existing_conditions), clean_string)
    ) %>%
    dplyr::filter(dplyr::between(age, MIN_AGE, MAX_AGE)) %>% # JB 18/01/2025 09:49:19 age filter
    dplyr::mutate(
      dplyr::across(c(gender, smoking_status, pre_existing_conditions), replace_unknown), # JB 18/01/2025 10:03:31 replace unkwns
      age = impute_median(age, MIN_AGE, MAX_AGE), # JB 18/01/2025 10:08:24 impute median
      bmi = impute_median(bmi, MIN_BMI),
      dplyr::across(c(gender, smoking_status, pre_existing_conditions), factor) # JB 18/01/2025 10:03:46 to factor
    )
}

#' Preprocess insurance policy data
#'
#' @description
#' Preprocesses insurance policy data by cleaning, standardizing, and imputing values.
#'
#' @param policies A data frame containing policy information with columns:
#'   \itemize{
#'     \item policy_id: character, unique policy identifier
#'     \item coverage_level: character, level of coverage
#'     \item location: character, policy location
#'     \item company_name: character, insurance company name
#'     \item annual_premium: numeric or character, yearly premium amount
#'     \item excess: numeric or character, excess amount
#'   }
#'
#' @return A preprocessed data frame with standardized and cleaned policy data
#'
#' @details
#' The function performs the following operations:
#' \itemize{
#'   \item Removes duplicate records
#'   \item Standardizes text fields
#'   \item Truncates company names to 12 characters
#'   \item Imputes missing numeric values
#'   \item Converts categorical variables to factors
#' }
#'
#' @examples
#' \dontrun{
#' policies_df <- data.frame(
#'   policy_id = c("POL001", "POL002", "POL003"),
#'   coverage_level = c("Basic", "Premium", "NA"),
#'   location = c("NY", "CA", "Unknown"),
#'   company_name = c("InsureCo", "NA", "SafeGuard"),
#'   annual_premium = c("1000", "NA", "1500"),
#'   excess = c("500", "NA", "750")
#' )
#' processed_policies <- pre_process_policies(policies_df)
#' }
#'
#' @export
pre_process_policies <- function(policies) {
  # JB 18/01/2025 10:15:10 verify datafame input
  if (!is.data.frame(policies)) {
    stop("Input must be a data frame", call. = FALSE)
  }
  # JB 18/01/2025 10:19:00 check required cols
  required_cols <- c("policy_id", "coverage_level", "location",
                     "company_name", "annual_premium", "excess")
  missing_cols <- setdiff(required_cols, names(policies))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  policies %>% # JB 18/01/2025 10:22:13 same process as pre_process_customers()
    dplyr::distinct() %>%
    dplyr::mutate(
      dplyr::across(c(policy_id, coverage_level, location), clean_string),
      company_name = stringr::str_sub(clean_string(company_name), 1, MAX_COMPANY_NAME_LENGTH), # JB 18/01/2025 11:13:56 substr of cleaene string
      dplyr::across(c(annual_premium, excess), ~ readr::parse_number(as.character(.)))
    ) %>%
    dplyr::filter(!is.na(company_name)) %>%
    dplyr::mutate(
      location = replace_unknown(location),
      dplyr::across(c(annual_premium, excess), impute_median),
      dplyr::across(c(coverage_level, location), factor)
    )
}

#' Preprocess insurance claims data
#'
#' @description
#' Preprocesses insurance claims data by cleaning, standardizing, and imputing values.
#'
#' @param claims A data frame containing claims information with columns:
#'   \itemize{
#'     \item claim_id: character, unique claim identifier
#'     \item customer_id: character, customer identifier
#'     \item type_of_service: character, service type
#'     \item claim_amount: numeric or character, claim amount
#'     \item date_of_service: character or Date, service date
#'   }
#'
#' @return A preprocessed data frame with standardized and cleaned claims data
#'
#' @details
#' The function performs the following operations:
#' \itemize{
#'   \item Removes duplicate records
#'   \item Standardizes text fields
#'   \item Converts dates to proper format
#'   \item Imputes missing claim amounts by service type
#'   \item Converts categorical variables to factors
#'   \item Sorts by date of service
#' }
#'
#' @examples
#' \dontrun{
#' claims_df <- data.frame(
#'   claim_id = c("CLM001", "CLM002", "CLM003"),
#'   customer_id = c("CUS001", "CUS002", "CUS003"),
#'   type_of_service = c("Medical", "Dental", "NA"),
#'   claim_amount = c("1000", "NA", "750"),
#'   date_of_service = c("2023-01-01", "2023-02-01", "2023-03-01")
#' )
#' processed_claims <- pre_process_claims(claims_df)
#' }
#'
#' @export
pre_process_claims <- function(claims) {
  if (!is.data.frame(claims)) {
    stop("Input must be a data frame", call. = FALSE)
  }

  required_cols <- c("claim_id", "customer_id", "type_of_service",
                     "claim_amount", "date_of_service")
  missing_cols <- setdiff(required_cols, names(claims))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  claims %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      dplyr::across(c(claim_id, customer_id), clean_string),
      type_of_service = clean_string(type_of_service),
      claim_amount = readr::parse_number(as.character(claim_amount)),
      date_of_service = as.Date(date_of_service)
    ) %>%
    dplyr::mutate(
      type_of_service = replace_unknown(type_of_service)
    ) %>%
    dplyr::group_by(type_of_service) %>%
    dplyr::mutate(
      claim_amount = impute_median(claim_amount)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      type_of_service = factor(type_of_service)
    ) %>%
    dplyr::arrange(date_of_service)
}

#' Preprocess insurance data list
#'
#' @description
#' Takes a list containing customers, policies, and claims data frames and applies
#' standardized preprocessing to each component.
#'
#' @param data_list A list containing three data frames:
#'   \itemize{
#'     \item customers: Customer information data frame
#'     \item policies: Policy information data frame
#'     \item claims: Claims information data frame
#'   }
#'
#' @return A list containing preprocessed data frames:
#'   \itemize{
#'     \item customers: Cleaned customer data
#'     \item policies: Cleaned policy data
#'     \item claims: Cleaned claims data
#'   }
#'
#' @details
#' This function applies standardized preprocessing to each component of the insurance
#' data. It handles missing values, standardizes formats, and ensures data consistency
#' across all data frames.
#'
#' @examples
#' \dontrun{
#' raw_data <- import_data("customers.xlsx", "policies.xlsx", "claims.xlsx")
#' clean_data <- preprocess_insurance_data(raw_data)
#' }
#'
#' @export
preprocess_insurance_data <- function(data_list) {
  # JB 18/01/2025 11:47:10 verify input is a lsit
  if (!is.list(data_list)) {
    stop("Input must be a list", call. = FALSE)
  }
  # JB 18/01/2025 11:51:46 find difference between input and required df's , error if differene
  required_components <- c("customers", "policies", "claims")
  missing_components <- setdiff(required_components, names(data_list))

  if (length(missing_components) > 0) {
    stop(
      "Missing required data components: ",
      paste(missing_components, collapse = ", "),
      call. = FALSE
    )
  }
  # JB 18/01/2025 11:57:30 check df.s
  if (!all(sapply(data_list, is.data.frame))) {
    stop("All components must be data frames", call. = FALSE)
  }

  # JB 18/01/2025 12:08:19 process individual ataframes
  tryCatch({
    processed_data <- list(
      customers = pre_process_customers(data_list$customers),
      policies = pre_process_policies(data_list$policies),
      claims = pre_process_claims(data_list$claims)
    )
    # JB 18/01/2025 12:15:38 validate datframe
    validate_processed_data(processed_data)

    return(processed_data)
  }, error = function(e) {
    stop("Error in preprocessing: ", e$message, call. = FALSE)
  })
}

#' Validate processed insurance data
#' @param processed_data List of processed data frames
#' @return TRUE if validation passes, errors if not
#' @keywords internal
validate_processed_data <- function(processed_data) {
  # JB 18/01/2025 12:25:03 sanity ccheck all have rows > 0
  if (nrow(processed_data$customers) == 0) {
    stop("No valid customer records after preprocessing", call. = FALSE)
  }

  if (nrow(processed_data$policies) == 0) {
    stop("No valid policy records after preprocessing", call. = FALSE)
  }

  if (nrow(processed_data$claims) == 0) {
    stop("No valid claim records after preprocessing", call. = FALSE)
  }

  return(TRUE)
}

# Example usage:
#' @examples
#' \dontrun{
#' # Import raw data
#' raw_data <- import_data("customers.xlsx", "policies.xlsx", "claims.xlsx")
#'
#' # Preprocess all data
#' clean_data <- preprocess_insurance_data(raw_data)
#'
#' # Access individual components
#' clean_customers <- clean_data$customers
#' clean_policies <- clean_data$policies
#' clean_claims <- clean_data$claims
#' }
#'
#
