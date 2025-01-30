#' Import Insurance Data from Excel Files
#'
#' @description Reads customer, policy, and claims data from excel files and returns them as list of data frames
#'
#' @param customers_path Character string Path to Excel file containing customer data
#' @param policies_path Character string. Path to Excel file containing policy data
#' @param claims_path Character string. Path to Excel file containing claims data
#'
#' @return List containing three data frames:
#'   \describe{
#'     \item{customers}{Data frame of customer information}
#'     \item{policies}{Data frame of policy information}
#'     \item{claims}{Data frame of claims information}
#'   }
#'
#' @importFrom readxl read_xlsx
#' @export
#'
#' @examples
#' \dontrun{
#' data <- import_data(
#'   customers_path = "data/customers.xlsx",
#'   policies_path = "data/policies.xlsx",
#'   claims_path = "data/claims.xlsx"
#' )
#' }
import_data <- function(customers_path, policies_path, claims_path) {
  # JB 18/01/2025 08:21:00 check data exists
  if (!all(file.exists(customers_path, policies_path, claims_path))) {
    stop("One or more input files do not exist")
  }

  # JB 18/01/2025 08:24:35 import data + catch errors
  tryCatch({
    customers <-  readxl::read_xlsx(customers_path)
    policies <- readxl::read_xlsx(policies_path)
    claims <- readxl::read_xlsx(claims_path)
  }, error = function(e) {
    stop("Error reading Excel files: ", e$message)
  })

  # JB 18/01/2025 08:31:54 return list of dataframes
  list(
    customers = customers,
    policies = policies,
    claims = claims
  )
}

