#' Prepare Insurance Analysis Datasets
#'
#' @description
#' Creates two datasets from insurance company data:
#' 1. An expanded dataset with all claims matched to customer and policy information
#' 2. An aggregated dataset with claim summaries per customer
#'
#' @param customers A data frame containing customer information with at least 'customer_id'
#' @param policies A data frame containing policy information with at least 'policy_id'
#' @param claims A data frame containing claim information with at least:
#'   - customer_id
#'   - policy_id
#'   - claim_amount
#'
#' @return A list containing two data frames:
#'   - expanded_data: Detailed claims data joined with customer and policy information
#'   - aggregated_data: Customer-level summary with total claims and policy information
#'
#' @examples
#' \dontrun{
#' results <- prepare_analysis_data(
#'   customers = customer_data,
#'   policies = policy_data,
#'   claims = claims_data
#' )
#'
#' # Access expanded data
#' expanded_claims <- results$expanded_data
#'
#' # Access aggregated data
#' customer_summaries <- results$aggregated_data
#' }
#'
#' @importFrom dplyr inner_join left_join group_by summarize n mutate coalesce
#' @importFrom tidyr drop_na
#'
#' @export
prepare_analysis_data <- function(customers, policies, claims) {
  # JB 18/01/2025 17:37:59 checck dataframes contain the key columns, else error
  if (!all(c("customer_id") %in% names(customers))) {
    stop("customers must contain 'customer_id' column")
  }
  if (!all(c("policy_id") %in% names(policies))) {
    stop("policies must contain 'policy_id' column")
  }
  if (!all(c("customer_id", "claim_amount") %in% names(claims))) {
    stop("claims must contain 'customer_id'and 'claim_amount' columns")
  }
  # JB 18/01/2025 18:22:07 join 3 by customers and policies, inner_join keeps cols with both x and y
  expanded_data <- claims %>%
    dplyr::inner_join(customers, by = "customer_id") %>%
    dplyr::inner_join(policies, by = "policy_id")

  aggregated_claims <- claims %>%
    dplyr::group_by(customer_id) %>%
    dplyr::summarize(
      total_claims_amount = sum(claim_amount, na.rm = TRUE), # JB 18/01/2025 18:25:11 calculate total claims amount and numner of claims
      number_of_claims = dplyr::n(),
      .groups = "drop"
    )

  aggregated_data <- customers %>%
    dplyr::left_join(aggregated_claims, by = "customer_id") %>%
    dplyr::left_join(policies, by = "policy_id") %>% # JB 18/01/2025 18:29:53 innner join as before
    dplyr::mutate(
      total_claims_amount = dplyr::coalesce(total_claims_amount, 0),
      number_of_claims = dplyr::coalesce(number_of_claims, 0) # JB 18/01/2025 18:36:01 if NA for total and No, of claims -> 0
    ) %>%
    tidyr::drop_na()
  # JB 18/01/2025 18:39:51 list output
  list(
    expanded_data = expanded_data,
    aggregated_data = aggregated_data
  )
}
