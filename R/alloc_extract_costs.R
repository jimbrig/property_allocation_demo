#' Extract Costs for Allocation
#'
#' This function takes as input the initial cost table (by default uses the internal
#' dataset \code{initial_costs}) and returns a named list containing:
#' \itemize{
#'   \item terrorism - Terrorism Cost
#'   \item all_risk - All Risk Cost (CAT + AOP)
#'   \item risk_transfer - Risk Transfer Cost (CAT + AOP + Terrorism)
#'   \item expenses - Total expenses
#'   \item total_w_expense - Total Including Expenses
#' }
#'
#' @param cost_table initial costs data.frame to extract from (default equals initial_costs)
#'
#' @return list of costs
#' @export
#'
#' @importFrom dplyr mutate filter pull
#' @importFrom rlang .data
extract_costs <- function(cost_table) {

  if (missing(cost_table)) cost_table <- load_demo_data("renewal_costs",
                                                        assign = FALSE)

  # extract terrorism
  terror_cost <- cost_table %>%
    dplyr::filter(.data$description %in% "terrorism") %>%
    dplyr::pull(ncol(cost_table))

  # extract risk transfer cost
  total_risk_transfer_cost <- cost_table %>%
    dplyr::filter(.data$cost_type %in% "premium") %>%
    dplyr::pull(ncol(cost_table)) %>%
    sum()

  # extract total expenses
  total_expenses <- cost_table %>%
    dplyr::filter(.data$cost_type %in% "expense") %>%
    dplyr::pull(ncol(cost_table)) %>%
    sum()

  # derive total all risk cost
  total_all_risk_cost <- total_risk_transfer_cost - terror_cost

  list(
    terrorism = terror_cost,
    all_risk = total_all_risk_cost,
    risk_transfer = total_risk_transfer_cost,
    expenses = total_expenses,
    total_incl_expense = total_expenses + total_risk_transfer_cost
  )

}
