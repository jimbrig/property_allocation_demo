#' Allocate expenses
#'
#' @param dat data frame with required columns: tiv and
#'   allocated
#' @param costs renewal costs list
#' @param weight_variable String: one of \code{c("tiv", "premium")} indicating
#' which variable should be used to weight and allocate expenses
#'
#' @return a [tibble][tibble::tibble-package]
#' @export
#' @importFrom dplyr mutate
#' @importFrom rlang .data
allocate_expenses <- function(dat,
                              costs,
                              weight_variable = c("tiv", "premium")) {

  if (weight_variable == "tiv") {
    weight_variable_in_data <- "tiv"
  } else {
    weight_variable_in_data <- "allocated"
  }

  dat %>%
    dplyr::mutate(expenses_weight = .data[[weight_variable_in_data]] / sum(.data[[weight_variable_in_data]]),
                  allocated_pct = .data$allocated / sum(.data$allocated, na.rm = TRUE),
                  rebalanced_allocated = .data$allocated_pct * costs$risk_transfer,
                  allocated_expenses = .data$expenses_weight * costs$expenses,
                  final_allocated_w_expense = .data$rebalanced_allocated + .data$allocated_expenses)
}
