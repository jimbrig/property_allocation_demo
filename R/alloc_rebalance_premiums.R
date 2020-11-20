#' Rebalance premiums to a total value
#'
#' Preserves the portion of the total held by the column that is being rebalanced.
#'
#'
#' @param the_data data.frame
#' @param column_name_to_rebalance character reflecting the name of the (numeric) column that is being rebalanced
#' @param total_to_rebalance_to numeric
#' @param do_keep_column_name boolean If FALSE, then automatically renames to "column_name_rebalanced"
#'
#' @return data.frame
#' @export
#'
#' @importFrom dplyr mutate
#'
#' @examples
#' data.frame(premiums = 1:3) %>% rebalance_premiums("premiums", 12)
#' # Returns data.frame(premiums = c(2, 4, 6))
rebalance_premiums <- function(the_data,
                               column_name_to_rebalance,
                               total_to_rebalance_to,
                               do_keep_column_name = TRUE) {
  column_name_rebalanced <- paste0(column_name_to_rebalance,
                                   ifelse(do_keep_column_name, "", "_rebalanced"))
  the_data %>%
    dplyr::mutate(!!column_name_rebalanced := .data[[column_name_to_rebalance]] /
                    sum(.data[[column_name_to_rebalance]], na.rm = TRUE) *
                    total_to_rebalance_to)
}
