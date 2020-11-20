#' Apply Threshold
#'
#' @param dat data frame with required columns: uncapped_allocated and
#'   prior_allocated
#' @param total_pct_chg total percent change since prior on the overall
#'   rate (premium / TIV) excluding expenses.
#' @param threshold threshold to apply
#'
#' @return a [tibble][tibble::tibble-package]
#' @export
#' @importFrom dplyr mutate if_else
#' @importFrom stats optim
apply_threshold <- function(dat,
                            total_pct_chg,
                            threshold) {

  cap_fn <- function(offbal_factor, dat, total_pct_chg, threshold) {

    capped_data <- dat %>%
      dplyr::mutate(
        maximum_rate = prior_allocated_rate * (1 + total_pct_chg + threshold),
        minimum_rate = prior_allocated_rate * (1 + total_pct_chg - threshold),
        maximum_premium = maximum_rate * tiv,
        minimum_premium = minimum_rate * tiv,
        capped_allocated = pmax(
          pmin(
            uncapped_allocated * offbal_factor, maximum_premium
          ),
          minimum_premium),
        capped_pct = ((capped_allocated / tiv) / prior_allocated_rate) - 1
      )

    return(abs(sum(capped_data$uncapped_allocated, na.rm = TRUE) -
                 sum(capped_data$capped_allocated, na.rm = TRUE)))

  }

  offbal_factor <- stats::optim(1, cap_fn, dat = dat,
                                total_pct_chg = total_pct_chg,
                                threshold = threshold, method = "Brent",
                                lower = 1, upper = 3)$par

  dat %>%
    dplyr::mutate(
      maximum_rate = prior_allocated_rate * (1 + total_pct_chg + threshold),
      minimum_rate = prior_allocated_rate * (1 + total_pct_chg - threshold),
      maximum_premium = maximum_rate * tiv,
      minimum_premium = minimum_rate * tiv,
      allocated = pmax(pmin(uncapped_allocated * offbal_factor,
                            maximum_premium), minimum_premium),
      capped_dollar_change = allocated - prior_allocated,
      capped_percent_change = dplyr::if_else(prior_allocated == 0,
                                             0,
                                             (allocated / prior_allocated) - 1),
      capped_rate_percent_change = ((allocated / tiv) / prior_allocated_rate) - 1
    )

}
