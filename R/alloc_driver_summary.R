#' Prepare a driver summary using the final allocation
#'
#' @param allocated_data the data that has passed through all allocation stages
#' (\code{apply_threshold()} is the final stage). This data has had filters applied to
#' fields bu:department
#' @param filter_vector Named character vector. Entries represent the filter (from Rshiny) to be applied to
#' allocated_data. Values of the vector either NA (meaning no filter) or character. Names of vector
#' refer to bu:department
#'
#' @return data.frame
#' @export
#' @importFrom dplyr filter transmute summarise_all
prepare_driver_summary <- function(allocated_data,
                                   filter_vector) {

  # First, filter the allocated_data by the filter_vector

  allocated_data_filtered <- allocated_data

  filter_vector_to_use <- filter_vector[which(!is.na(filter_vector))]

  if (length(filter_vector_to_use) > 0) {
    for (filter_index in 1:length(filter_vector_to_use)) {
      filter_field <- names(filter_vector_to_use)[filter_index]
      filter_item <- filter_vector_to_use[filter_index]

      allocated_data_filtered <- allocated_data_filtered %>%
        dplyr::filter(.data[[filter_field]] == filter_item)
    }
  }

  # Now summarise the results by driver

  driver_summary <- vector("list", length = 9)
  names(driver_summary) <- c("final_premium_current",
                             "final_premium_prior",
                             "final_premium_change",
                             "final_premium_change_pc",
                             "implied_rate_current",
                             "implied_rate_prior",
                             "implied_rate_change",
                             "implied_rate_change_pc",
                             "driver_summary")

  driver_summary$final_premium_current <- sum(allocated_data_filtered$final_allocated_w_expense, na.rm = TRUE)
  driver_summary$final_premium_prior <- sum(allocated_data_filtered$prior_premium_incl_expenses, na.rm = TRUE)
  driver_summary$final_premium_change <- driver_summary$final_premium_current - driver_summary$final_premium_prior
  driver_summary$final_premium_change_pc <- driver_summary$final_premium_change / driver_summary$final_premium_prior

  driver_summary$implied_rate_current <- driver_summary$final_premium_current / sum(allocated_data_filtered$tiv, na.rm = TRUE)
  driver_summary$implied_rate_prior <- driver_summary$final_premium_prior / sum(allocated_data_filtered$prior_tiv, na.rm = TRUE)
  driver_summary$implied_rate_change <- driver_summary$implied_rate_current - driver_summary$implied_rate_prior
  driver_summary$implied_rate_change_pc <- driver_summary$implied_rate_change / driver_summary$implied_rate_prior

  # NOTE: backing into relativity impact for now (see below for attempts to get
  # it to reconcile directly). We will simply need to re-state the 'model rates'
  # as premiums / adjusted TIVs and should balance out hopefully.

  driver_summary$driver_summary_individual <- allocated_data_filtered %>%
    dplyr::transmute(
      tiv_impact = (tiv - prior_tiv) * driver_summary$implied_rate_prior,
      aop_rate_impact = (model_aop_rate - prior_aop_rate) * tiv,
      eq_rate_impact = (model_cat_eq_rate - prior_cat_eq_rate) * tiv,
      flood_rate_impact = (model_cat_flood_rate - prior_cat_flood_rate) * tiv,
      wind_rate_impact = (model_cat_wind_rate - prior_cat_wind_rate) * tiv,
      terrorism_rate_impact = (model_terrorism_rate - prior_terrorism_rate) * tiv,
      surcharge_impact = surcharge,
      capping_rebalancing_impact = rebalanced_allocated - total_model_premium_adj - surcharge,
      expense_impact = final_allocated_w_expense - rebalanced_allocated - prior_expenses,
      relativity_impact = final_allocated_w_expense - prior_premium_incl_expenses -
        (tiv_impact + aop_rate_impact + eq_rate_impact + flood_rate_impact + wind_rate_impact +
           terrorism_rate_impact + surcharge_impact + capping_rebalancing_impact + expense_impact))

  driver_summary$driver_summary <- driver_summary$driver_summary_individual %>%
    dplyr::summarise_all(sum, na.rm = TRUE)

  driver_summary$driver_summary_individual <- driver_summary$driver_summary_individual %>%
    dplyr::mutate(total = rowSums(., na.rm = TRUE))

  driver_summary$driver_summary_individual <- cbind(allocated_data %>% dplyr::select(entity_id),
                                                    driver_summary$driver_summary_individual)

  return(driver_summary)
}
