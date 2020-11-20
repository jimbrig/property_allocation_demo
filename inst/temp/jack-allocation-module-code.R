#' Allocation menu item UI
#'
#' @return HTML for the allocation menu item
#' @export
#'
#' @examples shiny h1 fluidRow column
alloc_ui <- function() {
  shiny::fluidRow(
    shiny::column(
      12,
      shiny::h1("Driver summary"),
      rhandsontable::rHandsontableOutput(
        outputId = "driver_summary_table"
      ) %>%
        shinycustomloader::withLoader()
    )
  )
}


#' Return a data frame containing the driver summary
#'
#' @param driver_summary Named list of various, but for this function, must
#' contain variable named "driver_summary": driver_summary$driver_summary
#' @param dictionary propalloc:::dictionary
#'
#' @return data.frame
#' @export
get_driver_summary_table <- function(driver_summary,
                                     dictionary) {
  result <- driver_summary$driver_summary %>%
    t() %>%
    as.data.frame() %>%
    dplyr::mutate(driver = rownames(.)) %>%
    transmute(driver,
              impact = V1) %>%
    apply_labels(dict = dictionary,
                 dataset_name = "driver_summary")

  rownames(result) <- NULL

  return(result)
}



#### Allocation ####

allocation_inputs <- shiny::reactiveValues()
demo_data <- load_demo_data(assign = FALSE)
for (allocation_input_index in 1:length(demo_data)) {
  allocation_input_name <- names(demo_data)[allocation_input_index]
  allocation_inputs[[allocation_input_name]] <- demo_data[[allocation_input_name]]
}
allocation_inputs$cap_threshold <- 0.25
allocation_inputs$budget_guidance_increase_factor <- 0.05
allocation_inputs$driver_summary_filter_vector <- c()


allocation_outputs <- shiny::reactiveValues()

observe({
  # extract costs from renewal cost table
  allocation_outputs$costs <- extract_costs(allocation_inputs$renewal_costs)

  # derive current and prior overall rates and percent change
  allocation_outputs$curr_rate <- allocation_outputs$costs$risk_transfer / sum(allocation_inputs$sov$tiv)
  allocation_outputs$prior_rate <- sum(allocation_inputs$priors$prior_risk_transfer_premium) /
    sum(allocation_inputs$priors$prior_tiv)
  allocation_outputs$pct_change <- (allocation_outputs$curr_rate / allocation_outputs$prior_rate) - 1

  # derive relativity adjusted TIVs
  allocation_outputs$relativity_adjusted_tivs <- apply_rels(allocation_inputs$bu_rels,
                                                            allocation_inputs$sprinkler_tier_rels,
                                                            allocation_inputs$combustible_rels,
                                                            allocation_inputs$sov)

  # summarize loss data (counts and incurred $'s) by entity and split out
  # count buckets based off count buckets table and experience period
  allocation_outputs$entity_loss_data <- entity_loss_summary(allocation_inputs$loss_run,
                                                             allocation_inputs$count_buckets)


  # perform allocation ------------------------------------------------------
  allocation_outputs$allocation_data <- merge_entity_data(
    allocation_inputs$sov,
    allocation_outputs$relativity_adjusted_tivs,
    allocation_outputs$entity_loss_data,
    allocation_inputs$rates,
    allocation_inputs$priors
  ) %>%
    # perform initial preliminary allocation (CAT first, back into AOP, terror)
    # this is uncapped. before surcharges, and excluding expenses
    preliminary_allocation(allocation_outputs$costs,
                           allocation_inputs$budget_guidance_increase_factor) %>%
    # apply surcharges
    apply_surcharges(allocation_inputs$count_buckets) %>%
    # adjust column names for apply threshold function
    # TODO: add arguments to apply threshold functions for specifying column
    # so don't have to add this step.
    mutate(
      prior_allocated = prior_risk_transfer_premium,
      prior_allocated_rate = prior_allocated / prior_tiv,
      uncapped_allocated = surcharged_premium
    ) %>%
    # apply capping using a default 25% threshold
    # TODO: add argument to apply threshold for whether or not to net the
    # total pct change or not - currently it does this
    apply_threshold(
      total_pct_chg = allocation_outputs$pct_change,
      threshold = allocation_inputs$cap_threshold
    ) %>%
    # final rebalancing and allocate expenses
    allocate_expenses(allocation_outputs$costs,
                      weight_variable = "tiv")


  driver_summary <- prepare_driver_summary(allocation_outputs$allocation_data,
                                           allocation_inputs$driver_summary_filter_vector)
})

output$driver_summary_table <- rhandsontable::renderRHandsontable({
  rhandsontable::rhandsontable(
    data = get_driver_summary_table(allocation_outputs$driver_summary,
                                    dictionary)
  )
})
