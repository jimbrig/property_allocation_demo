
#  ------------------------------------------------------------------------
#
# Title : Allocation Walkthrough
#         Simplified code mimicking 'allocation' vignette
#    By : Jimmy Briggs & Jack Reardon
#  Date : 2020-04-15 (Initial)
#
#  ------------------------------------------------------------------------

# setup -------------------------------------------------------------------

# remove scientific notation
options(scipen = 999)

# library packages

# install package first or run devtools::load_all() and
# source 'inst/scripts/dependencies.R'
# library(propalloc)
devtools::load_all()
library(dplyr)

# run devtools::load_all(); source("inst/scripts/dependencies.R")

# load data
# load_demo_data()

# ingest data / user-defined inputs ---------------------------------------

# TODO: once get a final listing of user-defined inputs that are note in tables
# (i.e. experience period, cap threshold, etc) - wrap in a list names "inputs"
# to mimic shiny server code for testing purposes

# specify percent change capping threshold
cap_threshold <- .25

# extract costs from renewal cost table
costs <- extract_costs(renewal_costs)

# derive current and prior overall rates and percent change
curr_rate <- costs$risk_transfer / sum(sov$tiv)
prior_rate <- sum(priors$prior_risk_transfer_premium) /
  sum(priors$prior_tiv)
pct_change <- (curr_rate / prior_rate) - 1

# derive relativity adjusted TIVs
relativity_adjusted_tivs <- apply_rels(bu_rels, sprinkler_tier_rels, combustible_rels, sov)

# summarize loss data (counts and incurred $'s) by entity and split out
# count buckets based off count buckets table and experience period
entity_loss_data <- entity_loss_summary(loss_run, count_buckets)

# perform allocation ------------------------------------------------------

# merge entity data (sov, rel adjusted tivs, loss data, market and model rates,
# and priors)
budget_guidance_percent <- 0.05

allocation_data <- merge_entity_data(
  sov,
  relativity_adjusted_tivs,
  entity_loss_data,
  rates,
  priors
) %>%
  # perform initial preliminary allocation (CAT first, back into AOP, terror)
  # this is uncapped. before surcharges, and excluding expenses
  preliminary_allocation(costs, budget_guidance_percent) %>%
  # apply surcharges
  apply_surcharges(count_buckets) %>%
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
    total_pct_chg = pct_change,
    threshold = cap_threshold
  ) %>%
  # final rebalancing and allocate expenses
  allocate_expenses(costs, weight_variable = "tiv")

# split out allocation data between "allocation_data_tidy" and "comparison_data":




# validate results and output ---------------------------------------------


# declutter results and output
allocation_results <- allocation_data %>%
  mutate(current_allocated_rate = rebalanced_allocated / tiv) %>% # excl expense
  select(
    entity_id,
    tiv,
    prior_tiv,
    aop_adj_tiv:terrorism_adj_tiv,
    model_aop_rate,
    model_cat_eq_rate,
    model_cat_wind_rate,
    model_cat_flood_rate,
    model_terrorism_rate,
    prior_risk_transfer_premium,
    preliminary_model_premium = total_model_premium_adj,
    surcharge,
    surcharged_premium,
    prior_allocated,
    prior_allocated_rate,
    uncapped_allocated,
    capped_allocated = allocated,
    final_allocated = rebalanced_allocated,
    current_allocated_rate,
    rate_percent_change = capped_rate_percent_change,
    allocated_expenses,
    final_allocated_w_expense
  )

driver_summary <- prepare_driver_summary(allocation_data, filter_vector = NULL)



# outputs -----------------------------------------------------------------
inputs <- list(
  "Dictionary" = dictionary,
  "SOV" = sov,
  "Renewal Costs" = renewal_costs,
  "Priors" = priors,
  "Loss Run" = loss_run,
  "Count Buckets" = count_buckets,
  "Rates" = rates,
  "BU Relativity" = bu_rels,
  "Combustible Relativity" = combustible_rels,
  "Sprinkler Relativity" = sprinkler_tier_rels
)

outputs <- list(
  "Full Allocation Data" = allocation_data,
  "Simple Allocation Results" = allocation_results
)

# writexl::write_xlsx(inputs, "./outputs/2020-04-15-allocation-inputs.xlsx")
# writexl::write_xlsx(outputs, "./outputs/2020-04-15-allocation-results.xlsx")

