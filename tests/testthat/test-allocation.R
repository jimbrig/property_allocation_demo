context("allocation")


test_that("final allocation results are reasonable", {

  # load data
  load_demo_data()

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
  relativity_adjusted_tivs <- apply_rels()

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

  expect_equal(sum(allocation_data$rebalanced_model_terrorism_premium_adj), costs$terrorism)

  expect_equal(sum(allocation_data$total_model_premium_adj), costs$risk_transfer)

  expect_equal(sum(allocation_data$rebalanced_model_aop_premium_adj, allocation_data$total_model_cat_premium_adj), costs$all_risk)

  expect_equal(sum(allocation_data$final_allocated_w_expense), costs$risk_transfer + costs$expenses)

  expect_equal(sum(allocation_data$final_allocated_w_expense), costs$total_incl_expense)

  # Result entity count is same as initial count
  expect_equal(allocation_data %>% nrow(), sov %>% nrow())

  # Changes in rates between prior and current/final allocated is within a reasonable bound
  change <- (allocation_data$allocated / allocation_data$tiv) / (allocation_data$prior_risk_transfer_premium / allocation_data$prior_tiv) - 1
  expect_true(all(abs(change) < 0.5))
})


test_that("load demo data succeeds", {
  load_demo_data()

  demo_data_object_names <- c(
    "sov", "priors",
    "renewal_costs", "count_buckets",
    "rates", "loss_run",
    "bu_rels", "sprinkler_tier_rels",
    "combustible_rels"
    )

  expect_true(all(sapply(demo_data_object_names, exists)))
  expect_true(all(sapply(demo_data_object_names, function(x) class(get(x))) == "data.frame"))
  expect_true(all(sapply(demo_data_object_names, function(x) nrow(get(x))) > 0))
})

test_that("sov is reasonable", {
  load_demo_data()

  # Duplicate entities (by bu:department)
  duplicate_entities <- sov %>%
    dplyr::select(bu:department, entity_id) %>%
    dplyr::group_by_at(setdiff(names(.), "entity_id")) %>%
    dplyr::mutate(count = n()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(count > 1)
  expect_equal(nrow(duplicate_entities), 0)

  # Duplicate entities (by entity_id)
  duplicate_entities <- sov %>%
    dplyr::group_by(entity_id) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(count > 1)
  expect_equal(nrow(duplicate_entities), 0)

  expect_true(all(sov$tiv > 0))
})

test_that("costs extraction is successful", {
  load_demo_data()
  costs <- extract_costs(renewal_costs)

  expect_identical(names(costs), c("terrorism", "all_risk", "risk_transfer", "expenses", "total_incl_expense"))
  expect_true(all(costs > 0))
  expect_equal(costs$terrorism + costs$all_risk, costs$risk_transfer)
  expect_equal(costs$risk_transfer + costs$expenses, costs$total_incl_expense)
  expect_equal(sum(renewal_costs$current), costs$total_incl_expense)
})


test_that("merging of entity data succeeds", {
  load_demo_data()
  entity_loss_data <- entity_loss_summary(loss_run, count_buckets)
  relativity_adjusted_tivs <- apply_rels()

  merged_entity_data <- merge_entity_data(
    sov,
    relativity_adjusted_tivs,
    entity_loss_data,
    rates,
    priors
  )

  expect_true(all(c("entity_id", "tiv", "prior_tiv") %in% names(merged_entity_data)))
})

test_that("preliminary allocation is reasonable", {
  load_demo_data()
  entity_loss_data <- entity_loss_summary(loss_run, count_buckets)
  relativity_adjusted_tivs <- apply_rels()
  costs <- extract_costs(renewal_costs)
  budget_guidance_percent <- 0.05
  the_preliminary_allocation <- merge_entity_data(
    sov,
    relativity_adjusted_tivs,
    entity_loss_data,
    rates,
    priors
  ) %>%
    preliminary_allocation(costs, budget_guidance_percent)

  expect_equal(sum(the_preliminary_allocation$total_model_cat_premium_adj + the_preliminary_allocation$rebalanced_model_aop_premium_adj), costs$all_risk)
  expect_equal(sum(the_preliminary_allocation$rebalanced_model_terrorism_premium_adj), costs$terrorism)
  expect_equal(sum(the_preliminary_allocation$total_model_premium_adj), costs$risk_transfer)
})

test_that("surcharges are being applied correctly", {
  load_demo_data()

  # Use made-up count percent surcharges
  count_buckets_to_use <- count_buckets
  count_buckets_to_use$percent_surcharge <- (1:nrow(count_buckets_to_use)) * 0.2

  entity_loss_data <- entity_loss_summary(loss_run, count_buckets_to_use)
  rels_list <- list(
    relativity_data = list(
      bu_rels[, c(1, 2)], bu_rels[, c(1, 3)], bu_rels[, c(1, 4)], bu_rels[, c(1, 5)],
      bu_rels[, c(1, 6)], sprinkler_tier_rels, combustible_rels
    ),
    coverage = list(
      "aop", "cat_eq", "cat_wind", "cat_flood", "terrorism", "aop", "aop"
    ),
    sov_linker = list(
      "bu", "bu", "bu", "bu", "bu", "aop_sprinkler_tier", "aop_combustible"
    )
  )

  relativity_adjusted_tivs <- ingest_relativities(rels_list, sov = sov)

  costs <- extract_costs(renewal_costs)
  budget_guidance_percent <- 0.05
  the_surge_data <- merge_entity_data(
    sov,
    relativity_adjusted_tivs,
    entity_loss_data,
    rates,
    priors
  ) %>%
    preliminary_allocation(costs, budget_guidance_percent) %>%
    apply_surcharges(count_buckets_to_use)

  expect_true(all(the_surge_data$counts >= 0))
  expect_true(all(the_surge_data$surcharge >= 0))
  expect_true(all(the_surge_data$surcharged_premium == the_surge_data$total_model_premium_adj + the_surge_data$surcharge))


  # Bucket count checks
  loss_count_raw_data <- if (exists("experience_period")) { loss_run %>% dplyr::filter(year %in% experience_period) %>% nrow() } else { loss_run %>% nrow() }
  loss_count_surge_data <- the_surge_data %>% dplyr::select(starts_with("bucket_")) %>% sum()
  expect_equal(loss_count_raw_data, loss_count_surge_data)
})

test_that("thresholds are being applied correctly", {
  load_demo_data()
  entity_loss_data <- entity_loss_summary(loss_run, count_buckets)
  rels_list <- list(
    relativity_data = list(
      bu_rels[, c(1, 2)], bu_rels[, c(1, 3)], bu_rels[, c(1, 4)], bu_rels[, c(1, 5)],
      bu_rels[, c(1, 6)], sprinkler_tier_rels, combustible_rels
    ),
    coverage = list(
      "aop", "cat_eq", "cat_wind", "cat_flood", "terrorism", "aop", "aop"
    ),
    sov_linker = list(
      "bu", "bu", "bu", "bu", "bu", "aop_sprinkler_tier", "aop_combustible"
    )
  )

  relativity_adjusted_tivs <- ingest_relativities(rels_list, sov = sov)
  costs <- extract_costs(renewal_costs)
  cap_threshold <- .25
  curr_rate <- costs$risk_transfer / sum(sov$tiv)
  prior_rate <- sum(priors$prior_risk_transfer_premium) /
    sum(priors$prior_tiv)
  pct_change <- (curr_rate / prior_rate) - 1
  budget_guidance_percent <- 0.05
  the_threshold_data <- merge_entity_data(
    sov,
    relativity_adjusted_tivs,
    entity_loss_data,
    rates,
    priors
  ) %>%
    preliminary_allocation(costs, budget_guidance_percent) %>%
    apply_surcharges(count_buckets) %>%
    mutate(
      prior_allocated = prior_risk_transfer_premium,
      prior_allocated_rate = prior_allocated / prior_tiv,
      uncapped_allocated = surcharged_premium
    ) %>%
    apply_threshold(
      total_pct_chg = pct_change,
      threshold = cap_threshold
    )

  # Capping keeps rates within bounds
  allowance <- 0.00001
  capping_bounds <- the_threshold_data %>%
    dplyr::mutate(maximum_rate = prior_allocated_rate * (1 + pct_change + cap_threshold),
                  minimum_rate = prior_allocated_rate * (1 + pct_change - cap_threshold),
                  capped_rate = allocated / tiv,
                  capped_rate_is_within_bounds = minimum_rate - allowance <= capped_rate & capped_rate <= maximum_rate + allowance)
  expect_true(all(capping_bounds$capped_rate_is_within_bounds))
})

test_that("driver summary is sensible", {
  load_demo_data()
  entity_loss_data <- entity_loss_summary(loss_run, count_buckets)
  rels_list <- list(
    relativity_data = list(
      bu_rels[, c(1, 2)], bu_rels[, c(1, 3)], bu_rels[, c(1, 4)], bu_rels[, c(1, 5)],
      bu_rels[, c(1, 6)], sprinkler_tier_rels, combustible_rels
    ),
    coverage = list(
      "aop", "cat_eq", "cat_wind", "cat_flood", "terrorism", "aop", "aop"
    ),
    sov_linker = list(
      "bu", "bu", "bu", "bu", "bu", "aop_sprinkler_tier", "aop_combustible"
    )
  )

  relativity_adjusted_tivs <- ingest_relativities(rels_list, sov = sov)
  costs <- extract_costs(renewal_costs)
  cap_threshold <- .25
  curr_rate <- costs$risk_transfer / sum(sov$tiv)
  prior_rate <- sum(priors$prior_risk_transfer_premium) /
    sum(priors$prior_tiv)
  pct_change <- (curr_rate / prior_rate) - 1
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
    mutate(
      prior_allocated = prior_risk_transfer_premium,
      prior_allocated_rate = prior_allocated / prior_tiv,
      uncapped_allocated = surcharged_premium
    ) %>%
    apply_threshold(
      total_pct_chg = pct_change,
      threshold = cap_threshold
    ) %>%
    allocate_expenses(costs, weight_variable = "tiv")
  driver_summary <- prepare_driver_summary(allocation_data,
                                           filter_vector = c())
                                           # filter_vector = c(bu = "bu_a")) # Example

  change_in_premium <- sum(allocation_data$final_allocated_w_expense) - sum(allocation_data$prior_premium_incl_expenses)

  expect_equal(round(change_in_premium, 0), round(sum(driver_summary$driver_summary), 0))
  expect_equal(round(driver_summary$final_premium_current - driver_summary$final_premium_prior, 0), round(sum(driver_summary$driver_summary), 0))
})


test_that("budget guidance allocation scenario succeeds", {
  load_demo_data()
  entity_loss_data <- entity_loss_summary(loss_run, count_buckets)
  rels_list <- list(
    relativity_data = list(
      bu_rels[, c(1, 2)], bu_rels[, c(1, 3)], bu_rels[, c(1, 4)], bu_rels[, c(1, 5)],
      bu_rels[, c(1, 6)], sprinkler_tier_rels, combustible_rels
    ),
    coverage = list(
      "aop", "cat_eq", "cat_wind", "cat_flood", "terrorism", "aop", "aop"
    ),
    sov_linker = list(
      "bu", "bu", "bu", "bu", "bu", "aop_sprinkler_tier", "aop_combustible"
    )
  )

  relativity_adjusted_tivs <- ingest_relativities(rels_list, sov = sov)
  costs <- extract_costs(renewal_costs)
  cap_threshold <- .25
  curr_rate <- costs$risk_transfer / sum(sov$tiv)
  prior_rate <- sum(priors$prior_risk_transfer_premium) /
    sum(priors$prior_tiv)
  pct_change <- (curr_rate / prior_rate) - 1
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
    preliminary_allocation(costs, budget_guidance_percent)

  premium_increase_attributable_to_bg <- allocation_data %>%
    mutate(bg_premium = bg_cat_eq_premium + bg_cat_wind_premium + bg_cat_flood_premium + bg_aop_premium + bg_terrorism_premium,
           prior_premium = prior_aop_premium + prior_cat_eq_premium + prior_cat_wind_premium + prior_cat_flood_premium + prior_terrorism_premium,

           premium_increase = bg_premium / prior_premium,
           tiv_increase = tiv / prior_tiv,
           premium_increase_attributable_to_bg = premium_increase / tiv_increase) %>%
    pull(premium_increase_attributable_to_bg)

  expect_equal(min(premium_increase_attributable_to_bg - 1), budget_guidance_percent)
  expect_equal(max(premium_increase_attributable_to_bg - 1), budget_guidance_percent)
})
