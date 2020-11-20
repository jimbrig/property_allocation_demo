context("ingest_relativity")

# library(propalloc) # cannot use devtools::load_all in tests.

test_that("relativities ingested OK", {

  aop_bu_rels <- bu_rels[, c(1:2)]
  cat_eq_bu_rels <- bu_rels[, c(1, 3)]
  cat_wind_bu_rels <- bu_rels[,c(1,4)]
  cat_flood_bu_rels <- bu_rels[,c(1,5)]
  terrorism_bu_rels <- bu_rels[,c(1,6)]

  rels_list <- list(
    relativity_data = list(
      aop_bu_rels, cat_eq_bu_rels, cat_wind_bu_rels, cat_flood_bu_rels,
      terrorism_bu_rels, sprinkler_tier_rels, combustible_rels
    ),
    coverage = list(
      "aop", "cat_eq", "cat_wind", "cat_flood", "terrorism", "aop", "aop"
    ),
    sov_linker = list(
      "bu", "bu", "bu", "bu", "bu", "aop_sprinkler_tier", "aop_combustible"
    )
  )

  relativity_adjusted_tivs <- ingest_relativities(rels_list, sov = sov)

  test_aop_bu_rels <- apply_relativity(bu_rels[, c(1:2)], sov = sov, coverage = "aop", sov_linker = "bu")
  test_cat_eq_bu_rels <- apply_relativity(bu_rels[, c(1, 3)], sov = sov, coverage = "cat_eq", sov_linker = "bu")
  test_cat_wind_burels <- apply_relativity(bu_rels[, c(1, 4)], sov = sov, coverage = "cat_wind", sov_linker = "bu")
  test_cat_flood_bu_rels <- apply_relativity(bu_rels[, c(1, 5)], sov = sov, coverage = "cat_flood", sov_linker = "bu")
  test_terrorism_bu_rels <- apply_relativity(bu_rels[, c(1, 6)], sov = sov, coverage = "terrorism", sov_linker = "bu")
  test_aop_sprinkler_tier_rels <- apply_relativity(sprinkler_tier_rels, sov = sov, coverage = "aop", sov_linker = "aop_sprinkler_tier")
  test_aop_combustible_rels <- apply_relativity(combustible_rels, sov = sov, coverage = "aop", sov_linker = "aop_combustible")

  expect_equal(relativity_adjusted_tivs$aop_adj_tiv,
               test_aop_bu_rels$aop_bu_relativity *
                 test_aop_sprinkler_tier_rels$aop_sprinkler_tier_relativity *
                 test_aop_combustible_rels$aop_combustible_relativity *
                 relativity_adjusted_tivs$tiv)

  expect_equal(relativity_adjusted_tivs$cat_eq_adj_tiv,
               test_cat_eq_bu_rels$cat_eq_bu_relativity *
                 relativity_adjusted_tivs$tiv)

  expect_equal(relativity_adjusted_tivs$cat_flood_adj_tiv,
               test_cat_flood_bu_rels$cat_flood_bu_relativity *
                 relativity_adjusted_tivs$tiv)

  expect_equal(relativity_adjusted_tivs$cat_wind_adj_tiv,
               test_cat_wind_burels$cat_wind_bu_relativity *
                 relativity_adjusted_tivs$tiv)

  expect_equal(relativity_adjusted_tivs$terrorism_adj_tiv,
               test_terrorism_bu_rels$terrorism_bu_relativity *
                 relativity_adjusted_tivs$tiv)


})



test_that("made up relativity ingested OK", {

  breaks_to_use <- c(0, 100000, 500000, 999999999)

  sov_to_test <- sov %>%
    dplyr::mutate(aop_tiv_size_bucket = as.numeric(as.character(cut(tiv,
                                                       breaks = breaks_to_use,
                                                       labels = breaks_to_use[-1]))))

  rels_list <- list(
    relativity_data = list(
      data.frame(aop_tiv_size_bucket = breaks_to_use[-1],
                 aop_tiv_size_relativity = c(0.75, 1.0, 1.25))
    ),
    coverage = list(
      "aop"
    ),
    sov_linker = list(
      "aop_tiv_size_bucket"
    )
  )

  relativity_adjusted_tivs <- ingest_relativities(rels_list, sov = sov_to_test)

  # Check
  inferred_relativities <- relativity_adjusted_tivs %>%
    dplyr::select(tiv, aop_adj_tiv) %>%
    dplyr::mutate(bucket_name = factor(ifelse(tiv <= breaks_to_use[2], "low",
                                       ifelse(tiv <= breaks_to_use[3], "medium",
                                              "high")),
                                       levels = c("low", "medium", "high"))) %>%
    dplyr::group_by(bucket_name) %>%
    dplyr::summarise_all(sum) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(inferred_relativity = aop_adj_tiv / tiv)


  expect_equal(inferred_relativities$inferred_relativity, rels_list[[1]][[1]]$aop_tiv_size_relativity)

})
