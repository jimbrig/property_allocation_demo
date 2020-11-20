#' Apply Relativity Adjustment Factors
#'
#' @param bu_rels BU rels
#' @param sprinkler_tier_rels Sprinkler Tier rels
#' @param combustible_rels Combustible rels
#' @param sov SOV
#'
#' @return tibble
#' @export
apply_rels <- function(bu_rels = NULL,
                       sprinkler_tier_rels = NULL,
                       combustible_rels = NULL,
                       sov = NULL) {

  if (is.null(bu_rels)) bu_rels <- load_demo_data("bu_rels", assign = FALSE)
  if (is.null(sprinkler_tier_rels)) sprinkler_tier_rels <- load_demo_data("sprinkler_tier_rels", assign = FALSE)
  if (is.null(combustible_rels)) combustible_rels <- load_demo_data("combustible_rels", assign = FALSE)
  if (is.null(sov)) sov <- load_demo_data("sov", assign = FALSE)

  sov %>%
    dplyr::select(
      bu:department,
      entity_id,
      aop_sprinkler_tier,
      aop_combustible,
      # aop_tiv_size_bucket,
      # cat_wind_hurricane,
      tiv
    ) %>%
    dplyr::left_join(bu_rels, by = "bu") %>%
    dplyr::left_join(sprinkler_tier_rels, by = "aop_sprinkler_tier") %>%
    dplyr::left_join(combustible_rels, by = "aop_combustible") %>%
    dplyr::mutate(
      aop_adj_tiv = .data$tiv * .data$aop_bu_relativity * .data$aop_sprinkler_tier_relativity * .data$aop_combustible_relativity,
      cat_eq_adj_tiv = .data$tiv * .data$cat_eq_bu_relativity,
      cat_wind_adj_tiv = .data$tiv * .data$cat_wind_bu_relativity,
      cat_flood_adj_tiv = .data$tiv * .data$cat_flood_bu_relativity,
      terrorism_adj_tiv = .data$tiv * .data$terrorism_bu_relativity
    ) %>%
    dplyr::select(
      entity_id,
      tiv,
      aop_adj_tiv:terrorism_adj_tiv
    )
}
