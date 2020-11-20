#' Perform Preliminary Allocation
#'
#' @param entity_data entity data
#' @param costs renewal costs list
#' @param bg_pct Budget Guidance Percentage (eg. 0.05 representing 5%)
#'
#' @return data.frame
#' @export
#' @importFrom dplyr mutate
preliminary_allocation <- function(entity_data, costs, bg_pct) {

  entity_data %>%
    dplyr::mutate(
      # CAT premiums
      market_cat_eq_premium = market_cat_eq_rate * tiv,
      bg_cat_eq_premium = prior_cat_eq_rate * cat_eq_adj_tiv * (1 + bg_pct),
      model_cat_eq_premium_adj = model_cat_eq_rate * cat_eq_adj_tiv,
      market_cat_wind_premium = market_cat_wind_rate * tiv,
      bg_cat_wind_premium = prior_cat_wind_rate * cat_wind_adj_tiv * (1 + bg_pct),
      model_cat_wind_premium_adj = model_cat_wind_rate * cat_wind_adj_tiv,
      market_cat_flood_premium = market_cat_flood_rate * tiv,
      bg_cat_flood_premium = prior_cat_flood_rate * cat_flood_adj_tiv * (1 + bg_pct),
      model_cat_flood_premium_adj = model_cat_flood_rate * cat_flood_adj_tiv,

      total_market_cat_premium = market_cat_eq_premium +
        market_cat_wind_premium + market_cat_flood_premium,
      total_bg_cat_premium = bg_cat_eq_premium +
        bg_cat_wind_premium + bg_cat_flood_premium,
      total_model_cat_premium_adj = model_cat_eq_premium_adj +
        model_cat_wind_premium_adj + model_cat_flood_premium_adj,

      # AOP premiums
      aop_premium_to_allocate = (costs$all_risk -
                                   sum(.data$total_model_cat_premium_adj, na.rm = TRUE)),
      market_aop_premium_adj = market_aop_rate * tiv,
      bg_aop_premium = prior_aop_rate * aop_adj_tiv * (1 + bg_pct),
      prelim_model_aop_premium_adj = model_aop_rate * aop_adj_tiv,
      rebalanced_model_aop_premium_adj =
        (prelim_model_aop_premium_adj /
           sum(.data$prelim_model_aop_premium_adj, na.rm = TRUE)) *
        aop_premium_to_allocate,


      # Terrorism premiums
      market_terrorism_premium = market_terrorism_rate * tiv,
      bg_terrorism_premium = prior_terrorism_rate * terrorism_adj_tiv * (1 + bg_pct),
      prelim_model_terrorism_premium_adj = model_terrorism_rate *
        terrorism_adj_tiv,
      rebalanced_model_terrorism_premium_adj =
        (prelim_model_terrorism_premium_adj /
           sum(.data$prelim_model_terrorism_premium_adj, na.rm = TRUE)) *
        costs$terrorism,

      # Total premium
      total_model_premium_adj = rebalanced_model_aop_premium_adj +
        total_model_cat_premium_adj + rebalanced_model_terrorism_premium_adj,
      indicated_model_rate = total_model_premium_adj / tiv
    )

}
