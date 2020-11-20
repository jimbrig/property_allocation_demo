#' Derive Entity Data
#'
#' @param sov sov
#' @param relativity_adjusted_tivs rel adj tivs
#' @param entity_loss_data inc, buckets, and cnts by ent
#' @param rates rate table
#' @param priors prior premium table
#'
#' @return entity_data database
#' @export
#' @importFrom dplyr mutate if_else select left_join mutate_if
#' @importFrom purrr map
#' @importFrom tidyselect contains
merge_entity_data <- function(sov,
                              relativity_adjusted_tivs,
                              entity_loss_data,
                              rates,
                              priors) {

  # adjust rates for join
  rates_list <- rates %>%
    dplyr::mutate(rate_type = dplyr::if_else(rate_type == "terror", "terrorism", rate_type)) %>%
    split(.$rate_type) %>%
    purrr::map(function(x) {

      rate_type <- pull_unique(x, "rate_type")

      id_col <- paste0(rate_type, "_id")
      prior_col <- paste0("prior_", rate_type, "_rate")
      market_col <- paste0("market_", rate_type, "_rate")
      model_col <- paste0("model_", rate_type, "_rate")

      x %>%
        dplyr::select("{id_col}" := rate_id,
                      "{prior_col}" := prior_rate,
                      "{market_col}" := market_rate,
                      "{model_col}" := model_rate)
    })

  x <- sov %>%
    dplyr::left_join(
      relativity_adjusted_tivs %>% dplyr::select(-tiv), by = "entity_id"
    ) %>%
    # join loss data
    dplyr::left_join(entity_loss_data, by = "entity_id") %>%
    # join rates
    dplyr::left_join(rates_list[["aop"]], by = "aop_id") %>%
    dplyr::left_join(rates_list[["cat_eq"]], by = "cat_eq_id") %>%
    dplyr::left_join(rates_list[["cat_wind"]], by = "cat_wind_id") %>%
    dplyr::left_join(rates_list[["cat_flood"]], by = "cat_flood_id") %>%
    dplyr::left_join(rates_list[["terrorism"]], by = "terrorism_id") %>%
    dplyr::left_join(priors, by = "entity_id") %>%
    dplyr::mutate_if(is.numeric, ~ifelse(is.na(.), 0, .)) #%>%
    # dplyr::select(
    #   entity_id,
    #   bu:department,
    #   # aop_coverage:terrorism_coverage,
    #   aop_sprinkler_tier:cat_wind_hurricane,
    #   tidyselect::contains("bucket_"),
    #   total_counts:total_incurred,
    #   tiv,
    #   prior_tiv,
    #   tidyselect::contains("adj_tiv"),
    #   aop_id:terrorism_id,
    #   prior_aop_rate:model_terrorism_rate,
    #   prior_expenses:prior_premium_incl_expenses
    # )


}
