#' Derive Loss Data by Entity
#'
#' This function derives total incurred, total claim counts, and claim counts
#' by "count bucket" for use in the allocation model.
#'
#' @param loss_run loss run
#' @param count_buckets count bucket table
#' @param experience_period defined experience period
#' @param min_date minimum date
#' @param max_date maximum data
#'
#' @return tibble
#' @export
#' @importFrom dplyr filter group_by summarise n ungroup mutate mutate_if left_join
#' @importFrom lubridate year
#' @importFrom tidyr spread
entity_loss_summary <- function(loss_run, count_buckets,
                                experience_period = NA, min_date = NA,
                                max_date = NA) {

  # if nothing specified use all claims / years as experience period
  if (all(is.na(c(experience_period, min_date, max_date)))) {
    experience_period <- pull_unique(loss_run, "year")
  } else {
    # if no experience period try using min/max dates
    if (length(experience_period) == 1 && is.na(experience_period)) experience_period <- c(
      lubridate::year(min_date):lubridate::year(max_date)
    )
  }

  # filter loss run for years in experience period
  loss_run_filt <- loss_run %>%
    dplyr::filter(.data$year %in% experience_period)

  hold <- loss_run_filt %>%
    dplyr::group_by(entity_id) %>%
    dplyr::summarise(total_counts = dplyr::n() %>% as.numeric(),
                     total_incurred = sum(total_incurred)) %>%
    dplyr::ungroup()

  loss_run_filt %>%
    dplyr::mutate(
      count_bucket = cut(
        .data$total_incurred,
        breaks = c(0, count_buckets$max),
        labels = count_buckets$bucket,
        include.lowest = TRUE
      ) %>% as.character()
    ) %>%
    dplyr::group_by(entity_id, count_bucket) %>%
    dplyr::summarise(number_of_claims = dplyr::n()) %>%
    dplyr::ungroup() %>%
    tidyr::spread(key = "count_bucket", value = "number_of_claims") %>%
    dplyr::mutate_if(is.numeric, ~ifelse(is.na(.), 0, .)) %>%
    dplyr::left_join(hold, by = "entity_id") %>%
    arrange_by_entity()

}
