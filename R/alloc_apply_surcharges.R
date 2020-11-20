#' Apply Claim Count Surcharges
#'
#' @param preliminary_allocation_data prelim data
#' @param count_buckets count buckets table
#'
#' @return data.frame
#'
#' @export
#'
#' @importFrom dplyr select left_join mutate group_by summarise ungroup
#'   distinct filter arrange desc
#' @importFrom tidyr gather
#' @importFrom tidyselect contains
apply_surcharges <- function(preliminary_allocation_data, count_buckets) {

  surcharges <- preliminary_allocation_data %>%
    dplyr::select(entity_id, premium = total_model_premium_adj,
                  tidyselect::contains("bucket_")) %>%
    tidyr::gather(key = "bucket", value = "counts", tidyselect::contains("bucket_")) %>%
    dplyr::left_join(dplyr::select(count_buckets, bucket,
                                   percent_surcharge, dollar_surcharge),
                     by = "bucket") %>%
    dplyr::mutate(
      surcharge = percent_surcharge * counts * premium +
        dollar_surcharge * counts
    ) %>%
    dplyr::group_by(entity_id) %>%
    dplyr::summarise(surcharge = sum(surcharge),
                     counts = sum(counts)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct() %>%
    dplyr::filter(counts > 0) %>%
    dplyr::arrange(dplyr::desc(surcharge))

  preliminary_allocation_data %>%
    dplyr::left_join(surcharges, by = "entity_id") %>%
    dplyr::mutate(surcharge = ifelse(is.na(surcharge), 0, surcharge),
                  counts = ifelse(is.na(counts), 0, counts),
                  surcharged_premium = surcharge + total_model_premium_adj)

}
