#' Ingest Relativity Adjustment Factors
#'
#' This function consumes the user-defined relativity adjustment factors and
#' applies them to the TIV by entity resulting in a [tibble::tibble] with
#' columns for each coverages relativity adjusted TIV.
#'
#' @param rels_list A multi-layered, named list of lists. The first level of lists
#'   names are in the format
#'   \code{name = list(applies_to = "<coverage>", data = <data.frame>)} where the
#'   name refers to the name used to indentify the relativity (i.e. sprinkler tier,
#'   business unit, etc), the applies_to defines which coverage to applu the
#'   relativity adjustments to, and data is a 2-column data.frame where the first column
#'   specifies character labels and the second column specifies numeric factors.
#' @param sov SOV
#'
#' @return tibble
#' @export
#' @importFrom dplyr left_join select mutate
#' @importFrom purrr pmap reduce map
#' @importFrom tidyselect contains
ingest_relativities <- function(rels_list, sov = NULL) {

  stopifnot(is.data.frame(sov))

  hold <- purrr::pmap(rels_list, apply_relativity, sov = sov) %>%
    purrr::reduce(dplyr::left_join)

  final_factors <- c("aop", "cat_eq", "cat_wind", "cat_flood", "terrorism") %>%
    purrr::map(function(x) produce_factor(rel_data = hold, coverage = x)) %>%
    purrr::reduce(dplyr::left_join, by = "entity_id")

  sov %>%
    dplyr::select(entity_id, tiv) %>%
    dplyr::left_join(final_factors, by = "entity_id") %>%
    dplyr::mutate(
      aop_adj_tiv = .data$tiv * .data$aop_relativity_factor,
      cat_eq_adj_tiv = .data$tiv * .data$cat_eq_relativity_factor,
      cat_wind_adj_tiv = .data$tiv * .data$cat_wind_relativity_factor,
      cat_flood_adj_tiv = .data$tiv * .data$cat_flood_relativity_factor,
      terrorism_adj_tiv = .data$tiv * .data$terrorism_relativity_factor
    ) %>%
    dplyr::select(
      entity_id,
      tiv,
      tidyselect::contains("_adj_tiv") #aop_adj_tiv:terrorism_adj_tiv
    )

}

#


#' Produce Factor - Internal
#'
#' get all aop factors and derive total aop factor.
#'
#' @param rel_data df
#' @param coverage cov
#'
#' @return tibble
#'
#' @keywords internal
#' @importFrom dplyr select rowwise mutate
#' @importFrom rlang syms
#' @importFrom tidyselect contains
produce_factor <- function(rel_data,
                           coverage = c("aop",
                                        "cat_eq",
                                        "cat_wind",
                                        "cat_flood",
                                        "terrorism")) {

  cov_ <- paste0(coverage, "_")

  hold <- dplyr::select(rel_data, entity_id, tiv, tidyselect::contains(cov_)) %>%
    dplyr::select(entity_id, tiv, tidyselect::contains("_relativity"))

  mult_cols <- dplyr::select(hold, tidyselect::contains(cov_)) %>% names()

  out_col <- paste0(coverage, "_relativity_factor")

  hold %>%
    dplyr::rowwise() %>%
    dplyr::mutate({{out_col}} := prod(!!! rlang::syms(mult_cols))) %>%
    dplyr::select(c(1, ncol(.)))

}

#' Apply Relativity
#'
#' @param relativity_data df
#' @param coverage cov
#' @param sov_linker link
#' @param sov df
#'
#' @return df
#' @export
#' @importFrom dplyr select left_join
#' @importFrom rlang sym
apply_relativity <- function(
  relativity_data,
  coverage = c("aop", "cat_eq", "cat_wind", "cat_flood", "terrorism"),
  sov_linker = NA,
  sov = sov
) {

  # validate arguments
  cov <- match.arg(coverage)
  if (is.na(sov_linker)) sov_linker <- names(relativity_data)[1]
  stopifnot(is.data.frame(sov), ncol(relativity_data) == 2,
            is.character(sov_linker), length(sov_linker) == 1,
            sov_linker %in% colnames(sov), "tiv" %in% colnames(sov))

  # extract columns from sov
  tiv_data <- dplyr::select(sov, entity_id, tiv, !! rlang::sym(sov_linker))

  if (colnames(relativity_data)[1] != sov_linker) {
    names(relativity_data) <- c(sov_linker, names(relativity_data)[2])
  }

  # derive adj_tiv colname using coverage
  adj_tiv_col <- paste0(cov, "_", sov_linker, "_adj_tiv")

  # specify rel factor colname
  rel_col <- names(relativity_data)[2]

  # join with relativity data by sov_linker, calculate adj_tiv and output
  hold <- tiv_data %>%
    dplyr::left_join(relativity_data, by = sov_linker)

  hold[[adj_tiv_col]] <- hold$tiv * hold[[rel_col]]

  hold

}

