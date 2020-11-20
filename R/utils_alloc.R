#' Arrange a data.frame by entity
#'
#' Since entity_id is a character field, this function parsed the number and
#' arranges the data in increasing order by the entity id's number.
#'
#' @param data data containing a column for "entity_id".
#'
#' @return an arranged tibble.
#' @export
arrange_by_entity <- function(data, entcol = "entity_id") {

  # stopifnot("entity_id" %in% names(data))

  data %>%
    dplyr::mutate(order = extract_number(.data[[entcol]])) %>%
    dplyr::arrange(order) %>%
    dplyr::select(-order)

}

#' Arrange a data.frame by location
#'
#' Since entity_id is a character field, this function parsed the number and
#' arranges the data in increasing order by the location's number.
#'
#' @param data data containing a column for "location".
#'
#' @return an arranged tibble.
#' @export
arrange_by_location <- function(data) {

  stopifnot("location" %in% names(data))

  data %>%
    dplyr::mutate(order = as.numeric(extract_number(location))) %>%
    dplyr::arrange(order) %>%
    dplyr::select(-order)

}

