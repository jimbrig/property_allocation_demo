#' Coalesce Join
#'
#' @param x x
#' @param y y
#' @param by by
#' @param suffix suffix
#' @param join join type
#' @param ... passed to dplyr join function
#'
#' @return a [tibble][tibble::tibble-package]
#' @export
#' @importFrom dplyr union coalesce bind_cols
#' @importFrom purrr map_dfc
coalesce_join <- function(x,
                          y,
                          by = NULL,
                          suffix = c(".x", ".y"),
                          join = dplyr::full_join,
                          ...) {

  joined <- join(y, x, by = by, suffix = suffix, ...)

  # names of desired output
  cols <- dplyr::union(names(x), names(y))

  to_coalesce <- names(joined)[!names(joined) %in% cols]

  suffix_used <- suffix[ifelse(endsWith(to_coalesce, suffix[1]), 1, 2)]

  # remove suffixes and deduplicate
  to_coalesce <- unique(
    substr(
      to_coalesce,
      1,
      nchar(to_coalesce) - nchar(suffix_used)
    )
  )

  coalesced <- purrr::map_dfc(
    to_coalesce, ~ dplyr::coalesce(joined[[paste0(.x, suffix[1])]],
                                   joined[[paste0(.x, suffix[2])]])
  )

  names(coalesced) <- to_coalesce

  dplyr::bind_cols(joined, coalesced)[cols]

}


#' Pull all unique values for a variable
#'
#' @param data data.frame
#' @param var variable name
#' @param sort logical (default = TRUE)
#' @param decreasing logical (default = FALSE)
#' @param names logical (default = TRUE)
#'
#' @return vector
#'
#' @importFrom rlang sym !!
#' @importFrom dplyr pull
#' @importFrom purrr set_names
#'
#' @export
pull_unique <- function(data, var, sort = TRUE,
                        decreasing = FALSE, names = TRUE) {

  hold <- data %>%
    dplyr::pull(!!rlang::sym(var)) %>%
    unique()

  if (sort) hold <- hold %>% sort(decreasing = decreasing)
  if (names) hold <- hold %>% purrr::set_names()

  return(hold)

}

#' Get column classes from a data.frame
#'
#' @param data data.frame input
#'
#' @return if each column only has one associated class, a named character vector
#'   is returned with the names equal to the column names and values equal to
#'   the classes. If some columns have more than a single class, a named list
#'   with column names as names and classes as values is returned.
#' @export
#'
#' @examples
#' data <- data.frame(a = c(1:3), b = letters[1:3], c = c(TRUE, FALSE, TRUE))
#' get_col_classes(data)
#'
#' @importFrom purrr map simplify
get_col_classes <- function(data) {
  hold <- data %>% purrr::map(class)
  if (all(purrr::map(hold, length) == 1)) hold <- purrr::simplify(hold)
  return(hold)
}

#' Load Demo Data for Use in Property Allocation Shiny App and Examples
#'
#'
#' @param ... passed to [utils::data()]
#'
#' @export
#'
#' @examples
#' library(propalloc)
#'
#' \dontrun{
#' # unpack to global environment
#' load_demo_data(overwrite = TRUE)
#' }
load_demo_data <- function(...) {

  # path <- fs::path(fs::path_package("propalloc"), "extdata")
  data("sov", "priors", "renewal_costs", "count_buckets", "rates", "loss_run",
       "bu_rels", "sprinkler_tier_rels", "combustible_rels", "dictionary",
       package = "propalloc", ...)

  return(invisible(0))

}


#' To Proper
#'
#' @param string string to manipulate on
#' @param replace_underscores Logical: if \code{TRUE} replaces all underscores
#'   with specified \code{underscore_replacement} argument's value.
#' @param underscore_replacement Character: if argument \code{replace_underscores}
#'  equals \code{TRUE}, will replace all "_"'s with specified string.
#' @param return_as How should the string be returned? Options are:
#'   \itemize{
#'   \item{"titlecase"}: Applies \code{stringr::str_to_title}.
#'   \item{"uppercase"}: Applies \code{toupper}.
#'   \item{"lowercase"}: Applied \code{tolower}.
#'   \item{"asis"}: No manipulation. Returns as is.
#'   }
#' @param uppers Character vector of any strings that should be displayed in
#'   upper-case (i.e. TPA, WC, AL, ABC, etc.)
#'
#' @seealso \code{\link[stringr]{str_replace}}.
#'
#' @return "Proper" string
#' @export
#'
#' @examples
#' s <- "variable_a is awesome"
#' toproper(s)
#'
#' @importFrom stringr str_replace_all str_to_title
toproper <- function(string,
                     replace_underscores = TRUE,
                     underscore_replacement = " ",
                     return_as = c("titlecase", "uppercase", "lowercase", "asis"),
                     uppers = c("Id", "Aop", "Cat", "Eq", "Tiv")) {

  return_as <- match.arg(return_as, several.ok = FALSE)

  if (replace_underscores) {
    string <- stringr::str_replace_all(string, pattern = "_",
                                       replacement = underscore_replacement)
  }

  if (return_as == "asis") return(string)

  hold <- switch(
    return_as,
    titlecase = stringr::str_to_title(string),
    uppercase = toupper(string),
    lowercase = tolower(string)
  )

  if (!is.null(uppers)) {
    if (length(uppers) > 1) uppers <- paste(uppers, collapse = "|")
    hold <- stringr::str_replace_all(hold,
                                     stringr::regex(uppers,
                                                    ignore_case = TRUE),
                                     toupper)
  }

  return(hold)

}

#' Extract numbers from a string
#'
#' @param string String to pull numbers from
#' @param return_as Character or Numeric
#'
#' @return String of numbers
#' @export
#' @importFrom stringr str_extract
extract_number <- function(string, return_as = c("character", "numeric")) {
  return_as <- match.arg(return_as)
  n <- stringr::str_extract(string, "\\-*\\d+\\.*\\d*")
  if (return_as == "numeric") return(as.numeric(n))
  n
}

#' Sort Numeric
#'
#' Combines extract_number with sort to sort by integer values within character
#' strings. Useful to sort in order Unit 1, Unit 2, Unit 3 opposed to
#' Unit 1, Unit 10, Unit 11 (with 1's leading the default sort method).
#'
#' If any strings do not contain a number they will be placed first by default.
#'
#' @param vec character vector to sort.
#'
#' @return a sorted character vector, sorted by its extracted integers
#' @export
#'
#' @examples
#' library(propalloc)
#'
#' # undesired sort method
#' pull_unique(sov, "department") %>% sort_numeric()
#'
#' # correct method
#' pull_unique(sov, "department") %>% sort_numeric()
sort_numeric <- function(vec) {

  tibble::tibble(
    orig = vec,
    num = extract_number(vec, "numeric")
  ) %>%
    dplyr::arrange(num) %>%
    dplyr::pull(orig)
}

#' Format a numeric as rounded dollars
#'
#' @param value numeric
#'
#' @return character
#' @export
#'
#' @examples
#' format_round_dollar(1234.56) # Returns "$1,235"
format_round_dollar <- function(value) {
  "$" %>%
    paste0(value %>%
             round() %>%
             prettyNum(big.mark = ","))
}

