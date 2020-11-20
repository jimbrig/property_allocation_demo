#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
#' @importFrom tibble tibble
## usethis namespace: end
NULL

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' Tidy eval helpers
#'
#' @name tidyeval
#' @keywords internal
#' @importFrom rlang expr enquo enquos sym syms .data := as_name as_label
#' @aliases expr enquo enquos sym syms .data := as_name as_label
#' @export expr enquo enquos sym syms .data := as_name as_label
NULL

#' Open UserGuide of the package
#'
#' @param ext extension of the book to open: 'html', 'pdf'
#'
#' @importFrom utils browseURL
#'
#' @export
open_guide <- function(ext = "html") {
  if (ext == "html") {
    guide_path <- system.file('report/_book/index.html', package = 'propalloc')
  } else if (ext == "pdf") {
    guide_path <- system.file('report/_book/report.pdf', package = 'propalloc')
  } else {
    guide_path <- system.file(paste0("report/_book/report.", ext[1]), package = 'propalloc')
  }

  browseURL(paste0('file://', guide_path))
}

#' Open pkgdown site of the package
#'
#' @importFrom utils browseURL
#'
#' @export
open_docs <- function() {
  guide_path <- system.file('docs/index.html', package = 'propalloc')
  if (guide_path == "") {
    stop('There is no pkgdown site in ', 'docs/index.html')
  }

  browseURL(paste0('file://', guide_path))
}

