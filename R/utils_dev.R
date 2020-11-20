#' @keywords internal
#' @noRd
detach_packages <- function() {

  all_attached <- paste("package:",
                        names(utils::sessionInfo()$otherPkgs),
                        sep = "")

  suppressWarnings(
    invisible(
      lapply(
        all_attached, detach, character.only = TRUE, unload = TRUE
      )
    )
  )

}

#' Get Package Dependencies from DESCRIPTION
#'
#' @param path current projects root path; defaults to your current working
#'   directory.
#' @param dput logical
#' @param field Depends, Imports, or Suggests
#'
#' @return character vector of package names
#'
#' @importFrom stringr str_replace_all str_trim
#' @importFrom stats setNames
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' get_deps()
#' }
get_deps <- function(path = "DESCRIPTION",
                     dput = FALSE,
                     field = c("Depends", "Imports", "Suggests")) {

  init <- read.dcf(path)

  hold <- init[, intersect(colnames(init), field)] %>%
    gsub(pattern = "\n", replacement = "") %>%
    strsplit(",") %>%
    unlist() %>%
    stats::setNames(NULL)

  out <- hold[!grepl("^R [(]", hold)] %>%
    stringr::str_replace_all("\\(.+\\)", "") %>%
    stringr::str_trim() %>%
    unique() %>%
    sort()

  if (!dput) return(out)

  dput(out)

}

