if (!require(pacman)) install.packages("pacman")
pacman::p_unload(all)

# No Remotes ----
# Attachments ----
pkgs <- c("docthis",
          "dplyr",
          "fs",
          "fst",
          "glue",
          "janitor",
          "openxlsx",
          "pacman",
          "purrr",
          "readxl",
          "rlang",
          "summarytools",
          "tibble",
          "tidyr",
          "tidyselect",
          "tools",
          "usethis",
          "cli",
          "DT",
          "formattable",
          "htmltools",
          "kableExtra",
          "knitr",
          "lubridate",
          "magrittr",
          "matchmaker",
          "rhandsontable",
          "rintrojs",
          "shiny",
          "shinycustomloader",
          "shinydashboard",
          "shinyjs",
          "shinyWidgets",
          "stats",
          "stringr",
          "utils",
          "attachment",
          "devtools",
          "remotes",
          "rmarkdown",
          "roxygen2",
          "testthat",
          "tidyverse",
          "goodpractice",
          "pkgbuild",
          "renv",
          "rstudioapi",
          "spelling",
          "covr",
          "desc",
          "pkgload",
          "gh")

pacman::p_load(
  pkgs, character.only = TRUE
)

names(pkgs) <- pkgs

to_install <- c("cli", "dplyr", "DT", "formattable", "fs", "fst", "htmltools", "janitor", "kableExtra", "knitr", "lubridate", "magrittr", "matchmaker", "purrr", "rhandsontable", "rintrojs", "rlang", "shiny", "shinycustomloader", "shinydashboard", "shinyjs", "shinyWidgets", "stringr", "tibble", "tidyr", "tidyselect")
  for (i in pkgs) {
    message(paste("looking for ", i))
    if (!requireNamespace(i)) {
      message(paste("     installing", i))
      install.packages(i)
    }
  }


pacman::p_unload(all)

library(propalloc)

# attachment::att_amend_desc()
# attachment::create_dependencies_file()


# rstudioapi::jobRunScript(
#   # rstudioapi::getActiveDocumentContext()[["path"]],
#   fs::path_package("propalloc", "scripts/dependencies.R"),
#   importEnv = TRUE,
#   exportEnv = "R_GlobalEnv"
# )


# deps <- renv::dependencies()
