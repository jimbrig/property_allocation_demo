# No Remotes ----
# Attachments ----
to_install <-
  c("cli",
    "dplyr",
    "DT",
    "formattable",
    "fs",
    "highcharter",
    "htmltools",
    "htmlwidgets",
    "janitor",
    "kableExtra",
    "knitr",
    "lubridate",
    "magrittr",
    "matchmaker",
    "purrr",
    "rhandsontable",
    "rintrojs",
    "rlang",
    "scales",
    "shiny",
    "shinycustomloader",
    "shinydashboard",
    "shinyjs",
    "shinyWidgets",
    "stringr",
    "summarytools",
    "tibble",
    "tidyr",
    "tidyselect")

for (i in to_install) {
  message(paste("looking for ", i))
  if (!requireNamespace(i)) {
    message(paste("     installing", i))
    require(i)
  }
}
