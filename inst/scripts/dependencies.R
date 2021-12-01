# No Remotes ----
# Attachments ----
to_install <- c("cli", "config", "dplyr", "DT", "fs", "highcharter", "htmltools", "htmlwidgets", "lubridate", "magrittr", "matchmaker", "purrr", "rhandsontable", "rintrojs", "rlang", "scales", "shiny", "shinycustomloader", "shinydashboard", "shinyjs", "shinyWidgets", "stringr", "tibble", "tidyr", "tidyselect", "writexl", "openmetrics")
  for (i in to_install) {
    message(paste("looking for ", i))
    if (!requireNamespace(i)) {
      message(paste("     installing", i))
      install.packages(i)
    }
  }
