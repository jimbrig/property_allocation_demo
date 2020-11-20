
require(data.table)
require(dplyr)
require(forcats)
require(tibble)

sov_file <- "data-raw/working/sov.csv"

sov <- data.table::fread(sov_file,
                         stringsAsFactors = TRUE,
                         na.strings = c("N/A"),
                         data.table = FALSE) %>%
  tibble::as_tibble() %>%
  dplyr::arrange(location) %>%
  dplyr::mutate(tiv = as.integer(round(tiv, 0)))

glimpse(sov)
