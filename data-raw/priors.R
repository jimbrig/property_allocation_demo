
require(data.table)
require(dplyr)
require(forcats)
require(tibble)

priors_file <- "data-raw/working/priors.csv"

priors <- data.table::fread(priors_file, stringsAsFactors = FALSE, data.table = FALSE) %>%
  tibble::as_tibble() %>%
  dplyr::arrange(location) %>%
  dplyr::mutate_at(2:12, round, 0) %>%
  dplyr::mutate_if(is.numeric, as.integer)

glimpse(priors)

