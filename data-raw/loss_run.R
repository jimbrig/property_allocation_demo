
require(data.table)
require(dplyr)
require(forcats)
require(tibble)
require(lubridate)

loss_run_file <- "data-raw/working/loss_run.csv"
loss_run <- data.table::fread(loss_run_file, stringsAsFactors = FALSE, data.table = FALSE) %>%
  tibble::as_tibble() %>%
  dplyr::arrange(location) %>%
  dplyr::mutate(
    date_of_loss = lubridate::mdy(date_of_loss)
  ) %>%
  dplyr::mutate_if(is.numeric, as.integer)

glimpse(loss_run)
