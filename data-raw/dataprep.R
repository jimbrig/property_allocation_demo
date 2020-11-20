
#  ------------------------------------------------------------------------
#
# Title : Property Allocation Dataprep Script
#    By : Jimmy Briggs
#  Date : 2020-04-02
#
#  ------------------------------------------------------------------------

# ingest data from created working data excel workbook (which links to client
# excel model) and save for use in package:

# remove scientific notation
options(scipen = 999)

# packages ----------------------------------------------------------------
if (!require(pacman)) install.packages("pacman")
pacman::p_load(
  fs,
  dplyr,
  tidyr,
  openxlsx,
  readxl,
  purrr,
  summarytools,
  usethis,
  glue,
  rlang
)

pacman::p_load_current_gh("mdlincoln/docthis")

# specs -------------------------------------------------------------------
data_file <- fs::path("data-raw/working/working-data.xlsx")
sheets <- readxl::excel_sheets(data_file)

# scrubbed dataset --------------------------------------------------------
entity_data_scrubbed <- openxlsx::read.xlsx(
  data_file,
  "scrubbed"
) %>%
  janitor::clean_names() %>%
  mutate(entity_id = paste0("entity_", dplyr::row_number()))

# sov ---------------------------------------------------------------------
sov <- openxlsx::read.xlsx(
  data_file,
  "sov"
) %>%
  dplyr::select(
    entity_id,
    loss_run_id,
    tidyselect::everything()
  )

# market, model, and prior rates ------------------------------------------
rates <- openxlsx::read.xlsx(
  data_file,
  "rates"
) %>%
  dplyr::mutate(
    prior_rate = market_rate
  )

rates_split <- rates %>%
  mutate(rate_type = if_else(rate_type == "terror", "terrorism", rate_type)) %>%
  split(rates$rate_type) %>%
  map(function(x) {

    type <- x[1, "rate_type"]
    cols <- c(paste0(type, "_id"),
              paste0("prior_", type, "_rate"),
              paste0("market_", type, "_rate"),
              paste0("model_", type, "_rate"),
              paste0(type, "_market_id"))

    order <- c(paste0(type, "_id"),
               paste0(type, "_market_id"),
               paste0("prior_", type, "_rate"),
               paste0("market_", type, "_rate"),
               paste0("model_", type, "_rate"))

    market_col <- paste0(type, "_market_id")

    x %>%
      select(-rate_type) %>%
      mutate("{{market_col}}" := rate_id) %>%
      set_names(cols) %>%
      select(all_of(order))

  })

# priors ------------------------------------------------------------------

# NOTE: for priors instead of using actuals from excel model am going to utilize
# market rates x prior TIVs (replacing 0's with currents) to make priors more
# applicable to modelling against current derived premiums

initial_priors <- openxlsx::read.xlsx(
  data_file,
  "priors"
)

prior_expenses <- initial_priors %>%
  select(entity_id, prior_expenses)

priors <- entity_data_scrubbed %>%
  select(
    entity_id,
    tiv,
    tiv_prior,
    new
  ) %>%
  mutate(
    tiv_prior = if_else(new == 1, tiv, tiv_prior)
  ) %>%
  select(-tiv, -new) %>%
  left_join(
    select(
      sov, entity_id, aop_id:terrorism_id
    )
  ) %>%
  left_join(rates_split[["aop"]]) %>%
  left_join(rates_split[["cat_eq"]]) %>%
  left_join(rates_split[["cat_wind"]]) %>%
  left_join(rates_split[["cat_flood"]]) %>%
  left_join(rates_split[["terror"]]) %>%
  left_join(prior_expenses) %>%
  select(-(contains("_id") & !starts_with("entity_")),
         -contains("market"), -contains("model")) %>%
  mutate(
    prior_aop_premium = prior_aop_rate * tiv_prior,
    prior_cat_eq_premium = prior_cat_eq_rate * tiv_prior,
    prior_cat_wind_premium = prior_cat_wind_rate * tiv_prior,
    prior_cat_flood_premium = prior_cat_flood_rate * tiv_prior,
    prior_terrorism_premium = prior_terrorism_rate * tiv_prior,
    prior_total_cat_premium = prior_cat_eq_premium + prior_cat_flood_premium + prior_cat_wind_premium,
    prior_all_risk_premium = prior_aop_premium + prior_total_cat_premium,
    prior_risk_transfer_premium = prior_all_risk_premium + prior_terrorism_premium,
    prior_premium_incl_expenses = prior_risk_transfer_premium + prior_expenses
  ) %>%
  select(
    entity_id,
    prior_tiv = tiv_prior,
    prior_expenses:prior_premium_incl_expenses
  )

# renewal costs / prior allocated amounts ---------------------------------
initial_renewal_costs <- renewal_costs <- readxl::read_excel(
  data_file,
  "renewal_costs"
) %>%
  mutate(prior_pct = actual_2019 / sum(actual_2019))

prior_expense <- initial_renewal_costs %>%
  filter(cost_type == "expense") %>%
  mutate(pct = actual_2020 / sum(actual_2020),
         prior = pct * sum(priors$prior_expenses)) %>%
  filter(prior > 0) %>%
  pull(prior)

renewal_costs <- tibble::tibble(
  cost_type = c("premium", "premium", "expense", "expense", "expense"),
  description = c("all_risk", "terrorism", "taxes", "fees", "program"),
  prior = c(
    sum(priors$prior_all_risk_premium),
    sum(priors$prior_terrorism_premium),
    prior_expense
  ),
  current = c(
    prior * 1.03
  )
)

# relativities ------------------------------------------------------------
relativities <- openxlsx::read.xlsx(
  data_file,
  "rels"
)

# split relativities into separate tables by category

# extract BU relativities
bu_rels <- relativities %>%
  dplyr::filter(relativity_type == "bu") %>%
  tidyr::pivot_wider(names_from = "rate_type", values_from = "relativity") %>%
  dplyr::select(
    bu = level,
    aop_bu_relativity = aop,
    cat_eq_bu_relativity = cat_eq,
    cat_wind_bu_relativity = cat_wind,
    cat_flood_bu_relativity = cat_flood,
    terrorism_bu_relativity = terrorism
  )

# extract sprinkler tier relativities
sprinkler_tier_rels <- relativities %>%
  dplyr::filter(relativity_type == "sprinkler_tier") %>%
  dplyr::select(
    aop_sprinkler_tier = level,
    aop_sprinkler_tier_relativity = relativity
  )

# extract combustible relativities
combustible_rels <- relativities %>%
  dplyr::filter(relativity_type == "combustible") %>%
  dplyr::select(
    aop_combustible = level,
    aop_combustible_relativity = relativity
  ) %>%
  # switch Non-Combustible and Combustible per Adam
  mutate(
    aop_combustible = c("Non-Combustible", "Combustible")
  )

# loss run ----------------------------------------------------------------
loss_run <- openxlsx::read.xlsx(
  data_file,
  detectDates = TRUE,
  "loss_run"
)

# note: creating random entity id's for claims with no mapping to sov

# set seed for runif
set.seed(5)

loss_run <- loss_run %>%
  dplyr::rowwise() %>%
  mutate(
    entity_id = if_else(
      entity_id == "Missing",
      paste0("entity_", round(runif(1, min = 1, max = nrow(sov)), 0)),
      entity_id
    )
  ) %>%
  tibble::as_tibble()

# count buckets -----------------------------------------------------------
count_buckets <- readxl::read_excel(
  data_file,
  "count_buckets"
)

# data dictionary for labeling fields, values, and variables -------------
old_dictionary <- dictionary

dictionary <- old_dictionary %>%
  dplyr::mutate(variable_label = ifelse(dataset == "loss_run" & variable == "entity_id", "Location", variable_label),
                variable_label = ifelse(dataset == "loss_run" & variable == "accident_location", "Accident City", variable_label),
                variable_label = ifelse(dataset == "sov" & variable == "entity_id", "Location", variable_label),
                variable_label = ifelse(dataset == "sov" & variable == "location", "Unit", variable_label),
                value_label = ifelse(dataset == "sov", gsub("Entity", "Location", .data$value_label), value_label),
                value_label = ifelse(dataset == "sov" & variable == "location", gsub("Location", "Unit", .data$value_label), value_label))

dictionary_ <- dictionary %>%
  dplyr::filter(dataset == "allocation" | dataset == "details") %>%
  dplyr::mutate(
    variable = ifelse(variable_label == "Location" & variable == "location", "entity_id", variable),
    value = ifelse(variable_label == "Location" & variable == "entity_id", gsub("location_", "entity_", value), value)
  ) %>%
  dplyr::add_row(
    dataset = "allocation",
    variable = "capped_rate_percent_change_net",
    variable_label = "Net Capped Rate % Chg",
    value = NA,
    value_label = NA,
    value_order = NA
  ) %>%
  dplyr::add_row(
    dataset = "details",
    variable = "percent_change_bg_rate",
    variable_label = "% Change Budget Guidance (Rate)",
    value = NA,
    value_label = NA,
    value_order = NA
  ) %>%
  dplyr::add_row(
    dataset = "details",
    variable = "percent_change_market_rate",
    variable_label = "% Change Market (Rate)",
    value = NA,
    value_label = NA,
    value_order = NA
  ) %>%
  dplyr::add_row(
    dataset = "details",
    variable = "percent_change_rate",
    variable_label = "% Change (Rate)",
    value = NA,
    value_label = NA,
    value_order = NA
  ) %>%
  dplyr::mutate(value_order = as.numeric(value_order)) %>%
  dplyr::add_row(
    dataset = "details",
    variable = "entity_id",
    variable_label = "Location",
    value = "entity_739",
    value_label = "Location 739",
    value_order = 739
  ) %>%
  dplyr::add_row(
    dataset = "allocation",
    variable = "entity_id",
    variable_label = "Location",
    value = "entity_739",
    value_label = "Location 739",
    value_order = 739
  )

dictionary_old <- dictionary
dictionary <- dictionary_ %>% dplyr::bind_rows(dictionary %>% dplyr::filter(dataset != "allocation", dataset != "details") %>% dplyr::mutate(value_order = as.numeric(value_order)))

dictionary <- openxlsx::read.xlsx(
  data_file,
  "metadata"
) %>%
  dplyr::mutate(
    value = ifelse(dataset == "rates" & value == "terrorism", "terror", value)
  )

dictionary <- dictionary %>%
  dplyr::mutate(variable_label = stringr::str_replace(
    variable_label, "Percent Change Since Prior", "Percent Change Since Prior (Rate)"
  ))

ents <- dictionary %>% filter(variable == "entity_id") %>% mutate(variable_label = "Location")
dictionary <- filter(dictionary, variable != "entity_id") %>% bind_rows(ents)

dictionary <- propalloc::dictionary %>%
  dplyr::mutate(x = ifelse(dataset == "priors" & value == "entity_", "x", "")) %>%
  filter(x == "" | is.na(x)) %>%
  select(-x)

anti_join(old_dictionary, dictionary)

usethis::use_data(dictionary, overwrite = TRUE)

tools::resaveRdaFiles("data")

# save --------------------------------------------------------------------
usethis::use_data(sov,
                  loss_run,
                  priors,
                  rates,
                  renewal_costs,
                  sprinkler_tier_rels,
                  bu_rels,
                  combustible_rels,
                  count_buckets,
                  dictionary,
                  overwrite = TRUE, version = 3)

# usethis::use_data(dictionary, internal = TRUE, overwrite = TRUE, version = 3)
tools::resaveRdaFiles("R", version = 3)
tools::resaveRdaFiles("data", version = 3)
tools::checkRdaFiles("R")
tools::checkRdaFiles("data")
# vroom::vroom_write(dictionary, "data-raw/meta/dictionary.csv")

# fs::dir_create("inst/intdata", recurse = TRUE)
# vroom::vroom_write(dictionary, "inst/intdata/dictionary.csv")

# check RDA for optimal compression level
tools::checkRdaFiles("data")

# create documentation skeletopns via 'docthis::doc_this'
cat(docthis::doc_this("sov"),
    docthis::doc_this("loss_run"),
    docthis::doc_this("priors"),
    docthis::doc_this("rates"),
    docthis::doc_this("renewal_costs"),
    docthis::doc_this("sprinkler_tier_rels"),
    docthis::doc_this("bu_rels"),
    docthis::doc_this("combustible_rels"),
    docthis::doc_this("count_buckets"),
    file = "R/meta_data.R",
    sep = "\n\n\n\n\n")

# create data reports for documentation
meta_dir <- fs::path("data-raw/meta/dataviz")
fs::dir_create(meta_dir)

summarytools::view(summarytools::dfSummary(sov),
                   file = fs::path(meta_dir, "sov.html"))

summarytools::view(summarytools::dfSummary(priors),
                   file = fs::path(meta_dir, "priors.html"))

summarytools::view(summarytools::dfSummary(rates),
                   file = fs::path(meta_dir, "rates.html"))

summarytools::view(summarytools::dfSummary(renewal_costs),
                   file = fs::path(meta_dir, "renewal_costs.html"))

summarytools::view(summarytools::dfSummary(count_buckets),
                   file = fs::path(meta_dir, "count_buckets.html"))

summarytools::view(summarytools::dfSummary(loss_run),
                   file = fs::path(meta_dir, "loss_run.html"))

# add to inst/extdata
dir <- fs::path("inst/extdata")
fs::dir_create(dir, recurse = TRUE)
fs::dir_ls(dir, all = TRUE) %>% purrr::walk(fs::file_delete)

list(sov = sov,
     loss_run = loss_run,
     priors = priors,
     rates = rates,
     bu_rels = bu_rels,
     sprinkler_tier_rels = sprinkler_tier_rels,
     combustible_rels = combustible_rels,
     count_buckets = count_buckets,
     priors = priors,
     renewal_costs = renewal_costs,
     dictionary = dictionary) %>%
  purrr::walk2(., paste0("inst/extdata/", names(.), ".fst"), fst::write_fst) # %>%
# purrr::walk2(., paste0("data-raw/output/", names(.), ".csv"), readr::write_csv)

# copy html
fs::dir_create("inst/reports/dataviz", recurse = TRUE)
fs::dir_ls(fs::path(meta_dir)) %>%
  purrr::walk2(., paste0("inst/reports/dataviz/", basename(.)), fs::file_copy, overwrite = TRUE)

