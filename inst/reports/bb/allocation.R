## ---- include=FALSE-----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE,
  warning = FALSE,
  error = TRUE,
  message = FALSE,
  echo = FALSE
)

## ----setup, eval=TRUE---------------------------------------------------------
# remove scientific notation
options(scipen = 999)

# library packages
library(propAlloc)
library(qs)
library(fs)
library(purrr)
library(dplyr)
library(knitr)
library(rmarkdown)
library(janitor)
library(kableExtra)
library(formattable)
library(stringr)
library(rlang)
library(tidyselect)
library(tidyr)
library(readr)
library(lubridate)

## ----load_data, eval=TRUE-----------------------------------------------------
load_demo_data()

## ----preview costs, eval=TRUE, results='asis'---------------------------------
kkable(
  data = initial_costs,
  caption = "Initial Costs Table",
  proper_cols = c("cost_type", "description"),
  currency_cols = c("actual_2019", "actual_2020", "projected_2021"),
  digits = 0,
  format_header = TRUE,
  add_totals = TRUE,
  format_totals = TRUE
 )

## ----extract_costs, eval=TRUE-------------------------------------------------
costs <- extract_costs_for_allocation(initial_costs)

tibble::as_tibble(costs) %>%
  as.matrix() %>%
  t() %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "Cost Type") %>%
  dplyr::rename("Dollar Amount" = V1) %>%
  kkable(currency_cols = "Dollar Amount",
         proper_cols = "Cost Type",
         caption = "Extracted Costs for Allocation:")

## ----preview_relativities, eval=TRUE------------------------------------------
initial_relativities %>%
  dplyr::mutate_at(dplyr::vars(rate_type, relativity_type), toproper, 
                   underscore_replacement = "-", return_as = "uppercase") %>%
  dplyr::mutate(
    relativity_type = stringr::str_to_title(relativity_type),
    relativity_type = stringr::str_replace_all(relativity_type, "Bu", "Business Unit"),
    level = stringr::str_replace_all(level, "bu_", "Business Unit ") %>% 
      stringr::str_replace_all(" a", " A") %>% stringr::str_replace_all(" b", " B")
  ) %>%
  kkable(
    col_names = c("Rate Type", "Relativity Type", "Level", "Relativity Factor"), 
    caption = "User-Defined Relativities",
    decimal_cols = c(ncol(initial_relativities)),
    decimal_digits = 3
  )

## ----apply_relativities, eval=TRUE, echo=FALSE--------------------------------
# derive relativity adjusted TIVs
relativity_adjusted_tivs <- derive_relativity_adjusted_tivs(
  bu_relativites,
  sprinkler_tier_relativities,
  combustible_relativities,
  sov
)

relativity_adjusted_tivs %>% 
  dplyr::mutate(entity_id = toproper(entity_id)) %>%
  head(10) %>% 
  kkable(
    caption = "Resulting Relativty Adjusted Total Insured Values by Entity and Coverage: ",
    col_names = c("Entity ID", "TIV", "AOP", "CAT-EQ", "CAT-Wind", "CAT-Flood", "Terrorism"),
    currency_cols = c(2:ncol(relativity_adjusted_tivs))
    ) %>%
  kableExtra::add_header_above(
    header = c(" " = 2, "Relativity Adjusted Total Insured Values" = 5), 
    bold = TRUE
  )

## ----count_surcharge_setup, eval=TRUE-----------------------------------------
min_year <- min(loss_run$year)
max_year <- max(loss_run$year)
experience_period <- c(min_year:max_year)
experience_period_display <- paste0(
   lubridate::ymd(paste0(min_year, "-01-01")) %>% format("%B %d, %Y"), " to ",
   lubridate::ymd(paste0(max_year, "-12-31")) %>% format("%B %d, %Y")
)
# min_value <- min(initial_count_buckets$min)
# max_value <- max(initial_count_buckets$max)

## ----preview_count_bucket_surcharge_input_table, eval=TRUE, results='asis'----
initial_count_buckets %>%
  select(name:dollar_surcharge) %>%
  kkable(
    col_names = c("Label", "Minimum", "Maximum", "Percent Surcharge", "Dollar Surcharge"),
     caption = "User-Defined Claim Count Bucket Surcharges",
    currency_cols = c("min", "max", "dollar_surcharge"),
    percent_cols = c("percent_surcharge")
  )

## ----derive_count_buckets_by_entity, eval=TRUE--------------------------------
entity_loss_data <- derive_entity_loss_data(loss_run, initial_count_buckets, experience_period)

col_names <- c("Entity ID", 
               initial_count_buckets$name,
               "Total Counts", "Total Incurred")

kkable(entity_loss_data %>%
         dplyr::mutate(entity_id = toproper(entity_id)) %>%
         dplyr::arrange(desc(total_incurred)) %>%
         head(10), 
       col_names = col_names,
       currency_cols = length(col_names),
       caption = "Summarized Loss Data by Entity (Top 10 Entities by Total Incurred)")

## ----rate_tables, eval=TRUE, results='asis'-----------------------------------
rate_tables <- initial_rates %>%
  dplyr::mutate(rate_type = dplyr::case_when(
    rate_type == "aop" ~ "AOP",
    rate_type == "cat_eq" ~ "CAT-EQ",
    rate_type == "cat_wind" ~ "CAT-Wind",
    rate_type == "cat_flood" ~ "CAT-Flood",
    rate_type == "terror" ~ "Terrorism"
  )) %>%
  split(initial_rates$rate_type) %>%
  purrr::set_names(c("All Other Peril (AOP)",
                     "Catastrophe - Earthquake (CAT-EQ)",
                     "Catastrophe - Wind (CAT-Wind)",
                     "Catastrophe - Flood (CAT-Flood)",
                     "Terrorism"))

purrr::map2(rate_tables, names(rate_tables), function(x, y) {
   kable(
     x,
     row.names = FALSE,
     digits = 3, 
     align = "crrrcc",
     col.names = snakecase::to_mixed_case(names(x), sep_out = " ") %>% 
       stringr::str_to_title() %>%
       stringr::str_replace_all("Tiv", "TIV") %>%
       stringr::str_replace_all("Id", "ID"),
     caption = paste0(y, " - Rates"),
     format.args = list(big.mark = ",")
   ) %>%
     kableExtra::kable_styling(position = "center",
                               bootstrap_options = c("striped", "bordered", "condensed", "responsive")) %>%
     kableExtra::row_spec(row = 0, bold = TRUE, align = "center")
 }) %>% set_names(NULL) %>% purrr::walk(print)

## ----derive_entity_data, eval=TRUE--------------------------------------------
entity_data <- derive_entity_data(sov, relativity_adjusted_tivs, entity_loss_data, initial_rates)
# names(entity_data)

## ----preliminary_premiums, eval=TRUE, results='hide'--------------------------
preliminary_allocation_data <- preliminary_allocation(entity_data)

# checks
sum(preliminary_allocation_data$rebalanced_model_terrorism_premium_adj) == costs$terrorism
sum(preliminary_allocation_data$total_model_premium_adj) == costs$risk_transfer
sum(preliminary_allocation_data$rebalanced_model_aop_premium_adj, preliminary_allocation_data$total_model_cat_premium_adj) == costs$all_risk

## ----apply_surcharges, eval=TRUE----------------------------------------------
surcharged_allocation_data <- apply_surcharges(preliminary_allocation_data, initial_count_buckets)

## ----add_priors, eval=TRUE----------------------------------------------------
allocation_data <- surcharged_allocation_data %>%
    dplyr::left_join(priors, by = "entity_id") %>%
    dplyr::mutate(new = dplyr::if_else(is.na(prior_tiv) | prior_tiv == 0, 1, 0))


## ----rate_capping, eval=TRUE, results='hide'----------------------------------
curr_rate <- costs$risk_transfer / sum(sov$tiv)
prior_rate <- sum(priors$prior_total_premium_excl_expense) / 
  sum(priors$prior_tiv)

pct_change <- (curr_rate / prior_rate) - 1

new_ents <- allocation_data %>%
  filter(new == 1)

allocation_data_nonnew <- allocation_data %>%
  filter(new == 0)

allocation_capped <- allocation_data_nonnew %>%
  mutate(prior_allocated = prior_total_premium_excl_expense,
         prior_allocated_rate = prior_allocated / prior_tiv,
         uncapped_allocated = surcharged_premium) %>%
  apply_threshold(total_pct_chg = pct_change,
                  threshold = .25) %>%
  bind_rows(new_ents) %>%
  mutate(
    allocated = coalesce(allocated, surcharged_premium),
    percent_tiv = tiv / sum(tiv),
    allocated_pct = allocated / sum(allocated, na.rm = TRUE),
    rebalanced_allocated = allocated_pct * costs$risk_transfer,
    # allocated_expenses = percent_tiv * curr_expenses,
    final_allocated_w_expense = rebalanced_allocated + allocated_expense
    )
  
sum(allocation_capped$final_allocated_w_expense) == costs$risk_transfer + costs$expenses
sum(allocation_capped$final_allocated_w_expense) == costs$total_incl_expense

## ----summary, eval=TRUE-------------------------------------------------------
# allocation_summary <- allocation_capped %>%
#   dplyr::transmute(
#     entity_id = entity_id,
#     bu = bu,
#     region = region,
#     country = country,
#     division = division,
#     location = location,
#     department = department,
#     tiv = tiv,
#     initial_aop_rate = model_aop_rate,
#     initial_cat_rate = total_model_cat_premium_adj / tiv,
#     initial_all_risk_rate = initial_aop_rate + initial_cat_rate,
#     initial_all_risk_premium = tiv * initial_all_risk_rate,
#     initial_terrorism_rate = model_terrorism_rate,
#     initial_terrorism_premium = initial_terrorism_rate * tiv,
#     initial_premium_excl_expenses = initial_all_risk_premium + initial_terrorism_premium,
#     market_premium_rate = market_aop_rate + market_cat_eq_rate + market_cat_flood_rate + market_cat_wind_rate + market_terrorism_rate,
#     market_premium_excl_expense = market_premium_rate * tiv,
#     prior_premium_excl_expense = prior_total_premium_excl_expense,
#     initial_pct_change_from_market_premium_excl_expense = ifelse(
#       is.na(market_premium_excl_expense) | market_premium_excl_expense == 0, 
#       NA, (initial_premium_excl_expenses / market_premium_excl_expense) - 1),
#     initial_pct_change_from_prior_premium_excl_expense = ifelse(
#       is.na(prior_total_premium_excl_expense) | 
#         prior_total_premium_excl_expense == 0, 
#       NA, (initial_premium_excl_expenses / prior_total_premium_excl_expense) - 1),
#     surcharged_premium,
#     capped_surcharged_premium = rebalanced_allocated,
#     final_premium_w_expense = final_allocated_w_expense,
#     final_market_premium_w_expense = market_premium_excl_expense + allocated_expense,
#     final_pct_change_from_market = (final_premium_w_expense / final_market_premium_w_expense) - 1,
#     final_dollar_change_from_market = final_premium_w_expense - final_market_premium_w_expense,
#     final_prior_premium_w_expense = prior_total_premium_incl_expense,
#     final_pct_change_from_prior = (final_premium_w_expense / final_prior_premium_w_expense) - 1,
#     final_dollar_change_from_market = final_premium_w_expense - final_prior_premium_w_expense,
#     final_pct_change_from_initial_premium = (final_premium_w_expense / initial_premium_excl_expenses) - 1,
#     final_dollar_change_from_initial_premium = final_premium_w_expense - initial_premium_excl_expenses,
#     model_aop_id = model_aop_id,
#     aop_sprinkler_tier = aop_sprinkler_tier,
#     aop_combustible = aop_combustible,
#     aop_relativity = aop_adj_tiv / tiv,
#     aop_adj_tiv = aop_adj_tiv,
#     model_cat_eq_id = model_cat_eq_id,
#     cat_eq_adj_tiv = cat_eq_adj_tiv,
#     cat_eq_relativity = cat_eq_adj_tiv / tiv,
#     cat_eq_adj_tiv = cat_eq_adj_tiv,
#     model_cat_wind_id = model_cat_wind_id,
#     cat_wind_adj_tiv = cat_wind_adj_tiv,
#     cat_wind_relativity = cat_eq_adj_tiv / tiv,
#     cat_wind_adj_tiv = cat_wind_adj_tiv,
#     model_cat_flood_id = model_cat_flood_id,
#     cat_flood_adj_tiv = cat_flood_adj_tiv,
#     cat_flood_relativity = cat_flood_adj_tiv / tiv,
#     cat_flood_adj_tiv = cat_flood_adj_tiv,
#     model_terrorism_id = model_terrorism_id,
#     terrorism_adj_tiv = terrorism_adj_tiv,
#     terrorism_relativity = terrorism_adj_tiv / tiv,
#     terrorism_adj_tiv = terrorism_adj_tiv
#   )

## ----output-------------------------------------------------------------------
#  # output_dir <- fs::path(here::here(), "outputs")
#  # fs::dir_create(output_dir)
#  # writexl::write_xlsx(allocation_summary, fs::path(output_dir, "Allocation-Summary-v2.xlsx"))

## ----output_results-----------------------------------------------------------
#  # outputs <- list(
#  #   "Allocation Working" = allocation_capped,
#  #   # premium_tables,
#  #   "AOP" = aop_premiums %>% select(-entities),
#  #   "CAT" = cat_premiums %>% select(-entities),
#  #   "Terror" = terror_premiums %>% select(-entities),
#  #   "Count Bucket Surcharges" = bucket_summary,
#  #   "Initial Costs" = initial_costs,
#  #   "Initial Entity Data" = entity_data,
#  #   "Initial Rates" = initial_rates,
#  #   "Initial Premiums" = initial_premiums,
#  #   "SOV" = sov,
#  #   "Priors" = priors
#  # )
#  #
#  # writexl::write_xlsx(outputs, fs::path(output_dir, "Results.xlsx"))

## ----get_labels, echo=FALSE, eval=TRUE----------------------------------------
labs = knitr::all_labels()
labs = setdiff(labs, c("setup", "get_labels"))

## ----all_code, ref.label=labs, eval=FALSE, echo=TRUE--------------------------
#  knitr::opts_chunk$set(
#    collapse = TRUE,
#    comment = "#>",
#    eval = FALSE,
#    warning = FALSE,
#    error = TRUE,
#    message = FALSE,
#    echo = FALSE
#  )
#  load_demo_data()
#  kkable(
#    data = initial_costs,
#    caption = "Initial Costs Table",
#    proper_cols = c("cost_type", "description"),
#    currency_cols = c("actual_2019", "actual_2020", "projected_2021"),
#    digits = 0,
#    format_header = TRUE,
#    add_totals = TRUE,
#    format_totals = TRUE
#   )
#  costs <- extract_costs_for_allocation(initial_costs)
#  
#  tibble::as_tibble(costs) %>%
#    as.matrix() %>%
#    t() %>%
#    as.data.frame() %>%
#    tibble::rownames_to_column(var = "Cost Type") %>%
#    dplyr::rename("Dollar Amount" = V1) %>%
#    kkable(currency_cols = "Dollar Amount",
#           proper_cols = "Cost Type",
#           caption = "Extracted Costs for Allocation:")
#  initial_relativities %>%
#    dplyr::mutate_at(dplyr::vars(rate_type, relativity_type), toproper,
#                     underscore_replacement = "-", return_as = "uppercase") %>%
#    dplyr::mutate(
#      relativity_type = stringr::str_to_title(relativity_type),
#      relativity_type = stringr::str_replace_all(relativity_type, "Bu", "Business Unit"),
#      level = stringr::str_replace_all(level, "bu_", "Business Unit ") %>%
#        stringr::str_replace_all(" a", " A") %>% stringr::str_replace_all(" b", " B")
#    ) %>%
#    kkable(
#      col_names = c("Rate Type", "Relativity Type", "Level", "Relativity Factor"),
#      caption = "User-Defined Relativities",
#      decimal_cols = c(ncol(initial_relativities)),
#      decimal_digits = 3
#    )
#  # derive relativity adjusted TIVs
#  relativity_adjusted_tivs <- derive_relativity_adjusted_tivs(
#    bu_relativites,
#    sprinkler_tier_relativities,
#    combustible_relativities,
#    sov
#  )
#  
#  relativity_adjusted_tivs %>%
#    dplyr::mutate(entity_id = toproper(entity_id)) %>%
#    head(10) %>%
#    kkable(
#      caption = "Resulting Relativty Adjusted Total Insured Values by Entity and Coverage: ",
#      col_names = c("Entity ID", "TIV", "AOP", "CAT-EQ", "CAT-Wind", "CAT-Flood", "Terrorism"),
#      currency_cols = c(2:ncol(relativity_adjusted_tivs))
#      ) %>%
#    kableExtra::add_header_above(
#      header = c(" " = 2, "Relativity Adjusted Total Insured Values" = 5),
#      bold = TRUE
#    )
#  min_year <- min(loss_run$year)
#  max_year <- max(loss_run$year)
#  experience_period <- c(min_year:max_year)
#  experience_period_display <- paste0(
#     lubridate::ymd(paste0(min_year, "-01-01")) %>% format("%B %d, %Y"), " to ",
#     lubridate::ymd(paste0(max_year, "-12-31")) %>% format("%B %d, %Y")
#  )
#  # min_value <- min(initial_count_buckets$min)
#  # max_value <- max(initial_count_buckets$max)
#  initial_count_buckets %>%
#    select(name:dollar_surcharge) %>%
#    kkable(
#      col_names = c("Label", "Minimum", "Maximum", "Percent Surcharge", "Dollar Surcharge"),
#       caption = "User-Defined Claim Count Bucket Surcharges",
#      currency_cols = c("min", "max", "dollar_surcharge"),
#      percent_cols = c("percent_surcharge")
#    )
#  entity_loss_data <- derive_entity_loss_data(loss_run, initial_count_buckets, experience_period)
#  
#  col_names <- c("Entity ID",
#                 initial_count_buckets$name,
#                 "Total Counts", "Total Incurred")
#  
#  kkable(entity_loss_data %>%
#           dplyr::mutate(entity_id = toproper(entity_id)) %>%
#           dplyr::arrange(desc(total_incurred)) %>%
#           head(10),
#         col_names = col_names,
#         currency_cols = length(col_names),
#         caption = "Summarized Loss Data by Entity (Top 10 Entities by Total Incurred)")
#  rate_tables <- initial_rates %>%
#    dplyr::mutate(rate_type = dplyr::case_when(
#      rate_type == "aop" ~ "AOP",
#      rate_type == "cat_eq" ~ "CAT-EQ",
#      rate_type == "cat_wind" ~ "CAT-Wind",
#      rate_type == "cat_flood" ~ "CAT-Flood",
#      rate_type == "terror" ~ "Terrorism"
#    )) %>%
#    split(initial_rates$rate_type) %>%
#    purrr::set_names(c("All Other Peril (AOP)",
#                       "Catastrophe - Earthquake (CAT-EQ)",
#                       "Catastrophe - Wind (CAT-Wind)",
#                       "Catastrophe - Flood (CAT-Flood)",
#                       "Terrorism"))
#  
#  purrr::map2(rate_tables, names(rate_tables), function(x, y) {
#     kable(
#       x,
#       row.names = FALSE,
#       digits = 3,
#       align = "crrrcc",
#       col.names = snakecase::to_mixed_case(names(x), sep_out = " ") %>%
#         stringr::str_to_title() %>%
#         stringr::str_replace_all("Tiv", "TIV") %>%
#         stringr::str_replace_all("Id", "ID"),
#       caption = paste0(y, " - Rates"),
#       format.args = list(big.mark = ",")
#     ) %>%
#       kableExtra::kable_styling(position = "center",
#                                 bootstrap_options = c("striped", "bordered", "condensed", "responsive")) %>%
#       kableExtra::row_spec(row = 0, bold = TRUE, align = "center")
#   }) %>% set_names(NULL) %>% purrr::walk(print)
#  entity_data <- derive_entity_data(sov, relativity_adjusted_tivs, entity_loss_data, initial_rates)
#  # names(entity_data)
#  preliminary_allocation_data <- preliminary_allocation(entity_data)
#  
#  # checks
#  sum(preliminary_allocation_data$rebalanced_model_terrorism_premium_adj) == costs$terrorism
#  sum(preliminary_allocation_data$total_model_premium_adj) == costs$risk_transfer
#  sum(preliminary_allocation_data$rebalanced_model_aop_premium_adj, preliminary_allocation_data$total_model_cat_premium_adj) == costs$all_risk
#  surcharged_allocation_data <- apply_surcharges(preliminary_allocation_data, initial_count_buckets)
#  allocation_data <- surcharged_allocation_data %>%
#      dplyr::left_join(priors, by = "entity_id") %>%
#      dplyr::mutate(new = dplyr::if_else(is.na(prior_tiv) | prior_tiv == 0, 1, 0))
#  
#  curr_rate <- costs$risk_transfer / sum(sov$tiv)
#  prior_rate <- sum(priors$prior_total_premium_excl_expense) /
#    sum(priors$prior_tiv)
#  
#  pct_change <- (curr_rate / prior_rate) - 1
#  
#  new_ents <- allocation_data %>%
#    filter(new == 1)
#  
#  allocation_data_nonnew <- allocation_data %>%
#    filter(new == 0)
#  
#  allocation_capped <- allocation_data_nonnew %>%
#    mutate(prior_allocated = prior_total_premium_excl_expense,
#           prior_allocated_rate = prior_allocated / prior_tiv,
#           uncapped_allocated = surcharged_premium) %>%
#    apply_threshold(total_pct_chg = pct_change,
#                    threshold = .25) %>%
#    bind_rows(new_ents) %>%
#    mutate(
#      allocated = coalesce(allocated, surcharged_premium),
#      percent_tiv = tiv / sum(tiv),
#      allocated_pct = allocated / sum(allocated, na.rm = TRUE),
#      rebalanced_allocated = allocated_pct * costs$risk_transfer,
#      # allocated_expenses = percent_tiv * curr_expenses,
#      final_allocated_w_expense = rebalanced_allocated + allocated_expense
#      )
#  
#  sum(allocation_capped$final_allocated_w_expense) == costs$risk_transfer + costs$expenses
#  sum(allocation_capped$final_allocated_w_expense) == costs$total_incl_expense
#  # allocation_summary <- allocation_capped %>%
#  #   dplyr::transmute(
#  #     entity_id = entity_id,
#  #     bu = bu,
#  #     region = region,
#  #     country = country,
#  #     division = division,
#  #     location = location,
#  #     department = department,
#  #     tiv = tiv,
#  #     initial_aop_rate = model_aop_rate,
#  #     initial_cat_rate = total_model_cat_premium_adj / tiv,
#  #     initial_all_risk_rate = initial_aop_rate + initial_cat_rate,
#  #     initial_all_risk_premium = tiv * initial_all_risk_rate,
#  #     initial_terrorism_rate = model_terrorism_rate,
#  #     initial_terrorism_premium = initial_terrorism_rate * tiv,
#  #     initial_premium_excl_expenses = initial_all_risk_premium + initial_terrorism_premium,
#  #     market_premium_rate = market_aop_rate + market_cat_eq_rate + market_cat_flood_rate + market_cat_wind_rate + market_terrorism_rate,
#  #     market_premium_excl_expense = market_premium_rate * tiv,
#  #     prior_premium_excl_expense = prior_total_premium_excl_expense,
#  #     initial_pct_change_from_market_premium_excl_expense = ifelse(
#  #       is.na(market_premium_excl_expense) | market_premium_excl_expense == 0,
#  #       NA, (initial_premium_excl_expenses / market_premium_excl_expense) - 1),
#  #     initial_pct_change_from_prior_premium_excl_expense = ifelse(
#  #       is.na(prior_total_premium_excl_expense) |
#  #         prior_total_premium_excl_expense == 0,
#  #       NA, (initial_premium_excl_expenses / prior_total_premium_excl_expense) - 1),
#  #     surcharged_premium,
#  #     capped_surcharged_premium = rebalanced_allocated,
#  #     final_premium_w_expense = final_allocated_w_expense,
#  #     final_market_premium_w_expense = market_premium_excl_expense + allocated_expense,
#  #     final_pct_change_from_market = (final_premium_w_expense / final_market_premium_w_expense) - 1,
#  #     final_dollar_change_from_market = final_premium_w_expense - final_market_premium_w_expense,
#  #     final_prior_premium_w_expense = prior_total_premium_incl_expense,
#  #     final_pct_change_from_prior = (final_premium_w_expense / final_prior_premium_w_expense) - 1,
#  #     final_dollar_change_from_market = final_premium_w_expense - final_prior_premium_w_expense,
#  #     final_pct_change_from_initial_premium = (final_premium_w_expense / initial_premium_excl_expenses) - 1,
#  #     final_dollar_change_from_initial_premium = final_premium_w_expense - initial_premium_excl_expenses,
#  #     model_aop_id = model_aop_id,
#  #     aop_sprinkler_tier = aop_sprinkler_tier,
#  #     aop_combustible = aop_combustible,
#  #     aop_relativity = aop_adj_tiv / tiv,
#  #     aop_adj_tiv = aop_adj_tiv,
#  #     model_cat_eq_id = model_cat_eq_id,
#  #     cat_eq_adj_tiv = cat_eq_adj_tiv,
#  #     cat_eq_relativity = cat_eq_adj_tiv / tiv,
#  #     cat_eq_adj_tiv = cat_eq_adj_tiv,
#  #     model_cat_wind_id = model_cat_wind_id,
#  #     cat_wind_adj_tiv = cat_wind_adj_tiv,
#  #     cat_wind_relativity = cat_eq_adj_tiv / tiv,
#  #     cat_wind_adj_tiv = cat_wind_adj_tiv,
#  #     model_cat_flood_id = model_cat_flood_id,
#  #     cat_flood_adj_tiv = cat_flood_adj_tiv,
#  #     cat_flood_relativity = cat_flood_adj_tiv / tiv,
#  #     cat_flood_adj_tiv = cat_flood_adj_tiv,
#  #     model_terrorism_id = model_terrorism_id,
#  #     terrorism_adj_tiv = terrorism_adj_tiv,
#  #     terrorism_relativity = terrorism_adj_tiv / tiv,
#  #     terrorism_adj_tiv = terrorism_adj_tiv
#  #   )
#  # output_dir <- fs::path(here::here(), "outputs")
#  # fs::dir_create(output_dir)
#  # writexl::write_xlsx(allocation_summary, fs::path(output_dir, "Allocation-Summary-v2.xlsx"))
#  # outputs <- list(
#  #   "Allocation Working" = allocation_capped,
#  #   # premium_tables,
#  #   "AOP" = aop_premiums %>% select(-entities),
#  #   "CAT" = cat_premiums %>% select(-entities),
#  #   "Terror" = terror_premiums %>% select(-entities),
#  #   "Count Bucket Surcharges" = bucket_summary,
#  #   "Initial Costs" = initial_costs,
#  #   "Initial Entity Data" = entity_data,
#  #   "Initial Rates" = initial_rates,
#  #   "Initial Premiums" = initial_premiums,
#  #   "SOV" = sov,
#  #   "Priors" = priors
#  # )
#  #
#  # writexl::write_xlsx(outputs, fs::path(output_dir, "Results.xlsx"))
#  sessionInfo()

## ----session_info, echo=TRUE, eval=TRUE---------------------------------------
sessionInfo()

