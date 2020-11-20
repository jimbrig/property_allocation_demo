---
pagetitle: allocation
output: 
  rmarkdown::html_vignette:
    fig_caption: yes
    keep_html: yes
    keep_md: yes
vignette: >
  %\VignetteIndexEntry{allocation}
  %\VignetteEncoding{UTF-8}
  %\VignetteEngine{knitr::rmarkdown}
editor_options: 
  chunk_output_type: inline
---



<style type="text/css">
h1.title {
  font-size: 38px;
  color: Black;
  text-align: center;
  padding: 15px 5px 10px 5px
}
img {
  border: none
}
</style>

&nbsp;

<h1 class="title toc-ignore">
<div class="line-block">
Property Allocation Model
<br/>
Tychobra
</div>
</h1>


<p>
<center>
<img src="logo.png" width = "50%"/>
</center>
</p>
<center>
<p><strong>Author:</strong> Jimmy Briggs <a href="mailto:jimmy.briggs@tychobra.com" class="email">jimmy.briggs@tychobra.com</a></p>
<strong>Date:</strong> Last Compiled on March 24, 2020
</center>

&nbsp;

## Overview

The purpose of this report is to walk through the various allocation procedures 
utilized in the shiny application. 

In order to provide knowledgeable insights from this report, we have supplied 
reasonable inputs that mimic the *user-driven* aspects of the allocation 
procedures. 

### Setup 

+ Set Options and Configurations 
+ Library Packages 

See [Appendix B: Session Info] for all packages utilized in this document.



### Initial Data

The data utilized in the model is broken down into the following: 

1. Schedule of Values (SOV) 
1. Loss Run 
1. Priors 
1. Renewal Costs 
1. Count Bucket Definitions 
1. Market Rates 
1. Model Rates 
1. Relativity
1. User-Defined Experience Period 
1. User-Defined Percent Capping Threshold 

For more detailed information on each of these data inputs, please see the 
[Data Documentation Vignette](data_documentation.html).  



## Ingest Data

This step involves merging each user-defined input table into a single, unified 
*Entity Data* database. In no particular order the steps undertaken here are: 

+ Derive costs to be allocated from the renewal costs table 
+ Derive relativity adjusted TIVs from relativity table 
+ Merge data into a unified entity database before pushing into the model 
  
### Renewal Costs Overview  

Let's take a look at our initial renewal costs table. 

Note that in the actual shiny application, this will be a dynamic user-input 
table that the user can edit and make changes to. 

<table class="table table-striped table-bordered table-responsive" style="margin-left: auto; margin-right: auto;">
<caption>Initial Costs Table</caption>
 <thead>
  <tr>
   <th style="text-align:center;font-weight: bold;text-align: center;"> Cost Type </th>
   <th style="text-align:center;font-weight: bold;text-align: center;"> Description </th>
   <th style="text-align:right;font-weight: bold;text-align: center;"> Prior </th>
   <th style="text-align:right;font-weight: bold;text-align: center;"> Current </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> Premium </td>
   <td style="text-align:center;"> All Risk </td>
   <td style="text-align:right;"> $63,301,745 </td>
   <td style="text-align:right;"> $65,200,798 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Premium </td>
   <td style="text-align:center;"> Terrorism </td>
   <td style="text-align:right;"> $3,909,519 </td>
   <td style="text-align:right;"> $4,026,804 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Expense </td>
   <td style="text-align:center;"> Taxes </td>
   <td style="text-align:right;"> $602,831 </td>
   <td style="text-align:right;"> $620,916 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Expense </td>
   <td style="text-align:center;"> Fees </td>
   <td style="text-align:right;"> $4,645,560 </td>
   <td style="text-align:right;"> $4,784,927 </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Expense </td>
   <td style="text-align:center;"> Program </td>
   <td style="text-align:right;"> $1,435,010 </td>
   <td style="text-align:right;"> $1,478,060 </td>
  </tr>
  <tr>
   <td style="text-align:center;font-weight: bold;background-color: lightgrey !important;"> Total </td>
   <td style="text-align:center;font-weight: bold;background-color: lightgrey !important;">  </td>
   <td style="text-align:right;font-weight: bold;background-color: lightgrey !important;"> $73,894,665 </td>
   <td style="text-align:right;font-weight: bold;background-color: lightgrey !important;"> $76,111,505 </td>
  </tr>
</tbody>
</table>


Note that for the above initial costs table, only the *Premium* amounts
will be allocated using our model methods, while expenses are allocated based on 
the TIV distribution by entity. 

The Cost Group **All Risk** is composed of AOP and CAT Premiums. 

Using the initial costs table, we want to extract the following values for use
in the model:

- **Terrorism Premium** 
- **All Risk Cost** 
- **Risk Transfer Cost**  
- **Expenses** 

***

***Formulas:*** 

$$Risk\ Transfer\ Cost = All\ Risk\ Cost + Terrorism\ Premium$$\
$$All\ Risk\ Cost\ = \ AOP\ Premium + CAT\ Premium$$\
$$CAT\ Premium = \sum_i{CAT\ Premium_i}\\\ (where\ i = CAT\ EQ,\ CAT\ Wind,\ and\ CAT\ Flood)$$\
$$Expenses = \sum_i{Expenses_i}$$\

***

***Notes:***

- For Terrorism Premium, if not directly input by user, can be backed into by 
  subtracting the *All Risk Cost* from the *Risk Transfer Cost*.
  
- For expenses, we take the sum of all non-premium related costs. If there are 
  any expenses that should not be allocated in the model they should be excluded. 
  
***

### Derive Costs to be Allocated 

Now, we need to extract the ***costs to be allocated*** in the model, specifically: 

1. The Total Cost Including Expenses  
1. The Risk Transfer Cost  
1. The All Risk Cost  
1. The Sum of Expenses  

To do this, I utilize an internal, custom utility function \code{extract_costs_for_allocation()}.  

The resulting ***Costs for Allocation*** are:


```
#> Error in extract_costs_for_allocation(initial_costs): could not find function "extract_costs_for_allocation"
#> Error in tibble::as_tibble(costs): object 'costs' not found
```

### Derive Relativity Adjusted TIVs 

This step involves taking the user-defined relativity's and multiplying them by 
the initial *Total Insured Values* defined in the Schedule of Values (SOV). 

Specifically, we take the user-input *Relativity Table* and apply a custom utility 
function `derive_relativity_adjusted_tivs()` to derive our final relativity 
adjusted TIVs. 

First, lets look at the input relativities: 


```
#> Error in tbl_vars_dispatch(x): object 'initial_relativities' not found
```

Now using `derive_relativity_adjusted_tivs()`, we get our resulting adjusted
TIVs: 


```
#> Error in derive_relativity_adjusted_tivs(bu_relativites, sprinkler_tier_relativities, : could not find function "derive_relativity_adjusted_tivs"
#> Error in dplyr::mutate(., entity_id = toproper(entity_id)): object 'relativity_adjusted_tivs' not found
```

***NOTE: For display purposes I will only show the first 10 rows here:***

### Ingest Count Buckets and Loss Run



The purpose of this step is to ingest the user-defined count buckets, defined in
the count bucket surcharge table in order to set the **Entity Data** database up 
to properly flow into the allocation model before applying the percent and dollar 
surcharges to the allocated amounts. 

In the end, we will derive final surcharge amounts by entity based off of their
historical loss experience, but in order to apply these we first need our 
preliminary allocated premium amounts (in order to apply the percent surcharges). 
However, for now we need to shape the data so this process is streamlined later on. 

The user-driven inputs utilized are: 

+ The Loss Run 
+ The Count Bucket Surcharge Table's Defined Bucket Minimum and Maximum Values   
+ The User-Defined Experience Period 

From these we do the following to derive our desired results: 

1. Filter the loss run to only include dates of loss within the user-defined 
   experience period interval. 
   
1. Filter out any claims with incurred values below the minimum specified value 
   or above the maximum specified amount (for the first and last defined buckets,
   respectively).

2. Summarize the loss run into a table displaying the number of claims within  
   each user-defined claim count severity *bucket*.
   
3. Derive total **dollar** surcharge amounts by entity (cannot apply 
   the **percent surcharges yet**), by taking results from step 2 and 
   applying the dollar amount surcharges to each entity.  
   
For this example I assume an experience period of: **January 01, 2015 to December 31, 2019**. 

Also, I assume the following user-defined surcharge inputs: 


```
#> Error in select(., name:dollar_surcharge): object 'initial_count_buckets' not found
```

**NOTES:** 

- $0 claims are included in the first bucket, this can be changed by specifying a 
min value of \$1 instead of \$0 in the table. This will filter out $0 claims. 

- I have arranged the table in decreasing order of total incurred amount,
and am only displaying the top 10 rows by entity. 

Now, lets derive and preview the aforementioned table summarizing the 
results by entity to be ingested into the final **Entity Data** database.


```
#> Error in derive_entity_loss_data(loss_run, initial_count_buckets, experience_period): could not find function "derive_entity_loss_data"
#> Error in eval(expr, envir, enclos): object 'initial_count_buckets' not found
#> Error in kkable(entity_loss_data %>% dplyr::mutate(entity_id = toproper(entity_id)) %>% : object 'col_names' not found
```

### Ingest Rates 

This step takes the market rates, user-defined model rates, and prior rates and 
merges them with the entities via their respective *Rate ID's* defined in the 
SOV. 

Once we have mapped rates, relativity adjusted TIVs, and costs for allocation we 
are ready to begin the process of allocating premiums to individual entities. 

Let's preview out indicated prior rates, market rates, and the user-defined 
model rates for each coverage and subcoverage split: 


```
#> Error in dplyr::mutate(., rate_type = dplyr::case_when(rate_type == "aop" ~ : object 'initial_rates' not found
#> Error in purrr::map2(rate_tables, names(rate_tables), function(x, y) {: object 'rate_tables' not found
```

Now we can move onto the final ***data ingestion*** process which is to form our 
preliminary **Entity Data** database to push into the model. 

### Form Entity Database

This step merges all previous derived data into a single unified database for
use in the model. In particular it merges: 

1. The SOV 
1. Market Rates by Key 
1. Relativity Adjusted TIVs 
1. The Loss Data and Count Buckets 
1. The Selected Model Rates 
1. Prior Rates and Premiums  


```
#> Error in derive_entity_data(sov, relativity_adjusted_tivs, entity_loss_data, : could not find function "derive_entity_data"
```

## Perform Allocation

Now that we have ingested all of the relevant data we can begin allocating out 
premiums.

The steps are as follows:  

1. Derive (Relativity Adjusted) Preliminary Market Premiums for each coverage group.  

1. Derive (Relativity Adjusted) Preliminary Model Premiums for each coverage group.  

1. Sum together all ***CAT*** premiums to get a total CAT premium across EQ, Wind, and Flood.  

1. Subtract the Total CAT Premium from the ***All Risk Premium*** to derive our 
   total ***AOP*** amount to allocate.  
   
1. Re-balance the *preliminary* ***AOP*** derived premiums to force the total to 
   equal the necessary amount from the previous step.  
   
1. Sum together the Rebalanced AOP Premium, the total CAT premium, and the Terrorism
   Premium to get a total model premium.  
   
1. Add the percentage and dollar surcharges by claim count bucket to the total 
   preliminary premium. 
   
1. Allocate expenses using the derived total expenses amount from the costs table 
   via the percentage distribution of TIV (unadjusted) by entity. 
   
1. Apply capping on percentage ***rate*** changes since prior. 

1. Re-balance capped premiums to equal the desired cost inputs. 

1. Validate Results 

1. Summarize Results 

***

***Formulas:***

$$Market\ Premium_c = Market\ Rate_c\ * Adjusted\ TIV_c$$

$$Where\ Subscript_c\ stands\ for\ coverage.$$

***


### Derive Preliminary Premiums by Coverage Group 


```
#> Error in dplyr::mutate(., market_cat_eq_premium = market_cat_eq_rate * : object 'entity_data' not found
#> Error in eval(expr, envir, enclos): object 'preliminary_allocation_data' not found
#> Error in eval(expr, envir, enclos): object 'preliminary_allocation_data' not found
#> Error in eval(expr, envir, enclos): object 'preliminary_allocation_data' not found
```

### Apply Claim Count Surcharges 


```
#> Error in dplyr::select(., entity_id, premium = total_model_premium_adj, : object 'preliminary_allocation_data' not found
```


```
#> Error in dplyr::left_join(., priors, by = "entity_id"): object 'surcharged_allocation_data' not found
```



### Apply Rate Capping


```
#> Error in eval(expr, envir, enclos): object 'costs' not found
#> Error in eval(expr, envir, enclos): object 'curr_rate' not found
#> Error in filter(., new == 1): object 'allocation_data' not found
#> Error in filter(., new == 0): object 'allocation_data' not found
#> Error in mutate(., prior_allocated = prior_total_premium_excl_expense, : object 'allocation_data_nonnew' not found
#> Error in eval(expr, envir, enclos): object 'allocation_capped' not found
#> Error in eval(expr, envir, enclos): object 'allocation_capped' not found
```

### Summarize Results



## Output

And finally lets output to Excel for review:






***

#### Appendix A: Code




```r
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE,
  warning = FALSE,
  error = TRUE,
  message = FALSE,
  echo = FALSE
)
load_demo_data()
initial_costs <- renewal_costs

kkable(
  data = initial_costs,
  caption = "Initial Costs Table",
  proper_cols = c("cost_type", "description"),
  currency_cols = c("prior", "current"),
  digits = 0,
  format_header = TRUE,
  add_totals = TRUE,
  format_totals = TRUE
 )
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
min_year <- min(loss_run$year)
max_year <- max(loss_run$year)
experience_period <- c(min_year:max_year)
experience_period_display <- paste0(
   lubridate::ymd(paste0(min_year, "-01-01")) %>% format("%B %d, %Y"), " to ",
   lubridate::ymd(paste0(max_year, "-12-31")) %>% format("%B %d, %Y")
)
# min_value <- min(initial_count_buckets$min)
# max_value <- max(initial_count_buckets$max)
initial_count_buckets %>%
  select(name:dollar_surcharge) %>%
  kkable(
    col_names = c("Label", "Minimum", "Maximum", "Percent Surcharge", "Dollar Surcharge"),
     caption = "User-Defined Claim Count Bucket Surcharges",
    currency_cols = c("min", "max", "dollar_surcharge"),
    percent_cols = c("percent_surcharge")
  )
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
entity_data <- derive_entity_data(sov, relativity_adjusted_tivs, entity_loss_data, initial_rates)
# names(entity_data)
preliminary_allocation_data <- preliminary_allocation(entity_data)

# checks
sum(preliminary_allocation_data$rebalanced_model_terrorism_premium_adj) == costs$terrorism
sum(preliminary_allocation_data$total_model_premium_adj) == costs$risk_transfer
sum(preliminary_allocation_data$rebalanced_model_aop_premium_adj, preliminary_allocation_data$total_model_cat_premium_adj) == costs$all_risk
surcharged_allocation_data <- apply_surcharges(preliminary_allocation_data, initial_count_buckets)
allocation_data <- surcharged_allocation_data %>%
    dplyr::left_join(priors, by = "entity_id") %>%
    dplyr::mutate(new = dplyr::if_else(is.na(prior_tiv) | prior_tiv == 0, 1, 0))

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
# output_dir <- fs::path(here::here(), "outputs")
# fs::dir_create(output_dir)
# writexl::write_xlsx(allocation_summary, fs::path(output_dir, "Allocation-Summary-v2.xlsx"))
# outputs <- list(
#   "Allocation Working" = allocation_capped,
#   # premium_tables,
#   "AOP" = aop_premiums %>% select(-entities),
#   "CAT" = cat_premiums %>% select(-entities),
#   "Terror" = terror_premiums %>% select(-entities),
#   "Count Bucket Surcharges" = bucket_summary,
#   "Initial Costs" = initial_costs,
#   "Initial Entity Data" = entity_data,
#   "Initial Rates" = initial_rates,
#   "Initial Premiums" = initial_premiums,
#   "SOV" = sov,
#   "Priors" = priors
# )
# 
# writexl::write_xlsx(outputs, fs::path(output_dir, "Results.xlsx"))
sessionInfo()
```

***

#### Appendix B: Session Info


```r
sessionInfo()
#> R version 4.0.3 (2020-10-10)
#> Platform: x86_64-w64-mingw32/x64 (64-bit)
#> Running under: Windows 10 x64 (build 20262)
#> 
#> Matrix products: default
#> 
#> locale:
#> [1] LC_COLLATE=English_United States.1252 
#> [2] LC_CTYPE=English_United States.1252   
#> [3] LC_MONETARY=English_United States.1252
#> [4] LC_NUMERIC=C                          
#> [5] LC_TIME=English_United States.1252    
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#>  [1] lubridate_1.7.9.2   readr_1.4.0         tidyr_1.1.2        
#>  [4] tidyselect_1.1.0    rlang_0.4.8         stringr_1.4.0      
#>  [7] formattable_0.2.0.1 kableExtra_1.3.1    janitor_2.0.1      
#> [10] rmarkdown_2.5       knitr_1.30          dplyr_1.0.2        
#> [13] purrr_0.3.4         fs_1.5.0            qs_0.23.3          
#> [16] propalloc_1.1.0    
#> 
#> loaded via a namespace (and not attached):
#>  [1] xts_0.12.1              usethis_1.6.3           webshot_0.5.2          
#>  [4] httr_1.4.2              rprojroot_2.0.2         tools_4.0.3            
#>  [7] backports_1.2.0         R6_2.5.0                DT_0.16                
#> [10] colorspace_2.0-0        shinycustomloader_0.9.0 withr_2.3.0            
#> [13] curl_4.3                compiler_4.0.3          cli_2.1.0              
#> [16] rvest_0.3.6             xml2_1.3.2              shinyjs_2.0.0          
#> [19] desc_1.2.0              rhandsontable_0.3.7     stringfish_0.14.2      
#> [22] matchmaker_0.1.1        scales_1.1.1            digest_0.6.27          
#> [25] pkgconfig_2.0.3         htmltools_0.5.0         highcharter_0.8.2      
#> [28] attempt_0.3.1           highr_0.8               fastmap_1.0.1          
#> [31] htmlwidgets_1.5.2       TTR_0.24.2              rstudioapi_0.13        
#> [34] quantmod_0.4.17         shiny_1.5.0             generics_0.1.0         
#> [37] RApiSerialize_0.1.0     zoo_1.8-8               jsonlite_1.7.1         
#> [40] config_0.3              magrittr_2.0.1          rlist_0.4.6.1          
#> [43] Rcpp_1.0.5              munsell_0.5.0           fansi_0.4.1            
#> [46] lifecycle_0.2.0         stringi_1.5.3           yaml_2.2.1             
#> [49] rintrojs_0.2.2          snakecase_0.11.0        grid_4.0.3             
#> [52] promises_1.1.1          shinydashboard_0.7.1    forcats_0.5.0          
#> [55] crayon_1.3.4            lattice_0.20-41         hms_0.5.3              
#> [58] pillar_1.4.6            igraph_1.2.6            pkgload_1.1.0          
#> [61] glue_1.4.2              evaluate_0.14           golem_0.2.1            
#> [64] data.table_1.13.2       remotes_2.2.0           RcppParallel_5.0.2     
#> [67] vctrs_0.3.5             httpuv_1.5.4            testthat_3.0.0         
#> [70] assertthat_0.2.1        xfun_0.19               mime_0.9               
#> [73] xtable_1.8-4            broom_0.7.2             roxygen2_7.1.1         
#> [76] later_1.1.0.1           viridisLite_0.3.0       dockerfiler_0.1.3      
#> [79] tibble_3.0.4            writexl_1.3.1           shinyWidgets_0.5.4     
#> [82] ellipsis_0.3.1
```

