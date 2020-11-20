#' Premium and expense costs in prior and current financial years to allocate
#'
#' Sets out the high-level premium and expense costs in
#' both prior and current years. The sum of premiums and expenses in prior
#' and current columns will be allocated to individual entities
#' i.e. the premiums allocated by the model will sum to equal the
#' current column of this table.
#'
#' The table contains at least one entry with a description matching "all_risk"
#' and another matching "terrorism" - both of which should related to a
#' "premium" cost type.
#'
#' @format A data frame with 5 rows and 4 columns:
#' \describe{
#'   \item{cost_type}{String: Either 'premium' or 'expense' indicating the type of cost}
#'   \item{description}{String: Description the premium or expense}
#'   \item{prior}{Numeric: The prior year's premium or expense}
#'   \item{current}{Numeric: The current year's premium or expense}
#' }
"renewal_costs"

#' Current entity data detailing individual TIVs
#'
#' Sets out the high-level premium and expense costs in
#' both prior and current years. The sum of premiums and expenses in prior
#' and current columns will be allocated to individual entities
#' ie. the premiums allocated by the model will sum to equal the
#' current column of this table.
#'
#' The table contains at least one entry with a description matching "all_risk"
#' and another matching "terrorism" - both of which should related to a
#' "premium" cost type.
#'
#' @format A data frame with 739 rows and 24 columns:
#' \describe{
#'   \item{entity_id}{String: Unique identifier of the entity}
#'   \item{loss_run_id}{String: Identifier to match losses to entities.}
#'   \item{bu, region, country, state, division, location, department}{String: Attributes describing the entity. bu = Business unit.}
#'   \item{tiv}{Numeric: The Total Insured Value assessed per entity}
#'   \item{aop_coverage, cat_eq_coverage, cat_wind_coverage, cat_flood_coverage, terrorism_coverage}{Boolean: Indicating whether entity takes such coverage}
#'   \item{aop_sprinkler_tier}{String: Referencing exactly one of the tiers present in aop_sprinkler_tier, reflecting the tier of sprinkler system installed by the entity.}
#'   \item{aop_combustible}{String: Referencing exactly one of the tiers present in combustible_rels, reflecting the level of combustibility of the entity.}
#'   \item{aop_tiv_size_bucket}{String: Derivative field reflecting the relative size of TIV. Not used in default setup of the model, although can be conveniently used by custom rates (see \code{rates})}
#'   \item{cat_wind_hurricane}{String: Referencing...}
#'   \item{aop_id}{String: A geographical ID used by \code{rates}}
#'   \item{cat_eq_id, cat_wind_id, cat_flood_id}{String: An ID representing the threat of earthquake-, wind-, or flood-related catastrophic damage used by \code{rates}}
#'   \item{terrorism_id}{String: A geographical ID used by \code{rates}}
#' }
"sov"

#' Prior TIVs, premiums and expenses assessed for each entity
#'
#' @format A data frame with 739 rows and 12 columns:
#' \describe{
#'   \item{entity_id}{String: Unique identifier of the entity; Set may not match those of \code{sov}}
#'   \item{prior_tiv}{Numeric: The Total Insured Value assessed per entity in the prior financial period}
#'   \item{prior_expenses}{Numeric: Expenses attributable to the entity in the prior financial period}
#'   \item{prior_aop_premium, prior_cat_eq_premium, prior_cat_wind_premium, prior_cat_flood_premium, prior_terrorism_premium}{Numeric: Premiums attributable to lines of coverage for each entity}
#'   \item{prior_total_cat_premium}{Numeric: The sum of premiums attributable to catastrophic lines of coverage only}
#'   \item{prior_all_risk_premium}{Numeric: The sum of premiums attributable to all lines of coverage excluding terrorism}
#'   \item{prior_risk_transfer_premium}{Numeric: The sum of premiums attributable to all lines of coverage}
#'   \item{prior_premium_incl_expenses}{Numeric: The sum of all premiums and expenses for the entity in the prior financial period}
#' }
"priors"

#' Specification of surcharges to be applied to entities for claims made in the current financial period
#'
#' Eventually left_join()ed on to \code{loss_run}.
#'
#' @format A data frame with 8 rows and 6 columns:
#' \describe{
#'   \item{bucket}{String: Unique identifier of the bucket}
#'   \item{name}{String: Display-quality descriptor of the bucket}
#'   \item{min, max}{Numeric: Bounds of the bucket; bucket bounds should be mutually
#'     exclusive, start at zero, and encapsulate the largest claim size}
#'   \item{percent_surcharge, dollar_surcharge}{Numeric: The percentage of the claim,
#'     or the dollar amount to be added to the premiums already allocated to the entity}
#' }
"count_buckets"

#' Claims made by entities in the current financial period
#'
#' See \code{count_buckets}.
#'
#' @format A data frame with 482 rows and 8 columns:
#' \describe{
#'   \item{claim_number}{String: Unique identifier of the claim}
#'   \item{location_dud}{String: Unique location identifier}
#'   \item{entity_id}{String: ID referencing one of \code{sov$entity_id} to which the claim applies}
#'   \item{date_of_loss}{Date: Date of the claim}
#'   \item{total_incurred}{Numeric: Size of the claim in dollars}
#'   \item{accident_description}{String: Accident description}
#'   \item{accident_location}{String: Accident Location}
#'   \item{year}{Numeric: Year of Loss}
#' }
"loss_run"

#' Rates which apply to TIVs to produce premiums
#'
#' The default rates appearing in this data frame are based on historical actuarial assessments
#'
#' @format A data frame with 38 rows and 5 columns:
#' \describe{
#'   \item{rate_type}{String: One of \code{c("aop", "cat_eq", "cat_flood", "cat_wind", "terror")} reflecting the type of coverage; See vignette for details}
#'   \item{rate_id}{String: ID reflecting the extent of the coverage of field \code{rate_type}; See vignette for details}
#'   \item{prior_rate, market_rate, model_rate}{Numeric: Prior, market, and model rates of premium coverage relative to TIV}
#' }
"rates"

#' All Other Peril relativities to apply by level of combustibility
#'
#' @format A data frame with 2 rows and 2 columns:
#' \describe{
#'   \item{aop_combustible}{String: Unique identifier reflecting a level of combustibility}
#'   \item{aop_combustible_relativity}{Numeric: Relativity to apply for indicated level of combustibility}
#' }
"combustible_rels"

#' All Other Peril relativities to apply by tier of sprinkler system
#'
#' @format A data frame with 4 rows and 2 columns:
#' \describe{
#'   \item{aop_sprinkler_tier}{String: Unique identifier reflecting a tier of sprinkler system}
#'   \item{aop_sprinkler_tier_relativity}{Numeric: Relativity to apply for indicated tier of sprinkler system}
#' }
"sprinkler_tier_rels"

#' Relativities to apply by business unit
#'
#' @format A data frame with 2 rows and 6 columns that contains
#'   Relativity to apply for indicated business unit.
#' \describe{
#'   \item{bu}{String: Unique identifier representing the business unit of the entity}
#'   \item{aop_bu_relativity}{AOP factor}
#'   \item{cat_eq_bu_relativity}{CAT-EQ factor}
#'   \item{cat_wind_bu_relativity}{CAT-Wind factor}
#'   \item{cat_flood_bu_relativity}{CAT-Flood factor}
#'   \item{terrorism_bu_relativity}{Terrorism factor}
#' }
"bu_rels"

#' A dictionary that maps raw data column names and values to prettified strings for UI display
#'
#' Used in conjunction with \code{propalloc::apply_labels()} to apply the dictionary's mapping to a data set
#'
#' @format A data frame with 1727 rows and 6 columns:
#' \describe{
#'   \item{dataset}{String: Identifier of the dataset. Function \code{apply_labels()} takes as arguments
#'   the data.frame of the raw data to be prettified, as well as the \code{dataset} string (amongst other arguments)
#'   to which it is assigned. Therefore, the \code{dataset} string need not match exactly the symbol of the
#'   data.frame of the raw data, although this might be the most-consistent approach. For example, this would
#'   be a valid call:
#'   \code{apply_labels(data = bu_rels, dict = propalloc::dictionary, dataset_name = "business_unit_rels")}
#'   assuming that the dataset value \code{"business_unit_rels"} appears in propalloc::dictionary. The most-consistent
#'   approach would be to use \code{"bu_rels"} as the \code{dataset} identifier, as in:
#'   \code{apply_labels(data = bu_rels, dict = propalloc::dictionary, dataset_name = "bu_rels")}
#'   }
#'   \item{variable}{String: Field name of the dataset which is to be prettified. eg. "bu".}
#'   \item{variable_label}{String: The prettified label to replace \code{variable} with for UI display. eg. "Business Unit".}
#'   \item{value}{String: If \code{NA}, then this field is not used. Else, the dictionary will also attempt to prettify
#'   values within the \code{variable} field. String values in this field should complete all possible values that the
#'   \code{variable} can take. eg. "bu_a", "bu_b".}
#'   \item{value_label}{String: The prettified label to replace \code{value} with for UI display.
#'   eg. "Business Unit A", "Business Unit B". If \code{NA}, then this field is not used. If \code{value} field is \code{NA},
#'   then this field should be too.}
#'   \item{value_order}{Numeric: An integer value used to order the prettified data.frame according to the appearance of values in
#'   field \code{value}.}
#' }
"dictionary"

