
require(data.table)
require(forcats)
require(tibble)

library(dplyr)

bu_rels <- tibble::tibble(
  bu = c("XYZ", "ABC"),
  aop_bu_relativity = c(1, 2.13),
  cat_eq_bu_relativity = c(1, 1.44),
  cat_wind_bu_relativity = c(1, 1),
  cat_flood_bu_relativity = c(1, 1),
  terrorism_bu_relativity = c(1, 2.2)
) %>%
  dplyr::mutate(bu = as.factor(bu))

combustible_rels <- tibble::tibble(
  combustible = factor(c("Non-Combustible", "Combustible")),
  combustible_relativity = c(1, 1.15)
)

sprinkler_tier_rels <- tibble::tibble(
  sprinkler_tier = factor(c(paste0("Tier ", 0:3))),
  sprinkler_tier_relativity = c(1, 3.21, 1.67, 1.28)
)

glimpse(bu_rels)
glimpse(combustible_rels)
glimpse(sprinkler_tier_rels)
