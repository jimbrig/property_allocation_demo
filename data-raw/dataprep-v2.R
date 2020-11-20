
library(fs)
library(dplyr)
library(purrr)
library(usethis)

files <- fs::dir_ls("data-raw", type = "file", glob = "*.R") %>%
  setdiff("data-raw/dataprep.R")

purrr::walk(files, source)

usethis::use_data(
  sov,
  renewal_costs,
  priors,
  count_buckets,
  loss_run,
  rates,
  bu_rels,
  combustible_rels,
  sprinkler_tier_rels,
  overwrite = TRUE
)

tools::resaveRdaFiles("data")

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
    file = "R/meta_data_REMOVE_ONCE_USED.R",
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

# copy html
fs::dir_create("inst/reports/dataviz", recurse = TRUE)
fs::dir_ls(fs::path(meta_dir)) %>%
  purrr::walk2(., paste0("inst/reports/dataviz/", basename(.)), fs::file_copy, overwrite = TRUE)

