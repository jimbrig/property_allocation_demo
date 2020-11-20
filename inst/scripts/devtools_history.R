
#  ------------------------------------------------------------------------
#
# Title : propalloc - R package development history script
#    By : Jimmy Briggs
#  Date : 2020-04-02 (Started)
#
#  ------------------------------------------------------------------------

# devt libraries ----------------------------------------------------------
if (!require(pacman)) install.packages("pacman")
pacman::p_load(
  devtools,
  usethis,
  pkgbuild,
  pkgload,
  roxygen2,
  testthat,
  knitr,
  desc,
  fs
)

# initialize package ------------------------------------------------------
usethis::create_package("propalloc")

# create inst directory and store images
fs::dir_create("inst/images", recurse = TRUE)
# copy OW logo into inst/images

# ignore this script
usethis::use_build_ignore("devtools_history.R")
usethis::use_build_ignore("*.docx")

# setup git and github/bitbucket remotes
usethis::use_git()

# setup github project and upstream remotes
usethis::use_github(private = TRUE)
# run `git push --set-upstream origin master` in gitbash

# git sitrep
usethis::git_sitrep()

# setup namespace and roxygen
usethis::use_namespace()
usethis::use_roxygen_md()

# package R documentation and basic imports
usethis::use_package_doc()
usethis::use_tibble() # #' @return a [tibble][tibble::tibble-package]
usethis::use_pipe() # move to propaloc-package.R
usethis::use_tidy_eval() # move to propalloc-package.R

# document
devtools::document()

# set title and description for GH before making GH project
desc::desc_set(Title = "Oliver Wyman RShiny Property Allocation App",
               Description = "Property Allocation model in R Shiny that
               allocates an insured's prospective year property insurance
               costs.")

# document
devtools::document()

# DESCRIPTION -------------------------------------------------------------

# already set title and description for GH above

# authors
desc::desc_add_author(given = "Adam",
                      family = "Lewis",
                      role = "rev",
                      email = "adam.lewis@oliverwyman.com")

desc::desc_add_author(given = "Brian",
                      family = "Settle",
                      role = "rev",
                      email = "brian.settle@oliverwyman.com")

desc::desc_add_author(given = "Jack",
                      family = "Reardon",
                      role = "ctb")

desc::desc_add_author(given = "Oliver Wyman Actuarial Consulting, Inc.",
                      role = "fnd")

# package version
desc::desc_set_version("0.0.1")

#  R version
desc::desc_set("Depends", "R (>= 2.10)")

# license
usethis::use_mit_license(name = "Oliver Wyman Actuarial Consulting, Inc.")

# normalize
desc::desc_normalize() # usethis::use_tidy_description()

# README ------------------------------------------------------------------
usethis::use_readme_rmd()
usethis::use_logo("inst/images/ow-logo.png")
usethis::use_lifecycle_badge("Maturing")
usethis::use_badge(
  "Project Status: WIP",
  href = "http://www.repostatus.org/#wip",
  src = "https://www.repostatus.org/badges/latest/wip.svg"
)
knitr::knit("README.Rmd")

# directories -------------------------------------------------------------
dirs <- c("outputs", "inst/extdata", "inst/scripts", "inst/images",
          "inst/doc", "inst/app", "inst/app/www", "inst/RMD")
purrr::walk(dirs, fs::dir_create, recurse = TRUE)
rm(dirs)

# add ignores (git and build) to necessary directories
usethis::use_build_ignore("outputs/*")

ignore_dirs <- c("outputs", "data-raw/excel-models", "data-raw/working")
purrr::walk(ignore_dirs, function(x) {
  usethis::use_git_ignore(ignores = c("*", "!.gitignore"), directory = x)
})
rm(ignore_dirs)

# shiny app ---------------------------------------------------------------


# data --------------------------------------------------------------------
usethis::use_data_raw("dataprep")

# functions -------------------------------------------------------------
usethis::use_r("pkgdevt") # add detach_packages (attempt import)
usethis::use_r("apply_labels")
usethis::use_r("utils")
usethis::use_r("load_demo_data")
usethis::use_r("extract_costs")
usethis::use_r("entity_loss_summary")
usethis::use_r("apply_rels")
usethis::use_r("kkable")
usethis::use_r("merge_entity_data")
usethis::use_r("allocation")
usethis::use_r("globals")
usethis::use_r("market_rates")
usethis::use_r("run_app")
usethis::use_r("app_server")
usethis::use_r("app_ui")
usethis::use_r("ui_header")
usethis::use_r("ui_sidebar")
usethis::use_r("ui_body")
usethis::use_r("add_external_resources")
usethis::use_r("contacts")
usethis::use_r("insert_logo")
usethis::use_r("icon_text")
usethis::use_r("flucol")

# modules -----------------------------------------------------------------
usethis::use_r("mod_header_buttons")
usethis::use_r("mod_renewal_costs")
usethis::use_r("mod_sov")
usethis::use_r("mod_prior_premiums_module")
usethis::use_r("market_rates_module")
usethis::use_r("mod_loss_run_tab")
usethis::use_r("mod_inputs_menu")
usethis::use_r("mod_allocation_menu")

# module/app utils --------------------------------------------------------
usethis::use_r("tables")

# documentation -----------------------------------------------------------
usethis::use_vignette(name = "package-api", title = "Package API")
usethis::use_vignette(name = "allocation", title = "Allocation Walkthrough")
usethis::use_article(name = "setup-guide", title = "Setup Guide")

# news
usethis::use_news_md()

# tests -------------------------------------------------------------------
usethis::use_testthat()
usethis::use_test("pkgdevt")
usethis::use_test("apply_labels")

# build -------------------------------------------------------------------

# check build tools
pkgbuild::check_build_tools()
devtools::dev_sitrep()

# update devt packages
rstudioapi::restartSession()
devtools::update_packages("devtools")

# knitr README
knitr::knit("README.Rmd")

# update dependencies
detach_packages() # get_deps()
rstudioapi::restartSession()
attachment::att_to_description(
  extra.suggests = c("fst", "attempt", "devtools", "writexl",
                     "readr", "attachment", "roxygen2")
  )
attachment::create_dependencies_file(to = "inst/scripts/dependencies.R")

# document
devtools::document()

# check & test
devtools::check()
devtools::test()

# goodpractice check
goodpractice::gp()

# install
devtools::install()

# builds
devtools::build()
devtools::build_vignettes()
devtools::build_manual()

# release
spelling::update_wordlist()
devtools::spell_check()
devtools::release()

# R CMD CHECK RESULTS -----------------------------------------------------
#   Duration: 1m 48s
#
# 0 errors v | 0 warnings v | 0 notes v
#
# R CMD check succeeded


# extras ------------------------------------------------------------------

# usethis::use_coverage()
covr::package_coverage()

# propalloc Coverage: 23.45%
# R/app.R: 0.00%
# R/app_server.R: 0.00%
# R/app_ui.R: 0.00%
# R/kkable.R: 0.00%
# R/meta_contacts.R: 0.00%
# R/mod_count_buckets_tab.R: 0.00%
# R/mod_header_buttons.R: 0.00%
# R/mod_inputs_menu.R: 0.00%
# R/mod_loss_run_tab.R: 0.00%
# R/mod_market_rates_tab.R: 0.00%
# R/mod_priors_tab.R: 0.00%
# R/mod_rates_tab.R: 0.00%
# R/mod_rels_tab.R: 0.00%
# R/mod_renewal_costs_tab.R: 0.00%
# R/mod_sov_tab.R: 0.00%
# R/utils_app_ui.R: 0.00%
# R/utils_tables.R: 0.00%
# R/utils_dev.R: 35.00%
# R/utils_labelling.R: 35.71%
# R/utils_general.R: 55.22%
# R/alloc_driver_summary.R: 88.10%
# R/alloc_ingest_rels.R: 95.45%
# R/alloc_utils.R: 96.94%
# R/alloc_preliminary_allocation.R: 100.00%

knitr::knit("README.Rmd")
