
#  ------------------------------------------------------------------------
#
# Title : Build Script
#    By : Jimmy Briggs <jimmy.briggs@oliverwyman.com>
#  Date : 2020-06-03
#
#  ------------------------------------------------------------------------

devtools::document()

attachment::att_amend_desc(
  extra.suggests = c(
    "devtools",
    "attachment",
    "roxygen2"
  )
)

attachment::create_dependencies_file(to = "inst/scripts/dependencies.R")

devtools::document()

devtools::check()

tools::resaveRdaFiles("data", compress = "auto")

load("data/dictionary.rda")
dictionary <- dictionary
save(dictionary, file = "data/dictionary.rda")

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

deps <- renv::dependencies()

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
