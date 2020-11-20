
#  ------------------------------------------------------------------------
#
# Title : propalloc - Installation, Dependencies, and Streamlined App Script
#    By : Jimmy Briggs <jimmy.briggs@oliverwyman.com>
#  Date : 2020-07-07
#
#  ------------------------------------------------------------------------

# note: must have GitHub PAT (Personal Access Token) setup before using this script
# due to propalloc being a private hosted GitHub repository.

if (!require(pacman)) install.packages("pacman")

pacman::p_load(
  devtools,
  usethis
)

options(repos = getOption("repos")["CRAN"])

# pacman::p_load_current_gh(
#   "jimbrig/propalloc[@develop]"
# )

devtools::install_github(
  "jimbrig/propalloc",
  ref = "develop"
)

library(propalloc)

propalloc::run_app()
