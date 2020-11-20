if (!require(chameleon)) remotes::install_github("ThinkR-open/chameleon")
library(chameleon)

chameleon::build_pkgdown(
  move = TRUE, clean_after = FALSE, favicon = "inst/images/favicon.ico"
)
chameleon::create_pkg_desc_file()

chameleon::build_book()
chameleon::create_book()

chameleon::open_pkgdown_function()
chameleon::open_guide_function()
