# context("package development")
#
# testthat::test_that("test detach_packages",{
#     test <- detach_packages()
#     ok <- all(purrr::map_lgl(test, ~ is.null(.x)))
#     testthat::expect_true(ok)
# })
