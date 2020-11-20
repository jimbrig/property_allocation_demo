context("rebalance_premiums")


test_that("rebalance premiums behaves as expected", {
  test_data <- data.frame(premiums = 1:3, something_else = c("Jack", "Jimmy", "Adam"))

  rebalanced_total <- 12

  rebalanced_data <- test_data %>% rebalance_premiums("premiums", rebalanced_total)

  expect_equal(rebalanced_data %>% nrow(), test_data %>% nrow())
  expect_equal(rebalanced_data %>%
                 dplyr::mutate(premiums_pc = premiums / sum(.data$premiums)) %>%
                 dplyr::pull(premiums_pc),
               test_data %>%
                 dplyr::mutate(premiums_pc = premiums / sum(.data$premiums)) %>%
                 dplyr::pull(premiums_pc))
  expect_equal(sum(rebalanced_data$premiums), rebalanced_total)
})
