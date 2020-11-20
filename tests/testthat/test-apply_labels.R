context("apply labels")

test_that("test labeling", {

  dictionary <- data.frame(
    var = c(rep("x1", 2),
            rep("x2", 3),
            rep("x3", 5)),
    var_lab = c(rep("Column 1", 2),
                rep("Column 2", 3),
                rep("Column 3", 5)),
    val = c(c(TRUE, FALSE),
            c(1:3),
            letters[1:5]),
    val_lab = c(c("YES", "NO"),
                paste0("Group ", c(1:3)),
                paste0("Area ", LETTERS[1:5])),

    stringsAsFactors = FALSE
  )

  dat <- data.frame(
    "x1" = rep(c(TRUE, FALSE), 15),
    "x2" = rep(c(1:3), 10),
    "x3" = rep(letters[1:5], 6),

    stringsAsFactors = FALSE
  )

  out <- apply_labels(dat,
                      dictionary,
                      from = "val",
                      to = "val_lab",
                      by = "var",
                      names_from = "var",
                      names_to = "var_lab")

  expect_identical(names(out), c("Column 1", "Column 2", "Column 3"))

  expect_equal(unique(out[[1]]), c("YES", "NO"))

  expect_equal(unique(out[[2]]), c("Group 1", "Group 2", "Group 3"))

  expect_equal(unique(out[[3]]), paste0("Area ", LETTERS[1:5]))

})
