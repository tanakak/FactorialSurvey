test_that("fs_defficient generates correct number of designs and groups", {
  design <- fs_defficient(levels = c(3, 2, 2), n_designs = 12, n_groups = 3, seed = 123)
  expect_s3_class(design, "data.frame")
  expect_equal(nrow(design), 12)
  expect_true("group" %in% names(design))
  expect_equal(length(unique(design$group)), 3)
})

test_that("fs_defficient uses default variable names correctly", {
  result <- fs_defficient(levels = c(2, 2), n_designs = 4, n_groups = 2, seed = 42)
  expect_true(all(c("x1", "x2") %in% names(result)))
})

test_that("fs_defficient accepts custom variable names", {
  result <- fs_defficient(levels = c(3, 2), var_names = c("Alert", "Time"), n_designs = 6, n_groups = 2)
  expect_true(all(c("Alert", "Time", "group") %in% names(result)))
})

test_that("fs_defficient returns error on invalid inputs", {
  expect_error(fs_defficient(levels = c(1, 2), n_designs = 6), "must be integers >= 2")
  expect_error(fs_defficient(levels = c(3, 2), var_names = c("OnlyOne"), n_designs = 6),
               "Length of 'var_names' must match")
})

test_that("fs_defficient produces reproducible output with seed", {
  r1 <- fs_defficient(levels = c(2, 2), n_designs = 4, seed = 999)
  r2 <- fs_defficient(levels = c(2, 2), n_designs = 4, seed = 999)
  expect_equal(r1, r2)
})
