test_that("fs_defficient generates correct number of profiles and groups", {
  out <- fs_defficient(levels = c(2, 3), n_designs = 6, n_groups = 2, var_names = c("Alert", "Time"), seed = 123)
  expect_equal(nrow(out), 6)
  expect_true("Group" %in% colnames(out))
  expect_equal(length(unique(out$Group)), 2)
})

test_that("fs_defficient throws error for invalid inputs", {
  expect_error(fs_defficient(levels = c(1, 2), n_designs = 3, n_groups = 1))  # level < 2
  expect_error(fs_defficient(levels = c(2, 2), n_designs = 5, n_groups = 2))  # too many designs
  expect_error(fs_defficient(levels = c(2, 2), n_designs = 4, n_groups = 5))  # too many groups
  expect_error(fs_defficient(levels = c(2, 2), n_designs = 4, n_groups = 2, var_names = c("OnlyOne")))  # name mismatch
})
