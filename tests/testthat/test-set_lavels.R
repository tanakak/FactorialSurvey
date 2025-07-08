test_that("set_lavels correctly assigns factor labels", {
  df <- data.frame(Alert = c(1, 2), Time = c(1, 2))
  labels <- list(Alert = c("Low", "High"), Time = c("Morning", "Evening"))
  df_labeled <- set_lavels(df, labels)
  expect_equal(levels(df_labeled$Alert), c("Low", "High"))
  expect_equal(levels(df_labeled$Time), c("Morning", "Evening"))
})

test_that("set_lavels throws error for unmatched variable names", {
  df <- data.frame(A = c(1, 2))
  expect_error(set_lavels(df, list(B = c("x", "y"))))
})
