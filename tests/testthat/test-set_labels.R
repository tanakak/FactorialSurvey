test_that("numeric codes starting at one are labelled in order", {
  d <- data.frame(alert = c(1, 2, 1), timing = c(2, 1, 2))
  out <- set_labels(d, list(alert = c("Low", "High"),
                            timing = c("Morning", "Evening")))

  expect_s3_class(out$alert, "factor")
  expect_equal(levels(out$alert), c("Low", "High"))
  expect_equal(levels(out$timing), c("Morning", "Evening"))
  expect_equal(as.character(out$alert), c("Low", "High", "Low"))
})

test_that("columns that are not named are left alone", {
  d <- data.frame(alert = c(1, 2), rating = c(7.5, 3.5))
  out <- set_labels(d, list(alert = c("Low", "High")))

  expect_equal(out$rating, d$rating)
  expect_type(out$rating, "double")
})

test_that("explicit levels handle codes that do not start at one", {
  d <- data.frame(alert = c(0, 1, 0))
  out <- set_labels(d, list(alert = c("Absent", "Present")),
                    levels_list = list(alert = c(0, 1)))

  expect_equal(as.character(out$alert), c("Absent", "Present", "Absent"))
})

test_that("character columns are labelled in sorted order", {
  d <- data.frame(size = c("s", "l", "s"), stringsAsFactors = FALSE)
  out <- set_labels(d, list(size = c("Large", "Small")))

  expect_equal(as.character(out$size), c("Small", "Large", "Small"))
})

test_that("existing factors keep their level order", {
  d <- data.frame(size = factor(c("s", "l"), levels = c("s", "l")))
  out <- set_labels(d, list(size = c("Small", "Large")))

  expect_equal(levels(out$size), c("Small", "Large"))
})

test_that("a design from fs_defficient can be labelled", {
  d <- fs_defficient(list(alert = 1:2, timing = 1:2),
                     n_vignettes = 4, seed = 1, verbose = FALSE)
  out <- set_labels(as.data.frame(d),
                    list(alert = c("Low", "High"),
                         timing = c("Morning", "Evening")))

  expect_s3_class(out$alert, "factor")
  expect_equal(nrow(out), 4L)
})

test_that("mismatched names and lengths are caught", {
  d <- data.frame(a = c(1, 2))

  expect_error(set_labels(d, list(b = c("x", "y"))), "not found in `df`")
  expect_error(set_labels(1:5, list(a = "x")), "must be a data frame")
  expect_error(set_labels(d, list(c("x", "y"))), "named list")
  expect_error(set_labels(d, list(a = c("x", "y")),
                          levels_list = list(b = 1:2)),
               "not found in `label_list`")
})

test_that("a label count that contradicts the levels is caught", {
  # A factor column states its own levels, so a mismatch is unambiguous
  d <- data.frame(size = factor(c("s", "l")))
  expect_error(set_labels(d, list(size = c("Small", "Medium", "Large"))),
               "3 label")

  # So does an explicit levels_list
  d2 <- data.frame(a = c(1, 2))
  expect_error(set_labels(d2, list(a = c("x", "y", "z")),
                          levels_list = list(a = 1:2)),
               "3 label")
})

test_that("extra labels for a numeric code column create unused levels", {
  # Legitimate when a block happens not to contain every level
  d <- data.frame(a = c(1, 2))
  out <- set_labels(d, list(a = c("x", "y", "z")))
  expect_equal(levels(out$a), c("x", "y", "z"))
  expect_equal(as.character(out$a), c("x", "y"))
})

test_that("uncovered values error under strict and warn otherwise", {
  d <- data.frame(a = c(1, 2, 3))
  labs <- list(a = c("x", "y"))

  expect_error(set_labels(d, labs, levels_list = list(a = 1:2)),
               "not covered")
  expect_warning(out <- set_labels(d, labs, levels_list = list(a = 1:2),
                                   strict = FALSE),
                 "not covered")
  expect_true(is.na(out$a[3]))
})
