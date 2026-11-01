pw_design <- function() {
  fs_defficient(list(damage = c("minor", "severe"), cost = c(100, 300, 500)),
                n_vignettes = 6, n_blocks = 2, seed = 1, verbose = FALSE)
}
pw_coef <- c("(Intercept)" = 1, damagesevere = 0.4, cost = -0.003)

test_that("fs_power returns one row per sample size and term", {
  pw <- fs_power(pw_design(), c(30, 90), coef = pw_coef,
                 formula = ~ damage + cost, price = "cost", n_sim = 15,
                 seed = 1, verbose = FALSE)
  expect_s3_class(pw, "fs_power")
  expect_equal(nrow(pw), 4L)
  expect_equal(pw$n_respondents, c(30L, 30L, 90L, 90L))
  expect_equal(pw$term, rep(c("damagesevere", "cost"), 2))
  expect_true(all(pw$power >= 0 & pw$power <= 1))
  expect_equal(pw$mwtp[pw$term == "damagesevere"], rep(0.4 / 0.003, 2))
  expect_true(all(is.na(pw$mwtp_halfwidth[pw$term == "cost"])))
  expect_output(print(pw), "Power analysis")
  expect_s3_class(plot(pw), "ggplot")
})

test_that("power and precision improve with more respondents", {
  skip_on_cran()
  pw <- fs_power(pw_design(), c(20, 400), coef = pw_coef,
                 formula = ~ damage + cost, price = "cost", n_sim = 40,
                 seed = 2, verbose = FALSE)
  d <- pw[pw$term == "damagesevere", ]
  expect_gt(d$power[2], d$power[1])
  expect_lt(d$mwtp_halfwidth[2], d$mwtp_halfwidth[1])
  expect_gt(d$power[2], 0.8)
})

test_that("a null effect is rejected at about the nominal rate", {
  skip_on_cran()
  co <- c("(Intercept)" = 1, damagesevere = 0, cost = -0.003)
  pw <- fs_power(pw_design(), 200, coef = co, formula = ~ damage + cost,
                 n_sim = 200, seed = 3, verbose = FALSE)
  expect_lt(pw$power[pw$term == "damagesevere"], 0.12)
})

test_that("binary responses are supported", {
  skip_on_cran()
  pw <- fs_power(pw_design(), 100, coef = pw_coef,
                 formula = ~ damage + cost, response = "binary",
                 n_sim = 10, seed = 4, verbose = FALSE)
  expect_equal(nrow(pw), 2L)
})

test_that("fs_power is reproducible and leaves the RNG untouched", {
  set.seed(9)
  before <- .Random.seed
  a <- fs_power(pw_design(), 40, coef = pw_coef, formula = ~ damage + cost,
                n_sim = 5, seed = 5, verbose = FALSE)
  expect_identical(.Random.seed, before)
  b <- fs_power(pw_design(), 40, coef = pw_coef, formula = ~ damage + cost,
                n_sim = 5, seed = 5, verbose = FALSE)
  expect_equal(a, b)
})

test_that("fs_power validates its input", {
  d <- pw_design()
  expect_error(fs_power(d, 1, pw_coef, ~ damage + cost), "at least 2")
  expect_error(fs_power(d, 40, pw_coef, ~ damage + cost, alpha = 0),
               "between 0 and 1")
  expect_error(fs_power(d, 40, pw_coef, ~ damage + cost, price = "nope"),
               "must name one")
  expect_error(fs_power(d, 40, c(pw_coef[1:2], cost = 0.01), ~ damage + cost,
                        price = "cost"), "must be negative")
  expect_error(fs_power(d, 40, c(a = 1), ~ damage + cost, verbose = FALSE),
               "must match")
  expect_message(fs_power(d, 20, pw_coef, ~ damage + cost, n_sim = 2,
                          seed = 1), "Simulating")
})
