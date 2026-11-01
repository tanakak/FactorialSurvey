sim_design <- function() {
  fs_defficient(list(damage = c("minor", "severe"), cost = c(100, 300, 500)),
                n_vignettes = 6, n_blocks = 2, seed = 1, verbose = FALSE)
}
true_coef <- c("(Intercept)" = 1, damagesevere = 0.8, cost = -0.004)

test_that("simulated ratings have the documented structure", {
  sim <- fs_simulate_ratings(sim_design(), n_respondents = 40,
                             coef = true_coef, formula = ~ damage + cost,
                             seed = 1)
  expect_s3_class(sim, "fs_assignment")
  expect_equal(nrow(sim), 40L * 3L)
  expect_true(all(sim$rating %in% 0:10))
  expect_equal(attr(sim, "truth")$coef, true_coef)
  expect_equal(length(unique(sim$respondent)), 40L)
})

test_that("simulation is reproducible and leaves the RNG untouched", {
  set.seed(10)
  before <- .Random.seed
  a <- fs_simulate_ratings(sim_design(), 30, true_coef, ~ damage + cost,
                           seed = 2)
  expect_identical(.Random.seed, before)
  b <- fs_simulate_ratings(sim_design(), 30, true_coef, ~ damage + cost,
                           seed = 2)
  expect_identical(a$rating, b$rating)
})

test_that("an analysis of simulated data recovers willingness to pay", {
  skip_on_cran()
  sim <- fs_simulate_ratings(sim_design(), n_respondents = 3000,
                             coef = true_coef, formula = ~ damage + cost,
                             phi = 20, sd_respondent = 0, round = FALSE,
                             seed = 3)
  fit <- fs_betareg(rating ~ damage + cost, data = sim,
                    cluster = ~ respondent, compute_me = FALSE)
  expect_equal(unname(coef(fit)[c("damagesevere", "cost")]),
               unname(true_coef[c("damagesevere", "cost")]),
               tolerance = 0.1)
  w <- as.data.frame(fs_mwtp(fit, price = "cost"))
  expect_lt(abs(w$mwtp - 200), 3 * w$std_error)
})

test_that("binary responses can be simulated and analysed", {
  skip_on_cran()
  sim <- fs_simulate_ratings(sim_design(), n_respondents = 3000,
                             coef = true_coef, formula = ~ damage + cost,
                             response = "binary", sd_respondent = 0,
                             seed = 4)
  expect_true(all(sim$response %in% 0:1))
  fit <- fs_logit(response ~ damage + cost, data = sim,
                  cluster = ~ respondent, compute_me = FALSE)
  expect_equal(unname(coef(fit)), unname(true_coef), tolerance = 0.15)
})

test_that("a schedule from fs_assign() is used as given", {
  sched <- suppressMessages(fs_assign(sim_design(), 12, seed = 1))
  sim <- fs_simulate_ratings(sched, coef = true_coef,
                             formula = ~ damage + cost, seed = 1)
  expect_equal(sim$respondent, sched$respondent)
  expect_equal(sim$vignette, sched$vignette)
})

test_that("the rating scale and the response name can be changed", {
  sim <- fs_simulate_ratings(sim_design(), 20, true_coef, ~ damage + cost,
                             scale = c(1, 7), name = "score", seed = 1)
  expect_true(all(sim$score >= 1 & sim$score <= 7))
})

test_that("bad input is rejected with helpful messages", {
  d <- sim_design()
  expect_error(fs_simulate_ratings(d, 10, c(a = 1), ~ damage + cost),
               "\"damagesevere\"")
  expect_error(fs_simulate_ratings(d, coef = true_coef,
                                   formula = ~ damage + cost),
               "n_respondents")
  expect_error(fs_simulate_ratings(d, 10, true_coef, ~ nope), "not found")
  expect_error(fs_simulate_ratings(d, 10, true_coef, rating ~ damage),
               "one-sided")
  expect_error(fs_simulate_ratings(d, 10, true_coef, ~ damage + cost,
                                   phi = 0), "positive")
  expect_error(fs_simulate_ratings(d, 10, true_coef, ~ damage + cost,
                                   sd_respondent = -1), "non-negative")
  expect_error(fs_simulate_ratings(1:3, 10, true_coef, ~ damage + cost),
               "data frame")
  expect_error(fs_simulate_ratings(d, 10, unname(true_coef), ~ damage + cost),
               "named numeric")
})
