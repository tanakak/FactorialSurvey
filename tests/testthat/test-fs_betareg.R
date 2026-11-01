make_data <- function(n = 150, seed = 42) {
  set.seed(seed)
  d <- data.frame(
    quality = rnorm(n),
    urgent  = sample(0:1, n, replace = TRUE),
    price   = runif(n, 100, 1000)
  )
  d$rating <- 10 * stats::plogis(
    0.7 * d$quality + 0.5 * d$urgent - 0.0015 * (d$price - 550) +
      stats::rnorm(n, sd = 0.4)
  )
  d
}

test_that("fs_betareg returns the documented structure", {
  d <- make_data()
  fit <- fs_betareg(rating ~ quality + urgent + price, data = d,
                    compute_me = FALSE, cluster = FALSE)

  expect_s3_class(fit, "fs_betareg")
  expect_named(fit, c("model", "vcov", "cluster", "marginal_effects",
                      "marginal_effects_rescaled",
                      "gof", "scale", "transform", "rescale_factor", "call"))
  expect_s3_class(fit$model, "betareg")
  expect_true(fit$transform)
  expect_equal(fit$scale, c(0, 10))
})

test_that("the fit statistics are finite and internally consistent", {
  d <- make_data()
  g <- fs_betareg(rating ~ quality + urgent + price, data = d,
                  compute_me = FALSE, cluster = FALSE)$gof

  # The original implementation read a non-existent `loglik.null` component
  # and silently produced numeric(0) here.
  expect_length(g$pseudo_r2_cox_snell, 1L)
  expect_true(is.finite(g$pseudo_r2_cox_snell))
  expect_true(is.finite(g$logLik_null))
  expect_true(is.finite(g$pseudo_r2_betareg))

  # The null log-likelihood comes from a genuine intercept-only fit
  null_fit <- betareg::betareg(rating ~ 1,
                               data = transform(d, rating = to_unit(rating)))
  expect_equal(g$logLik_null, as.numeric(logLik(null_fit)), tolerance = 1e-6)

  # A model with predictors fits better than the null model
  expect_gt(g$logLik, g$logLik_null)
  expect_equal(g$lr_stat, 2 * (g$logLik - g$logLik_null), tolerance = 1e-10)
  expect_equal(g$lr_df, 3L)
  expect_lt(g$lr_p_value, 0.05)
})

test_that("the Cox-Snell pseudo R-squared stays inside [0, 1)", {
  d <- make_data()
  g <- fs_betareg(rating ~ quality + urgent + price, data = d,
                  compute_me = FALSE, cluster = FALSE)$gof

  expect_gte(g$pseudo_r2_cox_snell, 0)
  expect_lt(g$pseudo_r2_cox_snell, 1)
  expect_equal(g$pseudo_r2_cox_snell,
               1 - exp(2 * (g$logLik_null - g$logLik) / g$nobs),
               tolerance = 1e-12)

  # McFadden's ratio would be out of range here: beta regression is a
  # continuous density, so both log-likelihoods are positive.
  expect_gt(g$logLik, 0)
  expect_false(1 - g$logLik / g$logLik_null > 0)
})

test_that("the response is left untouched in the caller's data", {
  d <- make_data()
  original <- d
  invisible(fs_betareg(rating ~ quality, data = d, compute_me = FALSE, cluster = FALSE))
  expect_equal(d, original)
})

test_that("an arbitrary rating scale is handled", {
  d <- make_data()
  d$rating100 <- d$rating * 10

  fit10 <- fs_betareg(rating ~ quality, data = d, compute_me = FALSE, cluster = FALSE)
  fit100 <- fs_betareg(rating100 ~ quality, data = d, scale = c(0, 100),
                       compute_me = FALSE, cluster = FALSE)

  # Rescaling the outcome linearly leaves the beta model unchanged
  expect_equal(unname(coef(fit10)), unname(coef(fit100)), tolerance = 1e-6)
  expect_equal(fit100$rescale_factor, 10 * fit10$rescale_factor,
               tolerance = 1e-8)
})

test_that("a 1 to 7 Likert scale is accepted", {
  set.seed(11)
  d <- data.frame(x = rnorm(120))
  d$score <- pmin(pmax(round(4 + 1.2 * d$x + rnorm(120)), 1), 7)

  fit <- fs_betareg(score ~ x, data = d, scale = c(1, 7), compute_me = FALSE, cluster = FALSE)
  expect_s3_class(fit, "fs_betareg")
  expect_true(is.finite(fit$gof$pseudo_r2_cox_snell))
})

test_that("transform = FALSE requires data already inside (0, 1)", {
  d <- make_data()
  d$unit <- to_unit(d$rating)

  fit <- fs_betareg(unit ~ quality, data = d, transform = FALSE,
                    compute_me = FALSE, cluster = FALSE)
  expect_false(fit$transform)
  expect_true(is.na(fit$rescale_factor))
  expect_null(fit$marginal_effects_rescaled)

  expect_error(
    fs_betareg(rating ~ quality, data = d, transform = FALSE,
               compute_me = FALSE, cluster = FALSE),
    "strictly inside"
  )
})

test_that("bad input is rejected with a helpful message", {
  d <- make_data()

  expect_error(fs_betareg(rating ~ quality, data = as.matrix(d), cluster = FALSE),
               "must be a data frame")
  expect_error(fs_betareg("rating ~ quality", data = d, cluster = FALSE), "must be a formula")
  expect_error(fs_betareg(missing_y ~ quality, data = d, cluster = FALSE), "not found in")
  expect_error(fs_betareg(rating ~ quality, data = d, scale = c(10, 0), cluster = FALSE),
               "scale\\[1\\] < scale\\[2\\]")
  expect_error(fs_betareg(rating ~ quality, data = d, scale = c(0, 5), cluster = FALSE),
               "outside `scale`")

  d$rating[1] <- NA
  expect_error(fs_betareg(rating ~ quality, data = d, cluster = FALSE), "missing values")

  d2 <- make_data()
  d2$chr <- as.character(d2$rating)
  expect_error(fs_betareg(chr ~ quality, data = d2, cluster = FALSE), "must be numeric")
})

test_that("the extractor methods forward to the betareg fit", {
  d <- make_data()
  fit <- fs_betareg(rating ~ quality + price, data = d, compute_me = FALSE, cluster = FALSE)

  expect_equal(coef(fit), coef(fit$model))
  expect_equal(vcov(fit), vcov(fit$model))
  expect_equal(as.numeric(logLik(fit)), as.numeric(logLik(fit$model)))
  expect_equal(nobs(fit), nobs(fit$model))
  expect_length(fitted(fit), nrow(d))
})

test_that("print and summary produce output without printing on fit", {
  d <- make_data()

  expect_silent(fit <- fs_betareg(rating ~ quality, data = d,
                                  compute_me = FALSE, cluster = FALSE))
  expect_output(print(fit), "Factorial survey beta regression")
  expect_s3_class(summary(fit), "summary.fs_betareg")
  expect_output(print(summary(fit)), "Pseudo R2 \\(Cox-Snell\\)")
  expect_output(print(summary(fit)), "Pseudo R2 \\(betareg\\)")
})

test_that("verbose reports progress", {
  d <- make_data()
  expect_message(fs_betareg(rating ~ quality, data = d, compute_me = FALSE,
                            verbose = TRUE, cluster = FALSE),
                 "Fitting the beta regression")
})

test_that("marginal effects are rescaled onto the rating scale", {
  skip_if_not_installed("margins")

  d <- make_data()
  fit <- fs_betareg(rating ~ quality + price, data = d, compute_me = TRUE, cluster = FALSE)

  skip_if(is.null(fit$marginal_effects), "margins returned nothing")

  expect_s3_class(fit$marginal_effects, "margins")
  expect_s3_class(fit$marginal_effects_rescaled, "data.frame")

  unit <- summary(fit$marginal_effects)
  rescaled <- fit$marginal_effects_rescaled
  expect_equal(rescaled$AME, unit$AME * fit$rescale_factor,
               tolerance = 1e-8, ignore_attr = TRUE)
  expect_equal(rescaled$SE, unit$SE * fit$rescale_factor,
               tolerance = 1e-8, ignore_attr = TRUE)
  expect_equal(as.character(rescaled$factor), as.character(unit$factor))
})
