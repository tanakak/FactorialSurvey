make_wtp_data <- function(n = 250, seed = 7) {
  set.seed(seed)
  d <- data.frame(
    quality = sample(0:1, n, replace = TRUE),
    speed   = stats::rnorm(n),
    price   = stats::runif(n, 100, 1000)
  )
  d$rating <- 10 * stats::plogis(
    0.9 * d$quality + 0.4 * d$speed - 0.002 * (d$price - 550) +
      stats::rnorm(n, sd = 0.4)
  )
  d
}

fit_wtp <- function(...) {
  fs_betareg(rating ~ quality + speed + price, data = make_wtp_data(),
             compute_me = FALSE, ...)
}

test_that("fs_mwtp returns every attribute by default", {
  res <- fs_mwtp(fit_wtp(), price = "price")

  expect_s3_class(res, "fs_mwtp")
  expect_setequal(res$estimates$variable, c("quality", "speed"))
  expect_named(res$estimates,
               c("variable", "mwtp", "std_error", "conf_low", "conf_high"))
  expect_false("(Intercept)" %in% res$estimates$variable)
  expect_false("(phi)" %in% res$estimates$variable)
  expect_equal(res$method, "delta")
  expect_equal(res$level, 0.95)
})

test_that("the point estimate is minus the ratio of coefficients", {
  fit <- fit_wtp()
  b <- coef(fit)
  res <- fs_mwtp(fit, variable = "quality", price = "price")

  expect_equal(res$estimates$mwtp,
               unname(-b["quality"] / b["price"]), tolerance = 1e-12)
})

test_that("the delta-method standard error matches a direct calculation", {
  fit <- fit_wtp()
  b <- coef(fit)
  v <- vcov(fit)
  res <- fs_mwtp(fit, variable = "quality", price = "price")

  b1 <- unname(b["quality"])
  b2 <- unname(b["price"])
  grad <- c(-1 / b2, b1 / b2^2)
  vsub <- v[c("quality", "price"), c("quality", "price")]
  expected <- sqrt(drop(t(grad) %*% vsub %*% grad))

  expect_equal(res$estimates$std_error, expected, tolerance = 1e-10)
})

test_that("the delta interval is symmetric about the estimate", {
  res <- fs_mwtp(fit_wtp(), variable = "quality", price = "price")
  e <- res$estimates

  expect_equal(e$conf_high - e$mwtp, e$mwtp - e$conf_low, tolerance = 1e-10)
  expect_equal(e$conf_high - e$mwtp, qnorm(0.975) * e$std_error,
               tolerance = 1e-10)
})

test_that("the confidence level is honoured", {
  fit <- fit_wtp()
  w95 <- fs_mwtp(fit, "quality", price = "price", level = 0.95)$estimates
  w90 <- fs_mwtp(fit, "quality", price = "price", level = 0.90)$estimates

  expect_lt(w90$conf_high - w90$conf_low, w95$conf_high - w95$conf_low)
})

test_that("Krinsky-Robb gives a comparable but asymmetric interval", {
  fit <- fit_wtp()
  kr <- fs_mwtp(fit, "quality", price = "price", method = "krinsky-robb",
                n_sim = 4000, seed = 1)$estimates
  dl <- fs_mwtp(fit, "quality", price = "price")$estimates

  expect_equal(kr$mwtp, dl$mwtp, tolerance = 1e-12)
  expect_lt(kr$conf_low, kr$mwtp)
  expect_gt(kr$conf_high, kr$mwtp)
  # Within a factor of two of the delta interval for a well-identified price
  expect_lt(abs(kr$conf_low - dl$conf_low), abs(dl$mwtp))
})

test_that("Krinsky-Robb is reproducible and restores the RNG state", {
  fit <- fit_wtp()
  a <- fs_mwtp(fit, "quality", price = "price", method = "krinsky-robb",
               n_sim = 500, seed = 3)$estimates
  b <- fs_mwtp(fit, "quality", price = "price", method = "krinsky-robb",
               n_sim = 500, seed = 3)$estimates
  expect_equal(a, b)

  set.seed(1234)
  before <- .Random.seed
  invisible(fs_mwtp(fit, "quality", price = "price",
                    method = "krinsky-robb", n_sim = 200, seed = 5))
  expect_identical(.Random.seed, before)
})

test_that("Fieller's interval brackets the estimate when price is precise", {
  fit <- fit_wtp()
  fi <- fs_mwtp(fit, "quality", price = "price", method = "fieller")$estimates

  expect_false(anyNA(c(fi$conf_low, fi$conf_high)))
  expect_lt(fi$conf_low, fi$mwtp)
  expect_gt(fi$conf_high, fi$mwtp)
})

test_that("Fieller warns and returns NA when the price effect is weak", {
  set.seed(99)
  n <- 60
  d <- data.frame(x = rnorm(n), price = rnorm(n, sd = 0.01))
  d$rating <- 10 * plogis(0.5 * d$x + rnorm(n, sd = 1))
  fit <- fs_betareg(rating ~ x + price, data = d, compute_me = FALSE)

  expect_warning(res <- fs_mwtp(fit, "x", price = "price", method = "fieller"),
                 "unbounded")
  expect_true(anyNA(c(res$estimates$conf_low, res$estimates$conf_high)))
})

test_that("a plain betareg object is accepted", {
  fit <- fit_wtp()
  from_wrapper <- fs_mwtp(fit, "quality", price = "price")$estimates
  from_betareg <- fs_mwtp(fit$model, "quality", price = "price")$estimates

  expect_equal(from_wrapper, from_betareg)
})

test_that("bad input is rejected", {
  fit <- fit_wtp()

  expect_error(fs_mwtp(fit), "name of a single coefficient")
  expect_error(fs_mwtp(fit, price = "nope"), "not found")
  expect_error(fs_mwtp(fit, "nope", price = "price"), "not found")
  expect_error(fs_mwtp(fit, "price", price = "price"),
               "must not include the price")
  expect_error(fs_mwtp(fit, "quality", price = "price", level = 1),
               "strictly between 0 and 1")
  expect_error(fs_mwtp(fit, "quality", price = "price", n_sim = -1),
               "positive whole number")
})

test_that("print and as.data.frame work", {
  res <- fs_mwtp(fit_wtp(), price = "price")

  expect_output(print(res), "Marginal willingness to pay")
  expect_output(print(res), "delta method")
  expect_s3_class(as.data.frame(res), "data.frame")
  expect_equal(nrow(as.data.frame(res)), 2L)
})
