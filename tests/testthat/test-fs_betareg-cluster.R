make_panel <- function(n_resp = 60, k = 4, seed = 11) {
  set.seed(seed)
  n <- n_resp * k
  d <- data.frame(
    id      = rep(seq_len(n_resp), each = k),
    quality = sample(0:1, n, replace = TRUE),
    price   = sample(c(100, 300, 500), n, replace = TRUE)
  )
  taste <- rnorm(n_resp, sd = 0.6)[d$id]
  d$rating <- round(10 * plogis(0.7 * d$quality - 0.003 * (d$price - 300) +
                                  taste + rnorm(n, sd = 0.4)))
  d
}

test_that("cluster-robust covariance matches sandwich::vcovCL", {
  d <- make_panel()
  fit <- fs_betareg(rating ~ quality + price, data = d, cluster = ~ id,
                    compute_me = FALSE)
  ref <- sandwich::vcovCL(fit$model, cluster = d$id, type = "HC0",
                          cadjust = TRUE)
  expect_equal(vcov(fit), ref)
  expect_equal(fit$cluster$n_clusters, 60L)
  expect_equal(fit$cluster$name, "id")

  naive <- fs_betareg(rating ~ quality + price, data = d, cluster = FALSE,
                      compute_me = FALSE)
  expect_null(naive$cluster)
  expect_equal(vcov(naive), vcov(naive$model))
  # With a respondent effect, the intercept SE grows once clustering is used
  expect_gt(sqrt(vcov(fit)[1, 1]), sqrt(vcov(naive)[1, 1]))
  # Point estimates are unaffected
  expect_equal(coef(fit), coef(naive))
})

test_that("cluster can be a formula, a column name or a vector", {
  d <- make_panel()
  a <- fs_betareg(rating ~ quality, data = d, cluster = ~ id,
                  compute_me = FALSE)
  b <- fs_betareg(rating ~ quality, data = d, cluster = "id",
                  compute_me = FALSE)
  v <- fs_betareg(rating ~ quality, data = d, cluster = d$id,
                  compute_me = FALSE)
  expect_equal(vcov(a), vcov(b))
  expect_equal(vcov(a), vcov(v))
  expect_equal(v$cluster$name, "the supplied vector")
})

test_that("rows dropped for missing covariates are aligned with clusters", {
  d <- make_panel()
  d$quality[c(3, 50)] <- NA
  fit <- fs_betareg(rating ~ quality, data = d, cluster = ~ id,
                    compute_me = FALSE)
  keep <- !is.na(d$quality)
  ref <- sandwich::vcovCL(fit$model, cluster = d$id[keep], type = "HC0",
                          cadjust = TRUE)
  expect_equal(vcov(fit), ref)
})

test_that("leaving cluster unspecified warns; FALSE is silent", {
  d <- make_panel()
  expect_warning(fs_betareg(rating ~ quality, data = d, compute_me = FALSE),
                 "independent")
  expect_silent(fs_betareg(rating ~ quality, data = d, cluster = FALSE,
                           compute_me = FALSE))
})

test_that("bad cluster specifications are rejected", {
  d <- make_panel()
  expect_error(fs_betareg(rating ~ quality, data = d, cluster = ~ nope,
                          compute_me = FALSE), "not found")
  expect_error(fs_betareg(rating ~ quality, data = d, cluster = ~ id + price,
                          compute_me = FALSE), "one variable")
  expect_error(fs_betareg(rating ~ quality, data = d, cluster = 1:3,
                          compute_me = FALSE), "one element per row")
  d$id[5] <- NA
  expect_error(fs_betareg(rating ~ quality, data = d, cluster = ~ id,
                          compute_me = FALSE), "missing values")
  d$id <- 1
  expect_error(fs_betareg(rating ~ quality, data = d, cluster = ~ id,
                          compute_me = FALSE), "two distinct")
})

test_that("summary and print report clustered standard errors", {
  d <- make_panel()
  fit <- fs_betareg(rating ~ quality + price, data = d, cluster = ~ id,
                    compute_me = FALSE)
  s <- summary(fit)
  se_mean <- s$model_summary$coefficients$mean[, "Std. Error"]
  expect_equal(se_mean, sqrt(diag(vcov(fit)))[names(se_mean)])
  expect_output(print(fit), "clustered by id \\(60 clusters\\)")
  expect_output(print(s), "clustered by id")
})

test_that("fs_mwtp inherits the clustered covariance", {
  d <- make_panel()
  fc <- fs_betareg(rating ~ quality + price, data = d, cluster = ~ id,
                   compute_me = FALSE)
  fn <- fs_betareg(rating ~ quality + price, data = d, cluster = FALSE,
                   compute_me = FALSE)
  wc <- as.data.frame(fs_mwtp(fc, variable = "quality", price = "price"))
  wn <- as.data.frame(fs_mwtp(fn, variable = "quality", price = "price"))
  expect_equal(wc$mwtp, wn$mwtp)
  expect_false(isTRUE(all.equal(wc$std_error, wn$std_error)))
})

test_that("marginal effects use the clustered covariance", {
  skip_if_not_installed("margins")
  d <- make_panel()
  fc <- fs_betareg(rating ~ quality + price, data = d, cluster = ~ id)
  fn <- fs_betareg(rating ~ quality + price, data = d, cluster = FALSE)
  sc <- summary(fc$marginal_effects)
  sn <- summary(fn$marginal_effects)
  expect_equal(sc$AME, sn$AME)
  expect_false(isTRUE(all.equal(sc$SE, sn$SE)))
})
