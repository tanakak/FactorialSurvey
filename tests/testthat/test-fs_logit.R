make_binary <- function(n_resp = 100, k = 4, seed = 5) {
  set.seed(seed)
  n <- n_resp * k
  d <- data.frame(
    id      = rep(seq_len(n_resp), each = k),
    quality = sample(0:1, n, replace = TRUE),
    type    = factor(sample(c("a", "b", "c"), n, replace = TRUE)),
    cost    = sample(c(500, 1000, 2000), n, replace = TRUE)
  )
  taste <- rnorm(n_resp, sd = 0.5)[d$id]
  d$support <- rbinom(n, 1, plogis(1 + 0.8 * d$quality - 0.001 * d$cost +
                                     taste))
  d
}

test_that("fs_logit reproduces glm and clusters standard errors", {
  d <- make_binary()
  fit <- fs_logit(support ~ quality + type + cost, data = d, cluster = ~ id,
                  compute_me = FALSE)
  ref <- glm(support ~ quality + type + cost, family = binomial, data = d)
  expect_s3_class(fit, "fs_logit")
  expect_equal(coef(fit), coef(ref))
  expect_equal(vcov(fit), sandwich::vcovCL(ref, cluster = d$id,
                                           type = "HC0", cadjust = TRUE))
  expect_equal(nobs(fit), nrow(d))
  expect_equal(as.numeric(logLik(fit)), as.numeric(logLik(ref)))
  expect_equal(fit$gof$pseudo_r2_mcfadden,
               1 - as.numeric(logLik(ref)) /
                 as.numeric(logLik(update(ref, . ~ 1))))
  expect_true(fit$gof$percent_correct > 50 && fit$gof$percent_correct <= 100)
})

test_that("fs_logit accepts logical and two-level factor responses", {
  d <- make_binary()
  d$yes_lgl <- d$support == 1
  d$yes_fct <- factor(ifelse(d$support == 1, "yes", "no"),
                      levels = c("no", "yes"))
  a <- fs_logit(support ~ quality, data = d, cluster = FALSE,
                compute_me = FALSE)
  b <- fs_logit(yes_lgl ~ quality, data = d, cluster = FALSE,
                compute_me = FALSE)
  f <- fs_logit(yes_fct ~ quality, data = d, cluster = FALSE,
                compute_me = FALSE)
  expect_equal(coef(a), coef(b))
  expect_equal(coef(a), coef(f))
})

test_that("fs_logit validates its input", {
  d <- make_binary()
  expect_error(fs_logit(~ quality, data = d), "two-sided")
  expect_error(fs_logit(support ~ quality, data = as.list(d)), "data frame")
  expect_error(fs_logit(nope ~ quality, data = d), "not found")
  d$three <- sample(0:2, nrow(d), replace = TRUE)
  expect_error(fs_logit(three ~ quality, data = d, cluster = FALSE),
               "must be binary")
  d$one <- 1
  expect_error(fs_logit(one ~ quality, data = d, cluster = FALSE),
               "only one value")
  expect_warning(fs_logit(support ~ quality, data = d, compute_me = FALSE),
                 "independent")
})

test_that("summary and print methods work", {
  d <- make_binary()
  fit <- fs_logit(support ~ quality + cost, data = d, cluster = ~ id,
                  compute_me = FALSE)
  s <- summary(fit)
  expect_equal(unname(s$coefficients[, "Std. Error"]),
               unname(sqrt(diag(vcov(fit)))))
  expect_output(print(fit), "clustered by id")
  expect_output(print(s), "McFadden")
  expect_output(print(s), "Correctly predicted")
})

test_that("fs_mwtp works with an fs_logit fit", {
  d <- make_binary()
  fit <- fs_logit(support ~ quality + cost, data = d, cluster = ~ id,
                  compute_me = FALSE)
  w <- as.data.frame(fs_mwtp(fit, variable = "quality", price = "cost"))
  expect_equal(w$mwtp, unname(-coef(fit)["quality"] / coef(fit)["cost"]))
})

test_that("marginal effects are computed with the clustered covariance", {
  skip_if_not_installed("margins")
  d <- make_binary()
  fit <- fs_logit(support ~ quality + cost, data = d, cluster = ~ id)
  expect_s3_class(fit$marginal_effects, "margins")
  expect_output(print(summary(fit)), "Average marginal effects")
})
