make_ewtp_data <- function(n_resp = 150, k = 4, seed = 9) {
  set.seed(seed)
  n <- n_resp * k
  d <- data.frame(
    id      = rep(seq_len(n_resp), each = k),
    quality = sample(0:1, n, replace = TRUE),
    type    = factor(sample(c("a", "b", "c"), n, replace = TRUE)),
    cost    = sample(c(500, 1000, 2000, 3000), n, replace = TRUE)
  )
  eta <- 0.5 + 0.8 * d$quality + 0.4 * (d$type == "b") - 0.001 * d$cost
  d$support <- rbinom(n, 1, plogis(eta))
  d$rating <- round(10 * plogis(eta + rnorm(n, sd = 0.4)))
  d
}

scen <- data.frame(quality = c(0, 1, 1), type = c("a", "a", "b"),
                   row.names = c("base", "q", "q+b"))

test_that("EWTP equals the closed form and the integral it stands for", {
  d <- make_ewtp_data()
  fit <- fs_logit(support ~ quality + type + cost, data = d, cluster = ~ id,
                  compute_me = FALSE)
  e <- fs_ewtp(fit, scen, price = "cost")
  b <- coef(fit)
  gam <- -b[["cost"]]
  idx <- b[["(Intercept)"]] + b[["quality"]] * c(0, 1, 1) +
    b[["typeb"]] * c(0, 0, 1)
  expect_equal(e$estimates$index, idx)
  expect_equal(e$estimates$ewtp, log1p(exp(idx)) / gam)
  expect_equal(e$estimates$scenario, c("base", "q", "q+b"))

  num <- vapply(idx, function(a) {
    stats::integrate(function(r) plogis(a - gam * r), 0, Inf)$value
  }, numeric(1L))
  expect_equal(e$estimates$ewtp, num, tolerance = 1e-5)
})

test_that("the delta-method SE matches a numerical gradient", {
  d <- make_ewtp_data()
  fit <- fs_logit(support ~ quality + type + cost, data = d, cluster = ~ id,
                  compute_me = FALSE)
  e <- fs_ewtp(fit, scen, price = "cost")
  b <- coef(fit)
  V <- vcov(fit)
  X <- cbind(1, c(0, 1, 1), c(0, 0, 1), c(0, 0, 0), 0)
  colnames(X) <- c("(Intercept)", "quality", "typeb", "typec", "cost")
  f <- function(beta) log1p(exp(drop(X %*% beta))) / -beta[["cost"]]
  h <- 1e-6
  G <- vapply(seq_along(b), function(j) {
    up <- b
    dn <- b
    up[j] <- up[j] + h
    dn[j] <- dn[j] - h
    (f(up) - f(dn)) / (2 * h)
  }, numeric(3L))
  se_num <- sqrt(rowSums((G %*% V) * G))
  expect_equal(e$estimates$std_error, se_num, tolerance = 1e-5)
  z <- qnorm(0.975)
  expect_equal(e$estimates$conf_low, e$estimates$ewtp - z * se_num,
               tolerance = 1e-5)
})

test_that("EWTP recovers the true value in a large sample", {
  skip_on_cran()
  set.seed(2)
  n <- 40000
  d <- data.frame(id = rep(1:10000, each = 4),
                  q = rbinom(n, 1, 0.5),
                  cost = sample(c(500, 1000, 2000, 3000), n, replace = TRUE))
  d$y <- rbinom(n, 1, plogis(0.5 + 0.8 * d$q - 0.001 * d$cost))
  fit <- fs_logit(y ~ q + cost, data = d, cluster = ~ id, compute_me = FALSE)
  e <- fs_ewtp(fit, data.frame(q = 0:1), price = "cost")
  truth <- log1p(exp(0.5 + 0.8 * 0:1)) / 0.001
  expect_true(all(abs(e$estimates$ewtp - truth) < 3 * e$estimates$std_error))
})

test_that("Krinsky-Robb is reproducible and leaves the RNG untouched", {
  d <- make_ewtp_data()
  fit <- fs_logit(support ~ quality + type + cost, data = d, cluster = ~ id,
                  compute_me = FALSE)
  set.seed(100)
  before <- .Random.seed
  a <- fs_ewtp(fit, scen, price = "cost", method = "krinsky-robb",
               n_sim = 2000, seed = 1)
  expect_identical(.Random.seed, before)
  b <- fs_ewtp(fit, scen, price = "cost", method = "krinsky-robb",
               n_sim = 2000, seed = 1)
  expect_equal(a$estimates, b$estimates)
  expect_equal(dim(a$draws), c(2000L, 3L))
  dl <- fs_ewtp(fit, scen, price = "cost")
  expect_equal(a$estimates$ewtp, dl$estimates$ewtp)
  expect_equal(a$estimates$std_error, dl$estimates$std_error,
               tolerance = 0.15)
  expect_true(all(a$estimates$conf_low < a$estimates$ewtp &
                    a$estimates$ewtp < a$estimates$conf_high))
})

test_that("average = TRUE returns the mean over rows", {
  d <- make_ewtp_data()
  fit <- fs_logit(support ~ quality + type + cost, data = d, cluster = ~ id,
                  compute_me = FALSE)
  rows <- fs_ewtp(fit, scen, price = "cost")
  avg <- fs_ewtp(fit, scen, price = "cost", average = TRUE)
  expect_equal(nrow(avg$estimates), 1L)
  expect_equal(avg$estimates$ewtp, mean(rows$estimates$ewtp))
  expect_match(avg$estimates$scenario, "mean of 3 rows")
  kr <- fs_ewtp(fit, scen, price = "cost", average = TRUE,
                method = "krinsky-robb", n_sim = 500, seed = 3)
  expect_equal(dim(kr$draws), c(500L, 1L))
})

test_that("fs_ewtp works with fs_betareg, glm and betareg fits", {
  d <- make_ewtp_data()
  fb <- fs_betareg(rating ~ quality + type + cost, data = d, cluster = ~ id,
                   compute_me = FALSE)
  eb <- fs_ewtp(fb, scen, price = "cost")
  b <- coef(fb)
  idx <- b[["(Intercept)"]] + b[["quality"]] * c(0, 1, 1) +
    b[["typeb"]] * c(0, 0, 1)
  expect_equal(eb$estimates$ewtp, log1p(exp(idx)) / -b[["cost"]])

  g <- glm(support ~ quality + type + cost, family = binomial, data = d)
  expect_equal(fs_ewtp(g, scen, price = "cost")$estimates$ewtp,
               fs_ewtp(fs_logit(support ~ quality + type + cost, data = d,
                                cluster = FALSE, compute_me = FALSE),
                       scen, price = "cost")$estimates$ewtp)
  expect_s3_class(fs_ewtp(fb$model, scen, price = "cost"), "fs_ewtp")
})

test_that("fs_ewtp rejects models and data it cannot handle", {
  d <- make_ewtp_data()
  fit <- fs_logit(support ~ quality + type + cost, data = d, cluster = FALSE,
                  compute_me = FALSE)
  expect_error(fs_ewtp(fit, price = "cost"), "one row per scenario")
  expect_error(fs_ewtp(fit, scen), "name of the price")
  expect_error(fs_ewtp(fit, scen, price = "nope"), "not a term")
  expect_error(fs_ewtp(fit, data.frame(quality = 1), price = "cost"),
               "lacks variable")
  expect_error(fs_ewtp(fit, scen, price = "cost", level = 2), "between 0")

  probit <- glm(support ~ quality + cost, family = binomial("probit"),
                data = d)
  expect_error(fs_ewtp(probit, data.frame(quality = 1), price = "cost"),
               "logit link")
  expect_error(fs_ewtp(lm(support ~ quality + cost, data = d),
                       data.frame(quality = 1), price = "cost"),
               "must be an fs_logit")

  d$up <- rbinom(nrow(d), 1, plogis(-2 + 0.002 * d$cost))
  pos <- fs_logit(up ~ quality + cost, data = d, cluster = FALSE,
                  compute_me = FALSE)
  expect_error(fs_ewtp(pos, data.frame(quality = 1), price = "cost"),
               "must be negative")
})

test_that("print and as.data.frame methods work", {
  d <- make_ewtp_data()
  fit <- fs_logit(support ~ quality + type + cost, data = d, cluster = ~ id,
                  compute_me = FALSE)
  e <- fs_ewtp(fit, scen, price = "cost")
  expect_output(print(e), "Expected willingness to pay")
  expect_named(as.data.frame(e), c("scenario", "index", "ewtp", "std_error",
                                   "conf_low", "conf_high"))
})
