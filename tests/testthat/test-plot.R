plot_fit <- function(compute_me = FALSE) {
  set.seed(1)
  n <- 240
  d <- data.frame(id = rep(1:60, each = 4),
                  quality = rbinom(n, 1, 0.5),
                  speed = rbinom(n, 1, 0.5),
                  price = sample(c(100, 300, 500), n, replace = TRUE))
  d$rating <- round(10 * plogis(0.8 * d$quality + 0.4 * d$speed -
                                  0.004 * d$price + rnorm(n, sd = 0.5)))
  d$yes <- as.integer(d$rating >= 5)
  list(d = d,
       fit = fs_betareg(rating ~ quality + speed + price, data = d,
                        cluster = ~ id, compute_me = compute_me))
}

test_that("plot.fs_mwtp draws the estimates in order", {
  w <- fs_mwtp(plot_fit()$fit, price = "price")
  p <- plot(w)
  expect_s3_class(p, "ggplot")
  b <- ggplot2::ggplot_build(p)
  pts <- b$data[[3]]
  expect_equal(sort(pts$x), sort(w$estimates$mwtp))
  # first estimate is drawn at the top
  expect_equal(levels(p$data$label), rev(w$estimates$variable))
  seg <- b$data[[2]]
  expect_equal(sort(seg$x), sort(w$estimates$conf_low))
  expect_equal(sort(seg$xend), sort(w$estimates$conf_high))
})

test_that("plot.fs_ewtp labels scenarios", {
  e <- fs_ewtp(plot_fit()$fit,
               data.frame(quality = c(0, 1), speed = c(0, 1),
                          row.names = c("none", "both")),
               price = "price")
  p <- plot(e)
  expect_s3_class(p, "ggplot")
  expect_setequal(as.character(p$data$label), c("none", "both"))
  expect_match(p$labels$x, "Expected willingness to pay")
})

test_that("model plots fall back to coefficients without marginal effects", {
  fit <- plot_fit()$fit
  p <- plot(fit)
  expect_match(p$labels$x, "Coefficient")
  expect_false("(Intercept)" %in% as.character(p$data$label))
  expect_setequal(as.character(p$data$label), c("quality", "speed", "price"))
  expect_error(plot(fit, type = "ame"), "were not computed")
  expect_error(plot(fit, type = "coef", level = 1.2), "between 0 and 1")
})

test_that("coefficient intervals use the clustered covariance", {
  fit <- plot_fit()$fit
  p <- plot(fit, type = "coef", level = 0.9)
  se <- sqrt(diag(vcov(fit)))["quality"]
  row <- p$data[p$data$label == "quality", ]
  expect_equal(unname(row$upper - row$estimate),
               unname(qnorm(0.95) * se))
})

test_that("marginal-effect plots work when margins is available", {
  skip_if_not_installed("margins")
  pf <- plot_fit(compute_me = TRUE)
  p <- plot(pf$fit)
  expect_match(p$labels$x, "Average marginal effect \\(rating scale 0 to 10\\)")
  lg <- fs_logit(yes ~ quality + speed + price, data = pf$d, cluster = ~ id)
  expect_match(plot(lg)$labels$x, "probability")
  expect_match(plot(lg, type = "coef")$labels$x, "logit")
})

test_that("theme_fs returns a theme", {
  expect_s3_class(theme_fs(), "theme")
})
