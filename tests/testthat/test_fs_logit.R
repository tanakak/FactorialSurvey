test_that("fs_logit runs without error", {
  data <- data.frame(
    y = sample(0:1, 100, replace = TRUE),
    x1 = rnorm(100),
    x2 = rnorm(100),
    price = runif(100, 1, 10),
    respondent_id = rep(1:20, each = 5)  # Panel structure
  )

  model <- fs_logit(y ~ x1 + x2 + price, data = data, panel_id = "respondent_id")

  expect_s3_class(model, "coeftest")  # `coeftest` クラスを持っているかチェック
})

