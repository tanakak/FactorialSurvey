test_that("fs_logit runs without error", {
  set.seed(123)
  test_data <- data.frame(
    y = sample(0:1, 100, replace = TRUE),
    x1 = rnorm(100),
    x2 = rnorm(100),
    price = runif(100, 1, 10),
    respondent_id = rep(1:20, each = 5)
  )
  
  model <- fs_logit(y ~ x1 + x2 + price, data = test_data, panel_id = "respondent_id")
  
  # `model` がリストであることを確認
  expect_true(is.list(model))
  
  # `model$model` が glm オブジェクトであることを確認
  expect_s3_class(model$model, "glm")
  
  # `model$coeftest` が存在することを確認
  expect_true("coeftest" %in% names(model))
})
