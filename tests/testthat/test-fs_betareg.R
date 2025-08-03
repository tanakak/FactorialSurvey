# tests/testthat/test-fs_betareg.R

library(testthat)
# library(FactorialSurvey) # パッケージとして読み込む場合

# テスト用のサンプルデータを事前に作成
set.seed(42)
test_data <- data.frame(
  satisfaction = sample(0:10, 100, replace = TRUE),
  age = rnorm(100, 40, 10),
  gender = factor(sample(c("Male", "Female"), 100, replace = TRUE))
)

# ---------------------------------------------------------------------------

test_that("fs_betareg runs with default settings and returns correct structure", {
  # `betareg`からの警告を抑制しつつ実行
  results <- suppressWarnings(
    fs_betareg(
      satisfaction ~ age + gender,
      data = test_data,
      verbose = FALSE
    )
  )

  expect_type(results, "list")
  expect_named(results, c("model", "marginal_effects", "marginal_effects_rescaled"))
  expect_s3_class(results$model, "betareg")
  expect_s3_class(results$marginal_effects, "margins")
  expect_s3_class(results$marginal_effects_rescaled, "data.frame")
})

test_that("Side effect test: Original data is not modified", {
  original_data <- test_data

  suppressWarnings(
    fs_betareg(
      satisfaction ~ age + gender,
      data = test_data,
      verbose = FALSE
    )
  )

  expect_identical(test_data, original_data,
                   info = "The function should not modify the input 'data' frame."
  )
})

# ---------------------------------------------------------------------------

test_that("transform = FALSE works correctly", {
  transformed_data <- test_data
  y <- transformed_data$satisfaction
  N <- length(y)
  transformed_data$satisfaction <- ((y / 10) * (N - 1) + 0.5) / N

  # `betareg`からの警告を抑制しつつ、エラーが出ないことを確認
  expect_no_error(
    suppressWarnings(
      fs_betareg(
        satisfaction ~ age + gender,
        data = transformed_data,
        transform = FALSE,
        verbose = FALSE
      )
    )
  )

  # 関数がラップした後のエラーメッセージを期待するように修正
  expect_error(
    fs_betareg(
      satisfaction ~ age + gender,
      data = test_data,
      transform = FALSE,
      verbose = FALSE
    ),
    "Failed to fit the beta regression model. Error:"
  )
})

test_that("compute_me = FALSE works correctly", {
  results <- suppressWarnings(
    fs_betareg(
      satisfaction ~ age + gender,
      data = test_data,
      compute_me = FALSE,
      verbose = FALSE
    )
  )

  expect_null(results$marginal_effects)
  expect_null(results$marginal_effects_rescaled)
})

test_that("verbose = FALSE silences console output", {
  # `betareg`からの警告を抑制することで、他の予期せぬ出力がないことを確認
  expect_silent(
    suppressWarnings(
      fs_betareg(
        satisfaction ~ age + gender,
        data = test_data,
        verbose = FALSE
      )
    )
  )
})

# ---------------------------------------------------------------------------

test_that("Function handles invalid dependent variables", {
  data_with_na <- test_data
  data_with_na$satisfaction[1] <- NA
  expect_error(
    fs_betareg(satisfaction ~ age, data = data_with_na),
    "The dependent variable contains missing values."
  )

  data_with_char <- test_data
  data_with_char$satisfaction <- as.character(data_with_char$satisfaction)
  expect_error(
    fs_betareg(satisfaction ~ age, data = data_with_char),
    "The dependent variable must be numeric."
  )
})
