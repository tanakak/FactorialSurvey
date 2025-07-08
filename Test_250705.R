







# 必要なパッケージを読み込み
# devtools::load_all() を使うか、上記 fs_betareg 関数を直接実行してください
# source("path/to/fs_betareg.R")

# ---- サンプルデータ（安定版：ベータ分布ベース、0-10スコア） ----
set.seed(123)
n <- 100
x1 <- rnorm(n)
x2 <- sample(0:1, n, replace = TRUE)
score <- rbeta(n, 2, 5) * 10  # ベータ分布から生成（0-10スコア）

test_data <- data.frame(score, x1, x2)

# ---- モデル式 ----
formula_test <- score ~ x1 + x2

# ---- Test 1: transform = TRUE（自動変換あり） ----
# 修正された fs_betareg 関数により、このテストは正常に実行されます
cat("### Test 1: transform = TRUE（自動変換あり） ###\n")
result1 <- fs_betareg(
  formula = formula_test,
  data = test_data,
  transform = TRUE,
  compute_me = TRUE,
  verbose = TRUE
)

# ---- Test 2: transform = FALSE（事前変換済みデータで変換なし） ----
# 自前で (0,1) に変換したデータを作成
test_data2 <- test_data
y <- test_data2$score
N <- length(y)

# *** 修正箇所 ***
# Step 1: 0-10点を0-1スケールに正規化
y_scaled <- y / 10.0
# Step 2: (0,1)開区間への変換
y_beta <- (y_scaled * (N - 1) + 0.5) / N
# ****************

y_beta <- pmin(pmax(y_beta, .Machine$double.eps), 1 - .Machine$double.eps)
test_data2$score_beta <- y_beta  # 変換後のデータを新しい列として追加

# formula も必ず「変換後の列名」に合わせて変更
formula_test2 <- score_beta ~ x1 + x2

cat("\n### Test 2: transform = FALSE（変換なし、自前変換済みデータ） ###\n")
result2 <- fs_betareg(
  formula = formula_test2,
  data = test_data2,
  transform = FALSE,  # 変換済みデータなので、transform = FALSE にする
  compute_me = TRUE,
  verbose = TRUE
)
