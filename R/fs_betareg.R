#' @title Perform Beta Regression for Factorial Surveys
#' @description This function is a wrapper for `betareg::betareg` tailored for
#' factorial survey data, often on a 0-10 point scale. It includes an
#' option to automatically transform the response variable to the (0, 1) interval.
#'
#' @param formula A symbolic description of the model to be fitted.
#' @param data A data frame containing the variables in the model.
#' @param transform A logical value. If TRUE (default), the function assumes the
#'   response variable is on a 0-10 scale and transforms it to the (0, 1) interval
#'   required for beta regression. If FALSE, it assumes the data is already in the
#'   correct format.
#' @param compute_me A logical value. If TRUE (default), computes and returns
#'   marginal effects using the `margins` package.
#' @param verbose A logical value. If TRUE (default), prints the model summary
#'   and fit statistics to the console.
#'
#' @return A list containing the fitted model object (`model`) and the computed
#'   marginal effects (`marginal_effects`), if requested.
#'
#' @importFrom stats AIC BIC logLik pmin pmax
#' @examples
#' \dontrun{
#'   set.seed(123)
#'   n <- 100
#'   vignette_data <- data.frame(
#'     score = rbeta(n, 2, 5) * 10,
#'     x1 = rnorm(n),
#'     x2 = factor(sample(0:1, n, replace = TRUE))
#'   )
#'
#'   results <- fs_betareg(score ~ x1 + x2, data = vignette_data)
#'   summary(results$model)
#'   summary(results$marginal_effects)
#' }
#' @export
fs_betareg <- function(
    formula,
    data,
    transform = TRUE,  # 0-10スコア変換の有無を選択
    compute_me = TRUE,
    verbose = TRUE
) {
  # 必要なパッケージがなければエラーを出す
  if (!requireNamespace("betareg", quietly = TRUE)) {
    stop("Package 'betareg' is required. Please install it using: install.packages('betareg')")
  }

  # formulaから従属変数の名前を取得
  y_var <- all.vars(formula)[1]

  # --- Transformation Block ---
  # transform = TRUE の場合、0-10スコアを(0,1)に変換
  if (transform) {
    y <- data[[y_var]]

    # Step 1: 0-10点のスケールを0-1のスケールに正規化
    # 注意: 元のスケールが0-10でない場合は、この除数を変更してください
    y_scaled <- y / 10.0

    # Step 2: 0と1の値を回避するための変換 (Smithson & Verkuilen, 2006)
    N <- length(y_scaled)
    y_beta <- (y_scaled * (N - 1) + 0.5) / N

    # 安全ガード: 変換後の値が厳密に0や1になることを防ぐ
    y_beta <- pmin(pmax(y_beta, .Machine$double.eps), 1 - .Machine$double.eps)

    # 変換した値で元のデータフレームの列を上書き
    data[[y_var]] <- y_beta
  }

  # --- Model Estimation ---
  # ベータ回帰モデルを推定
  model <- tryCatch({
    betareg::betareg(formula, data = data)
  }, error = function(e) {
    stop("Failed to fit the beta regression model. Check if the response variable is in the (0, 1) interval. Error: ", e$message)
  })


  # --- Verbose Output ---
  # verbose = TRUE の場合、結果をコンソールに出力
  if (verbose) {
    cat("===== Beta Regression Model Summary =====\n")
    print(summary(model))

    cat("\n===== Fit Statistics =====\n")
    cat("AIC:", AIC(model), "\n")
    cat("BIC:", BIC(model), "\n")
    cat("Log-Likelihood (LL):", as.numeric(logLik(model)), "\n")
    cat("Log-Likelihood (Null Model, LL(0)):", model$loglik.null, "\n")
  }

  # --- Marginal Effects ---
  # compute_me = TRUE の場合、限界効果を計算
  me <- NULL
  if (compute_me) {
    if (!requireNamespace("margins", quietly = TRUE)) {
      warning("Package 'margins' is required for marginal effects. Please install it. Skipping marginal effects computation.")
    } else {
      me <- tryCatch({
        margins::margins(model)
      }, error = function(e) {
        warning("Failed to compute marginal effects: ", e$message)
        NULL
      })

      if (verbose && !is.null(me)) {
        cat("\n===== Average Marginal Effects =====\n")
        print(summary(me))
      }
    }
  }

  # --- Return Value ---
  # 結果をリストとして返す
  invisible(list(
    model = model,
    marginal_effects = me
  ))
}
