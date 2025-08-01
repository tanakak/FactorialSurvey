#' @title Perform Beta Regression for Factorial Surveys
#' @description This function is a wrapper for `betareg::betareg` tailored for
#' factorial survey data, often on a 0-10 point scale. It includes an
#' option to automatically transform the response variable to the (0, 1) interval.
#'
#' @importFrom stats AIC BIC logLik
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
#' @examples
#' \dontrun{
#'   # Ensure required packages are installed
#'   # install.packages(c("betareg", "margins"))
#'
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
    transform = TRUE,
    compute_me = TRUE,
    verbose = TRUE
) {
  # Check for required packages
  if (!requireNamespace("betareg", quietly = TRUE)) {
    stop("Package 'betareg' is required. Please install it using: install.packages('betareg')")
  }

  # Get the name of the dependent variable from the formula
  y_var <- all.vars(formula)[1]

  # --- Transformation Block ---
  # If transform = TRUE, convert the 0-10 score to the (0, 1) interval
  if (transform) {
    y <- data[[y_var]]

    # Step 1: Normalize the 0-10 scale to a 0-1 scale
    y_scaled <- y / 10.0

    # Step 2: Transformation to avoid 0 and 1 values (Smithson & Verkuilen, 2006)
    N <- length(y_scaled)
    y_beta <- (y_scaled * (N - 1) + 0.5) / N

    # Safeguard: Prevent transformed values from being exactly 0 or 1
    y_beta <- pmin(pmax(y_beta, .Machine$double.eps), 1 - .Machine$double.eps)

    # Overwrite the column in the original data frame with the transformed values
    data[[y_var]] <- y_beta
  }

  # --- Model Estimation ---
  # Fit the beta regression model
  model <- tryCatch({
    betareg::betareg(formula, data = data)
  }, error = function(e) {
    stop("Failed to fit the beta regression model. Check if the response variable is in the (0, 1) interval. Error: ", e$message)
  })


  # --- Verbose Output ---
  # If verbose = TRUE, print the results to the console
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
  # If compute_me = TRUE, compute marginal effects
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
  # Return the results as a list
  invisible(list(
    model = model,
    marginal_effects = me
  ))
}