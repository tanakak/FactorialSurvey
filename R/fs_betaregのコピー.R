#' @title Perform Beta Regression for Factorial Surveys
#' @description This function is a wrapper for `betareg::betareg` tailored for
#' factorial survey data, often on a 0-10 point scale. It includes an
#' option to automatically transform the response variable to the (0, 1) interval,
#' and returns marginal effects both on the 0-1 scale and the original 0-10 scale.
#'
#' @importFrom stats AIC BIC logLik
#'
#' @param formula A symbolic description of the model to be fitted.
#' @param data A data frame containing the variables in the model.
#' @param transform A logical value. If TRUE (default), the function assumes the
#'  response variable is on a 0-10 scale and transforms it to the (0, 1) interval
#'  required for beta regression. If FALSE, it assumes the data is already in the
#'  correct format.
#' @param compute_me A logical value. If TRUE (default), computes and returns
#'  marginal effects using the `margins` package.
#' @param verbose A logical value. If TRUE (default), prints the model summary
#'  and fit statistics to the console.
#'
#' @return A list containing the fitted model object (`model`), the marginal effects
#'  on the (0, 1) scale (`marginal_effects`), and the marginal effects rescaled to
#'  the original (0–10) scale (`marginal_effects_rescaled`).
#'
#' @export
fs_betareg <- function(
    formula,
    data,
    transform = TRUE,
    compute_me = TRUE,
    verbose = TRUE
) {
  if (!requireNamespace("betareg", quietly = TRUE)) {
    stop("Package 'betareg' is required. Please install it using: install.packages('betareg')")
  }

  # --- Data Preparation ---
  # Create a copy to avoid modifying the original data frame (eliminates side effects)
  data_internal <- data

  y_var <- all.vars(formula)[1]
  y <- data_internal[[y_var]]

  if (!is.numeric(y)) stop("The dependent variable must be numeric.")
  if (any(is.na(y))) stop("The dependent variable contains missing values.")

  # --- Transformation ---
  # Transform the response if needed, operating on the internal data copy
  if (transform) {
    y_scaled <- y / 10
    N <- length(y_scaled)
    y_beta <- (y_scaled * (N - 1) + 0.5) / N
    y_beta <- pmin(pmax(y_beta, .Machine$double.eps), 1 - .Machine$double.eps)
    data_internal[[y_var]] <- y_beta
  }

  # --- Model Estimation ---
  # Use the internal (potentially transformed) data for model fitting
  model <- tryCatch({
    betareg::betareg(formula, data = data_internal)
  }, error = function(e) {
    stop("Failed to fit the beta regression model. Error: ", e$message)
  })

  # --- Verbose Output ---
  if (verbose) {
    cat("===== Beta Regression Model Summary =====\n")
    print(summary(model))

    cat("\n===== Fit Statistics =====\n")
    cat("AIC:", AIC(model), "\n")
    cat("BIC:", BIC(model), "\n")
    cat("Log-Likelihood (LL):", as.numeric(logLik(model)), "\n")
    cat("Log-Likelihood (Null Model, LL(0)):", model$loglik.null, "\n")

    pseudo_r2 <- 1 - (as.numeric(logLik(model)) / model$loglik.null)
    cat("Pseudo-R² (McFadden):", round(pseudo_r2, 4), "\n")
  }

  # --- Marginal Effects ---
  me <- NULL
  me_rescaled <- NULL
  if (compute_me) {
    if (!requireNamespace("margins", quietly = TRUE)) {
      warning("Package 'margins' is required for marginal effects. Skipping marginal effects computation.")
    } else {
      me <- tryCatch({
        margins::margins(model)
      }, error = function(e) {
        warning("Failed to compute marginal effects: ", e$message)
        NULL
      })

      if (!is.null(me)) {
        if (nrow(summary(me)) > 0) {
          # Use the row count from the data that was used for the model
          N <- nrow(data_internal)
          rescale_factor <- (10 * N) / (N - 1)

          # Create a summary data frame for rescaled effects to avoid modifying the 'margins' object
          me_summary <- summary(me)
          me_rescaled_summary <- me_summary

          # Rescale the relevant columns
          cols_to_rescale <- c("AME", "SE", "lower", "upper")
          for (col in cols_to_rescale) {
            if (col %in% names(me_rescaled_summary)) {
              me_rescaled_summary[[col]] <- me_rescaled_summary[[col]] * rescale_factor
            }
          }

          if (verbose) {
            cat("\n===== Average Marginal Effects (0–1 Scale) =====\n")
            print(summary(me))
            cat("\n===== Average Marginal Effects (0–10 Scale) =====\n")
            print(me_rescaled_summary)
          }
          # Note: The returned 'me_rescaled' is now a summary data frame, not a 'margins' object
          me_rescaled <- me_rescaled_summary
        } else {
          warning("Marginal effects were computed but resulted in an empty summary. Rescaling skipped.")
        }
      }
    }
  }

  invisible(list(
    model = model,
    marginal_effects = me,
    marginal_effects_rescaled = me_rescaled
  ))
}
