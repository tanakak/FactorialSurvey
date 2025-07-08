#' @title Marginal Willingness to Pay (MWTP) Estimation
#' @description Calculates the Marginal Willingness to Pay (MWTP) from a regression
#'   model result, typically one from `fs_betareg`. It computes a point estimate,
#'   standard error, and confidence interval using the Delta Method.
#'
#' @param model_results A list object containing a fitted model. This is expected
#'   to be the output from the `fs_betareg` function, where `model_results$model`
#'   is the actual model object.
#' @param variable_name A string specifying the name of the attribute variable for
#'   which to calculate the MWTP. This name must exactly match a coefficient name
#'   in the model summary.
#' @param price_variable_name A string specifying the name of the price variable.
#'   This name must also exactly match a coefficient name.
#'
#' @return An invisible list containing the MWTP point estimate, standard error,
#'   and 95% confidence interval. The function also prints a formatted summary of
#'   these results to the console.
#'
#' @details The function calculates MWTP using the formula:
#'   MWTP = - (coefficient_variable / coefficient_price).
#'   The standard error is calculated via the Delta Method, which requires the
#'   `msm` package.
#'
#' @importFrom msm deltamethod
#' @importFrom stats coef vcov as.formula
#' @export
#' @examples
#' \dontrun{
#' # 1. First, run the beta regression analysis using fs_betareg
#' set.seed(123)
#' n <- 200
#' survey_data <- data.frame(
#'   rating_score = rbeta(n, 3, 4) * 10,
#'   design_A = sample(0:1, n, replace = TRUE), # Feature: Design A
#'   performance = rnorm(n, 100, 15),          # Feature: Performance metric
#'   price = rnorm(n, 500, 50)                 # Price in USD
#' )
#'
#' model_fit <- fs_betareg(rating_score ~ design_A + performance + price,
#'                         data = survey_data,
#'                         verbose = FALSE) # Suppress summary print for cleaner example
#'
#' # 2. Now, calculate MWTP for the 'design_A' feature
#' mwtp_design_a <- fs_mwtp(
#'   model_results = model_fit,
#'   variable_name = "design_A",
#'   price_variable_name = "price"
#' )
#'
#' # 3. Calculate MWTP for the 'performance' feature
#' mwtp_performance <- fs_mwtp(
#'   model_results = model_fit,
#'   variable_name = "performance",
#'   price_variable_name = "price"
#' )
#' }
fs_mwtp <- function(model_results, variable_name, price_variable_name) {

  # --- Input Validation ---

  # Check if the required 'msm' package is installed for Delta Method
  if (!requireNamespace("msm", quietly = TRUE)) {
    stop("Package 'msm' is required for confidence intervals. Please install it using: install.packages('msm')")
  }

  # Check if the input object is valid
  if (!"model" %in% names(model_results) || !inherits(model_results$model, "betareg")) {
    stop("The 'model_results' object must be a list containing a 'betareg' model object named 'model'. Please use the output from fs_betareg().")
  }
  
  model <- model_results$model
  coefs <- coef(model)
  all_coef_names <- names(coefs)

  # Check if the specified variable names exist in the model coefficients
  if (!(variable_name %in% all_coef_names)) {
    stop(paste0("Variable '", variable_name, "' not found in model coefficients. Available coefficients are: ", paste(all_coef_names, collapse = ", ")))
  }
  if (!(price_variable_name %in% all_coef_names)) {
    stop(paste0("Price variable '", price_variable_name, "' not found in model coefficients. Available coefficients are: ", paste(all_coef_names, collapse = ", ")))
  }

  # --- MWTP Calculation ---

  # Extract coefficients
  coef_var <- coefs[variable_name]
  coef_price <- coefs[price_variable_name]

  # Calculate the MWTP point estimate
  mwtp_estimate <- -coef_var / coef_price

  # --- Confidence Interval using Delta Method ---

  # Get the variance-covariance matrix
  vcov_matrix <- vcov(model)
  
  # Find the position (index) of the coefficients in the vector
  idx_var <- which(all_coef_names == variable_name)
  idx_price <- which(all_coef_names == price_variable_name)

  # Create the formula string for the deltamethod function, e.g., "~ -x2 / x4"
  # The variables `x1`, `x2`, etc., refer to the position in the coefficient vector
  formula_str <- paste0("~ -x", idx_var, " / x", idx_price)

  # Calculate the standard error of the MWTP estimate
  se_mwtp <- msm::deltamethod(
    g = as.formula(formula_str),
    mean = coefs,
    cov = vcov_matrix
  )

  # Calculate the 95% confidence interval
  ci_lower <- mwtp_estimate - 1.96 * se_mwtp
  ci_upper <- mwtp_estimate + 1.96 * se_mwtp

  # --- Output ---
  
  cat("===== Marginal Willingness to Pay (MWTP) Estimation =====\n")
  cat("Attribute Variable:", variable_name, "\n")
  cat("Price Variable    :", price_variable_name, "\n")
  cat("---------------------------------------------------------\n")
  cat("MWTP Point Estimate :", format(mwtp_estimate, digits = 4), "\n")
  cat("Standard Error (SE) :", format(se_mwtp, digits = 4), "(Delta Method)\n")
  cat("95% Conf. Interval  :", paste0("[", format(ci_lower, digits = 4), ", ", format(ci_upper, digits = 4), "]"), "\n")
  cat("=========================================================\n")


  # Return the results invisibly as a list
  invisible(list(
    variable = variable_name,
    price_variable = price_variable_name,
    mwtp_estimate = mwtp_estimate,
    std_error = se_mwtp,
    ci_95_lower = ci_lower,
    ci_95_upper = ci_upper
  ))
}