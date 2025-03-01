#' Standard Logit Regression
#'
#' @description Estimates a binary logit model with clustered standard errors.
#' @param formula A formula specifying the model (e.g., y ~ x1 + x2).
#' @param data A data frame containing the variables.
#' @param panel_id A string specifying the panel (individual) ID variable.
#' @return A model object with robust standard errors.
#' @export
fs_logit <- function(formula, data, panel_id) {
  if (!requireNamespace("sandwich", quietly = TRUE) || !requireNamespace("lmtest", quietly = TRUE)) {
    stop("Please install 'sandwich' and 'lmtest' packages.")
  }
  
  model <- glm(formula, data = data, family = binomial(link = "logit"))
  cluster_se <- sandwich::vcovCL(model, cluster = data[[panel_id]])
  
  return(lmtest::coeftest(model, vcov = cluster_se))
}
