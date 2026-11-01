#' @title Logit model for binary responses to vignettes
#'
#' @description
#' Fits a binary logit model to yes/no responses collected in a factorial
#' survey -- for example, whether a respondent would support the policy
#' described in a vignette at the stated cost. Together with [fs_betareg()]
#' for ratings, it covers the two response formats of the framework of
#' Tanaka (2025); [fs_mwtp()] and [fs_ewtp()] accept either fit.
#'
#' @param formula A model formula with a binary response on the left: a
#'   numeric 0/1 variable, a logical, or a factor with two levels (the second
#'   level is the "yes" outcome, as in [stats::glm()]).
#' @param data A data frame containing the variables in `formula`.
#' @param cluster How to account for repeated responses from the same
#'   respondent, exactly as in [fs_betareg()]: a one-sided formula such as
#'   `~ id`, a column name, a vector with one element per row, or `FALSE`.
#'   The default `NULL` treats responses as independent and warns.
#' @param compute_me A logical value. If `TRUE` (the default), average
#'   marginal effects on the probability scale are computed with the
#'   \pkg{margins} package when it is installed.
#' @param verbose A logical value. If `TRUE`, progress is reported through
#'   [message()].
#' @param ... Further arguments passed to [stats::glm()], such as `weights` or
#'   `subset`.
#'
#' @return An object of class `fs_logit`, a list with components:
#'   \describe{
#'     \item{`model`}{The fitted [stats::glm()] object.}
#'     \item{`vcov`}{The covariance matrix used for inference: cluster-robust
#'       when `cluster` is given, otherwise model-based.}
#'     \item{`cluster`}{A list describing the clustering, or `NULL`.}
#'     \item{`marginal_effects`}{The `margins` object, or `NULL`.}
#'     \item{`gof`}{A named list of fit measures: `logLik`, `logLik_null`,
#'       `AIC`, `BIC`, `lr_stat`, `lr_df`, `lr_p_value`,
#'       `pseudo_r2_mcfadden`, `percent_correct` and `nobs`.}
#'     \item{`call`}{The matched call.}
#'   }
#'
#' @details
#' For a binary outcome McFadden's pseudo R-squared, \eqn{1 - \ell/\ell_0}, is
#' well defined and is reported, together with the share of responses
#' predicted correctly at a 0.5 threshold. Cluster-robust standard errors are
#' computed as in [fs_betareg()].
#'
#' @references
#' Tanaka, K. (2025). A unified framework for estimating willingness to pay
#' using factorial survey experiments. SSRN Working Paper.
#' \doi{10.2139/ssrn.5238343}
#'
#' @seealso [fs_ewtp()] and [fs_mwtp()] for willingness to pay,
#'   [fs_betareg()] for rating responses.
#'
#' @importFrom stats glm binomial pnorm
#' @export
#' @examples
#' set.seed(42)
#' n_resp <- 80
#' d <- data.frame(
#'   id      = rep(seq_len(n_resp), each = 4),
#'   quality = sample(0:1, 4 * n_resp, replace = TRUE),
#'   cost    = sample(c(500, 1000, 2000), 4 * n_resp, replace = TRUE)
#' )
#' taste <- rnorm(n_resp, sd = 0.5)[d$id]
#' d$support <- rbinom(nrow(d), 1,
#'                     plogis(1 + 0.8 * d$quality - 0.001 * d$cost + taste))
#'
#' fit <- fs_logit(support ~ quality + cost, data = d, cluster = ~ id,
#'                 compute_me = FALSE)
#' summary(fit)
#' fs_mwtp(fit, price = "cost")
fs_logit <- function(formula,
                     data,
                     cluster = NULL,
                     compute_me = TRUE,
                     verbose = FALSE,
                     ...) {
  cl <- match.call()

  if (!inherits(formula, "formula") || length(formula) != 3L) {
    stop("`formula` must be a two-sided formula.", call. = FALSE)
  }
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  check_flag(compute_me)
  check_flag(verbose)

  y_var <- all.vars(formula)[1L]
  if (!y_var %in% names(data)) {
    stop("Response variable '", y_var, "' not found in `data`.", call. = FALSE)
  }
  y <- data[[y_var]]
  ok <- if (is.logical(y)) {
    TRUE
  } else if (is.factor(y)) {
    nlevels(y) == 2L
  } else if (is.numeric(y)) {
    all(y[!is.na(y)] %in% c(0, 1))
  } else {
    FALSE
  }
  if (!ok) {
    stop("The response must be binary: 0/1, logical, or a factor with two ",
         "levels.", call. = FALSE)
  }
  if (length(unique(y[!is.na(y)])) < 2L) {
    stop("The response takes only one value.", call. = FALSE)
  }

  cl_info <- resolve_cluster(cluster, data)

  say(verbose, "Fitting the logit model ...")
  model <- stats::glm(formula, family = stats::binomial(link = "logit"),
                      data = data, ...)
  if (!model$converged) {
    warning("The logit model did not converge.", call. = FALSE)
  }

  vc <- stats::vcov(model)
  cluster_out <- NULL
  if (!is.null(cl_info$values)) {
    used <- match(rownames(model$model), rownames(data))
    cl_used <- cl_info$values[used]
    vc <- sandwich::vcovCL(model, cluster = cl_used, type = "HC0",
                           cadjust = TRUE)
    cluster_out <- list(name = cl_info$name,
                        n_clusters = length(unique(cl_used)))
  }

  me <- NULL
  if (compute_me) {
    if (!requireNamespace("margins", quietly = TRUE)) {
      warning("Package 'margins' is not installed; skipping marginal ",
              "effects. Install it with install.packages('margins').",
              call. = FALSE)
    } else {
      me <- tryCatch(
        margins::margins(model, vcov = vc),
        error = function(e) {
          warning("Marginal effects could not be computed: ",
                  conditionMessage(e), call. = FALSE)
          NULL
        }
      )
    }
  }

  out <- list(
    model            = model,
    vcov             = vc,
    cluster          = cluster_out,
    marginal_effects = me,
    gof              = logit_gof(model),
    call             = cl
  )
  class(out) <- "fs_logit"
  out
}


#' Fit measures for a binary logit model
#' @noRd
logit_gof <- function(model) {
  ll <- as.numeric(stats::logLik(model))
  ll0 <- -model$null.deviance / 2
  df <- model$df.null - model$df.residual
  lr <- 2 * (ll - ll0)
  y <- model$y
  pc <- mean((stats::fitted(model) >= 0.5) == (y >= 0.5))
  list(
    logLik             = ll,
    logLik_null        = ll0,
    AIC                = stats::AIC(model),
    BIC                = stats::BIC(model),
    lr_stat            = lr,
    lr_df              = df,
    lr_p_value         = if (df > 0) {
      stats::pchisq(lr, df, lower.tail = FALSE)
    } else {
      NA_real_
    },
    pseudo_r2_mcfadden = 1 - ll / ll0,
    percent_correct    = 100 * pc,
    nobs               = stats::nobs(model)
  )
}

#' Coefficient table from estimates and a covariance matrix
#' @noRd
coef_table <- function(beta, vc) {
  se <- sqrt(diag(vc))[names(beta)]
  z <- beta / se
  cbind(Estimate = beta, `Std. Error` = se, `z value` = z,
        `Pr(>|z|)` = 2 * stats::pnorm(-abs(z)))
}


#' @title Print a fitted factorial survey logit model
#'
#' @param x An object of class `fs_logit`.
#' @param digits Number of significant digits.
#' @param ... Ignored.
#'
#' @return `x`, invisibly. Called for the side effect of printing.
#' @export
#' @examples
#' set.seed(1)
#' d <- data.frame(id = rep(1:30, each = 4), x = rnorm(120))
#' d$y <- rbinom(120, 1, plogis(0.8 * d$x))
#' print(fs_logit(y ~ x, data = d, cluster = ~ id, compute_me = FALSE))
print.fs_logit <- function(x, digits = 4, ...) {
  cat("Factorial survey logit model\n")
  cat("Call: ", deparse(x$call), "\n", sep = "")
  cat("Observations :", x$gof$nobs, "\n")
  cat(se_label(x$cluster), "\n\n", sep = "")
  cat("Coefficients:\n")
  print(round(stats::coef(x$model), digits))
  cat("\nUse summary() for standard errors and fit statistics.\n")
  invisible(x)
}


#' @title Summarise a fitted factorial survey logit model
#'
#' @param object An object of class `fs_logit`.
#' @param ... Ignored.
#'
#' @return An object of class `summary.fs_logit`, a list holding the
#'   coefficient table (with cluster-robust standard errors when the model was
#'   fitted with `cluster`), the fit measures and the marginal effects.
#' @export
#' @examples
#' set.seed(1)
#' d <- data.frame(id = rep(1:30, each = 4), x = rnorm(120))
#' d$y <- rbinom(120, 1, plogis(0.8 * d$x))
#' summary(fs_logit(y ~ x, data = d, cluster = ~ id, compute_me = FALSE))
summary.fs_logit <- function(object, ...) {
  out <- list(
    coefficients     = coef_table(stats::coef(object$model), object$vcov),
    gof              = object$gof,
    cluster          = object$cluster,
    marginal_effects = object$marginal_effects,
    call             = object$call
  )
  class(out) <- "summary.fs_logit"
  out
}


#' @title Print the summary of a factorial survey logit model
#'
#' @param x An object of class `summary.fs_logit`.
#' @param digits Number of significant digits.
#' @param ... Ignored.
#'
#' @return `x`, invisibly. Called for the side effect of printing.
#' @export
#' @examples
#' set.seed(1)
#' d <- data.frame(id = rep(1:30, each = 4), x = rnorm(120))
#' d$y <- rbinom(120, 1, plogis(0.8 * d$x))
#' print(summary(fs_logit(y ~ x, data = d, cluster = ~ id,
#'                        compute_me = FALSE)))
print.summary.fs_logit <- function(x, digits = 4, ...) {
  cat("Factorial survey logit model\n")
  cat("Call: ", deparse(x$call), "\n", sep = "")
  cat(se_label(x$cluster), "\n\n", sep = "")
  stats::printCoefmat(x$coefficients, digits = digits)

  g <- x$gof
  cat("\nFit statistics\n")
  cat("  Log-likelihood            :", fmt(g$logLik, digits), "\n")
  cat("  Log-likelihood (intercept):", fmt(g$logLik_null, digits), "\n")
  cat("  AIC                       :", fmt(g$AIC, digits), "\n")
  cat("  BIC                       :", fmt(g$BIC, digits), "\n")
  cat("  LR test vs intercept      :", fmt(g$lr_stat, digits),
      "on", g$lr_df, "df, p =", format.pval(g$lr_p_value, digits = 3), "\n")
  cat("  Pseudo R2 (McFadden)      :", fmt(g$pseudo_r2_mcfadden, digits),
      "\n")
  cat("  Correctly predicted       :",
      paste0(formatC(g$percent_correct, format = "f", digits = 1), "%"), "\n")

  if (!is.null(x$marginal_effects)) {
    cat("\nAverage marginal effects, probability scale\n")
    print(summary(x$marginal_effects))
  }
  invisible(x)
}


#' @title Extract components from a factorial survey logit model
#'
#' @description
#' Methods that forward to the underlying [stats::glm()] fit. `vcov()`
#' returns the covariance matrix used for inference, which is cluster-robust
#' when the model was fitted with `cluster`.
#'
#' @param object An object of class `fs_logit`.
#' @param ... Passed on to the corresponding method for `glm` objects.
#'
#' @return The value returned by the corresponding `glm` method.
#'
#' @name fs_logit-extractors
#' @examples
#' set.seed(1)
#' d <- data.frame(id = rep(1:30, each = 4), x = rnorm(120))
#' d$y <- rbinom(120, 1, plogis(0.8 * d$x))
#' fit <- fs_logit(y ~ x, data = d, cluster = ~ id, compute_me = FALSE)
#' coef(fit)
#' vcov(fit)
NULL

#' @rdname fs_logit-extractors
#' @export
coef.fs_logit <- function(object, ...) stats::coef(object$model, ...)

#' @rdname fs_logit-extractors
#' @export
vcov.fs_logit <- function(object, ...) object$vcov

#' @rdname fs_logit-extractors
#' @export
logLik.fs_logit <- function(object, ...) stats::logLik(object$model, ...)

#' @rdname fs_logit-extractors
#' @export
nobs.fs_logit <- function(object, ...) stats::nobs(object$model, ...)

#' @rdname fs_logit-extractors
#' @export
fitted.fs_logit <- function(object, ...) stats::fitted(object$model, ...)
