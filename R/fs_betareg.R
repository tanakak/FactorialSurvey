#' @title Beta regression for bounded rating outcomes
#'
#' @description
#' Fits a beta regression to the ratings collected in a factorial survey. Beta
#' regression is well suited to responses that are bounded and often skewed,
#' such as a 0 to 10 acceptability score, because it models the mean on a
#' bounded support instead of assuming an unbounded normal error.
#'
#' The observed scale is mapped onto the open unit interval, the model is
#' fitted with [betareg::betareg()], and -- optionally -- average marginal
#' effects are reported on both the unit interval and the original scale.
#'
#' @param formula A symbolic description of the model, passed to
#'   [betareg::betareg()]. Extended formulas of the form `y ~ x | z`, which
#'   model the precision parameter as well as the mean, are supported.
#' @param data A data frame containing the variables in `formula`.
#' @param scale A numeric vector of length two giving the lower and upper
#'   bound of the observed rating scale. The default `c(0, 10)` matches a
#'   0 to 10 scale. Ignored when `transform` is `FALSE`.
#' @param transform A logical value. If `TRUE` (the default), the response is
#'   rescaled from `scale` onto the open interval (0, 1). If `FALSE`, the
#'   response is assumed to already lie strictly inside (0, 1).
#' @param cluster How to account for the fact that each respondent rates
#'   several vignettes. Give the respondent identifier as a one-sided formula
#'   such as `~ id`, as the name of a column of `data`, or as a vector with
#'   one element per row of `data`. Standard errors, marginal effects and
#'   everything computed from them -- including [fs_mwtp()] -- are then
#'   cluster-robust. Use `FALSE` to treat every rating as independent. The
#'   default `NULL` does the same but issues a warning, because in a factorial
#'   survey independence is almost never true and ordinary standard errors
#'   are then not valid.
#' @param compute_me A logical value. If `TRUE` (the default), average
#'   marginal effects are computed with the \pkg{margins} package when it is
#'   installed.
#' @param verbose A logical value. If `TRUE`, progress is reported through
#'   [message()]. The default is `FALSE`; use `summary()` on the result to see
#'   the fitted model.
#' @param ... Further arguments passed to [betareg::betareg()], such as
#'   `link` or `subset`.
#'
#' @return An object of class `fs_betareg`, a list with components:
#'   \describe{
#'     \item{`model`}{The fitted [betareg::betareg()] object.}
#'     \item{`vcov`}{The covariance matrix of the estimates used for all
#'       inference: cluster-robust when `cluster` is given, otherwise the
#'       model-based matrix.}
#'     \item{`cluster`}{A list describing the clustering (`name`,
#'       `n_clusters`), or `NULL`.}
#'     \item{`marginal_effects`}{The `margins` object, or `NULL`.}
#'     \item{`marginal_effects_rescaled`}{A data frame of average marginal
#'       effects expressed on the original rating scale, or `NULL`.}
#'     \item{`gof`}{A named list of goodness-of-fit measures: `logLik`,
#'       `logLik_null`, `AIC`, `BIC`, `lr_stat`, `lr_df`, `lr_p_value`,
#'       `pseudo_r2_cox_snell`, `pseudo_r2_betareg` and `nobs`.}
#'     \item{`scale`}{The rating scale supplied.}
#'     \item{`transform`}{Whether the response was rescaled.}
#'     \item{`rescale_factor`}{The factor mapping effects on (0, 1) back to
#'       the original scale, or `NA` when `transform` is `FALSE`.}
#'     \item{`call`}{The matched call.}
#'   }
#'
#' @details
#' The response is mapped onto (0, 1) in two steps. It is first shifted and
#' scaled to \eqn{[0, 1]} using `scale`, then pulled away from the endpoints
#' with the transformation of Smithson and Verkuilen (2006),
#' \eqn{(y(N - 1) + 0.5) / N}, where \eqn{N} is the number of observations.
#' Average marginal effects on the unit interval are converted back to the
#' original scale by multiplying by \eqn{(u - l) N / (N - 1)}, where \eqn{l}
#' and \eqn{u} are the bounds in `scale`.
#'
#' Two pseudo R-squared values are reported because they measure different
#' things and are easy to confuse. `pseudo_r2_cox_snell` is the
#' likelihood-ratio measure of Cox and Snell,
#' \eqn{1 - \exp(2(\ell_0 - \ell) / n)}, computed against an intercept-only
#' model that this function fits for the purpose. `pseudo_r2_betareg` is the
#' value \pkg{betareg} itself reports, namely the squared correlation between
#' the linear predictor and the link-transformed response.
#'
#' With `cluster`, the covariance matrix is the cluster-robust sandwich
#' estimator of [sandwich::vcovCL()] with the usual \eqn{G / (G - 1)}
#' small-sample adjustment, where \eqn{G} is the number of respondents. It
#' allows the ratings of one respondent to be correlated in any way, which is
#' what a respondent-specific taste or response style produces. With few
#' respondents (say, fewer than 30) these standard errors are themselves
#' imprecise.
#'
#' McFadden's \eqn{1 - \ell / \ell_0} is deliberately **not** reported. It is
#' designed for discrete outcomes, whose log-likelihoods are always negative.
#' A beta regression is a continuous density, so its log-likelihood is
#' frequently positive, and McFadden's ratio then takes values outside
#' \eqn{[0, 1]} and cannot be interpreted. The Cox and Snell measure is well
#' behaved regardless of the sign of the log-likelihood.
#'
#' @section Clustered data:
#' When each respondent rates several vignettes the ratings of one person are
#' correlated, and standard errors that assume independence are not valid.
#' The error is largest for characteristics of the respondent, such as age or
#' gender, which are constant within a respondent: there, ordinary standard
#' errors are much too small (in the package's validation simulations,
#' nominal 95 percent intervals covered the truth only about 80 percent of the
#' time). For attributes varied within respondents they were close to right
#' or somewhat too large. Cluster-robust standard errors are valid in both
#' cases, so use `cluster`.
#'
#' @references
#' Ferrari, S. L. P. and Cribari-Neto, F. (2004). Beta regression for
#' modelling rates and proportions. *Journal of Applied Statistics*, 31(7),
#' 799--815. \doi{10.1080/0266476042000214501}
#'
#' Smithson, M. and Verkuilen, J. (2006). A better lemon squeezer?
#' Maximum-likelihood regression with beta-distributed dependent variables.
#' *Psychological Methods*, 11(1), 54--71.
#' \doi{10.1037/1082-989X.11.1.54}
#'
#' @seealso [fs_mwtp()] for turning the fitted coefficients into a
#'   willingness to pay, [fs_defficient()] for building the design.
#'
#' @importFrom stats AIC BIC logLik nobs update coef pchisq
#' @export
#' @examples
#' set.seed(123)
#' # 50 respondents, each rating 3 vignettes
#' n <- 150
#' d <- data.frame(
#'   id      = rep(1:50, each = 3),
#'   quality = rnorm(n),
#'   cost    = runif(n, 100, 1000)
#' )
#' taste <- rnorm(50, sd = 0.4)[d$id]   # respondent-specific shift
#' d$rating <- 10 * plogis(0.6 * d$quality - 0.002 * (d$cost - 500) +
#'                           taste + rnorm(n, sd = 0.5))
#'
#' fit <- fs_betareg(rating ~ quality + cost, data = d, cluster = ~ id,
#'                   compute_me = FALSE)
#' fit
#' summary(fit)
fs_betareg <- function(formula,
                       data,
                       scale = c(0, 10),
                       transform = TRUE,
                       cluster = NULL,
                       compute_me = TRUE,
                       verbose = FALSE,
                       ...) {

  cl <- match.call()

  if (!requireNamespace("betareg", quietly = TRUE)) {
    stop("Package 'betareg' is required. Install it with ",
         "install.packages('betareg').", call. = FALSE)
  }
  if (!inherits(formula, "formula")) {
    stop("`formula` must be a formula.", call. = FALSE)
  }
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  check_flag(transform)
  check_flag(compute_me)
  check_flag(verbose)

  ## ---- Response ------------------------------------------------------------
  y_var <- all.vars(formula)[1L]
  if (!y_var %in% names(data)) {
    stop("Response variable '", y_var, "' not found in `data`.", call. = FALSE)
  }
  y <- data[[y_var]]
  if (!is.numeric(y)) {
    stop("The response variable must be numeric.", call. = FALSE)
  }
  if (anyNA(y)) {
    stop("The response variable contains missing values.", call. = FALSE)
  }

  cl_info <- resolve_cluster(cluster, data)

  work <- data
  rescale_factor <- NA_real_

  if (transform) {
    if (!is.numeric(scale) || length(scale) != 2L || anyNA(scale) ||
        scale[2L] <= scale[1L]) {
      stop("`scale` must be a numeric vector of length two with ",
           "scale[1] < scale[2].", call. = FALSE)
    }
    if (min(y) < scale[1L] || max(y) > scale[2L]) {
      stop("The response ranges from ", fmt(min(y)), " to ", fmt(max(y)),
           ", which falls outside `scale` = [", scale[1L], ", ", scale[2L],
           "].", call. = FALSE)
    }
    width <- scale[2L] - scale[1L]
    n_obs <- length(y)
    y01 <- (y - scale[1L]) / width
    y_beta <- (y01 * (n_obs - 1) + 0.5) / n_obs
    work[[y_var]] <- y_beta
    rescale_factor <- width * n_obs / (n_obs - 1)
    say(verbose, "Response rescaled from [", scale[1L], ", ", scale[2L],
        "] onto the open unit interval.")
  } else {
    if (min(y) <= 0 || max(y) >= 1) {
      stop("With `transform = FALSE` the response must lie strictly inside ",
           "(0, 1); observed range is [", fmt(min(y)), ", ", fmt(max(y)),
           "].", call. = FALSE)
    }
  }

  ## ---- Fit -----------------------------------------------------------------
  say(verbose, "Fitting the beta regression ...")
  model <- tryCatch(
    betareg_quiet(formula, work, ...),
    error = function(e) {
      stop("The beta regression failed to converge: ", conditionMessage(e),
           call. = FALSE)
    }
  )

  ## ---- Goodness of fit -----------------------------------------------------
  gof <- beta_gof(model, formula, work)

  ## ---- Covariance matrix ---------------------------------------------------
  vc <- stats::vcov(model)
  cluster_out <- NULL
  if (!is.null(cl_info$values)) {
    used <- match(rownames(model$model), rownames(work))
    cl_used <- cl_info$values[used]
    vc <- sandwich::vcovCL(model, cluster = cl_used, type = "HC0",
                           cadjust = TRUE)
    cluster_out <- list(name = cl_info$name,
                        n_clusters = length(unique(cl_used)))
    say(verbose, "Standard errors clustered by ", cl_info$name, " (",
        cluster_out$n_clusters, " clusters).")
  }

  ## ---- Average marginal effects --------------------------------------------
  me <- NULL
  me_rescaled <- NULL
  mean_names <- names(stats::coef(model, model = "mean"))
  if (compute_me) {
    if (!requireNamespace("margins", quietly = TRUE)) {
      warning("Package 'margins' is not installed; skipping marginal ",
              "effects. Install it with install.packages('margins').",
              call. = FALSE)
    } else {
      me <- tryCatch(
        margins::margins(model, vcov = vc[mean_names, mean_names,
                                          drop = FALSE]),
        error = function(e) {
          warning("Marginal effects could not be computed: ",
                  conditionMessage(e), call. = FALSE)
          NULL
        }
      )
      if (!is.null(me) && transform) {
        me_summary <- summary(me)
        if (nrow(me_summary) > 0L) {
          for (col in intersect(c("AME", "SE", "lower", "upper"),
                                names(me_summary))) {
            me_summary[[col]] <- me_summary[[col]] * rescale_factor
          }
          me_rescaled <- me_summary
        }
      }
    }
  }

  out <- list(
    model                     = model,
    vcov                      = vc,
    cluster                   = cluster_out,
    marginal_effects          = me,
    marginal_effects_rescaled = me_rescaled,
    gof                       = gof,
    scale                     = if (transform) scale else c(0, 1),
    transform                 = transform,
    rescale_factor            = rescale_factor,
    call                      = cl
  )
  class(out) <- "fs_betareg"
  out
}


#' Goodness-of-fit measures for a beta regression
#'
#' @param model A fitted `betareg` object.
#' @param formula The formula used to fit it.
#' @param data The data the model was fitted to.
#' @return A named list of fit measures.
#' @noRd
beta_gof <- function(model, formula, data) {
  ll <- as.numeric(stats::logLik(model))
  n <- stats::nobs(model)

  null_fit <- tryCatch(
    betareg_quiet(stats::update(formula, . ~ 1), data),
    error = function(e) NULL
  )
  ll_null <- if (is.null(null_fit)) NA_real_ else {
    as.numeric(stats::logLik(null_fit))
  }

  if (is.na(ll_null)) {
    cox_snell <- NA_real_
    lr_stat <- NA_real_
    lr_df <- NA_integer_
    lr_p <- NA_real_
  } else {
    cox_snell <- 1 - exp(2 * (ll_null - ll) / n)
    lr_stat <- 2 * (ll - ll_null)
    lr_df <- length(stats::coef(model)) - length(stats::coef(null_fit))
    lr_p <- if (lr_df > 0L) {
      stats::pchisq(lr_stat, df = lr_df, lower.tail = FALSE)
    } else {
      NA_real_
    }
  }

  list(
    logLik              = ll,
    logLik_null         = ll_null,
    AIC                 = stats::AIC(model),
    BIC                 = stats::BIC(model),
    lr_stat             = lr_stat,
    lr_df               = lr_df,
    lr_p_value          = lr_p,
    pseudo_r2_cox_snell = cox_snell,
    pseudo_r2_betareg   = model$pseudo.r.squared,
    nobs                = n
  )
}


#' @title Print a fitted factorial survey beta regression
#'
#' @param x An object of class `fs_betareg`.
#' @param digits Number of significant digits.
#' @param ... Ignored.
#'
#' @return `x`, invisibly. Called for the side effect of printing.
#' @export
#' @examples
#' set.seed(1)
#' d <- data.frame(id = rep(1:20, each = 3), x = rnorm(60))
#' d$rating <- 10 * plogis(0.5 * d$x + rnorm(60, sd = 0.4))
#' print(fs_betareg(rating ~ x, data = d, cluster = ~ id,
#'                compute_me = FALSE))
print.fs_betareg <- function(x, digits = 4, ...) {
  cat("Factorial survey beta regression\n")
  cat("Call: ", deparse(x$call), "\n", sep = "")
  cat("Rating scale :", paste0("[", x$scale[1L], ", ", x$scale[2L], "]"),
      if (!x$transform) "(no transformation applied)" else "", "\n")
  cat("Observations :", x$gof$nobs, "\n")
  cat(se_label(x$cluster), "\n\n", sep = "")
  cat("Mean model coefficients:\n")
  print(round(stats::coef(x$model, model = "mean"), digits))
  cat("\nUse summary() for standard errors, fit statistics and marginal",
      "effects.\n")
  invisible(x)
}


#' @title Summarise a fitted factorial survey beta regression
#'
#' @param object An object of class `fs_betareg`.
#' @param ... Ignored.
#'
#' @return An object of class `summary.fs_betareg`, a list holding the
#'   \pkg{betareg} summary, the fit measures and the rescaled marginal
#'   effects.
#' @export
#' @examples
#' set.seed(1)
#' d <- data.frame(id = rep(1:20, each = 3), x = rnorm(60))
#' d$rating <- 10 * plogis(0.5 * d$x + rnorm(60, sd = 0.4))
#' summary(fs_betareg(rating ~ x, data = d, cluster = ~ id,
#'                compute_me = FALSE))
summary.fs_betareg <- function(object, ...) {
  ms <- summary(object$model)
  if (!is.null(object$cluster)) {
    se <- sqrt(diag(object$vcov))
    for (part in names(ms$coefficients)) {
      tab <- ms$coefficients[[part]]
      s_part <- se[rownames(tab)]
      tab[, 2L] <- s_part
      tab[, 3L] <- tab[, 1L] / s_part
      tab[, 4L] <- 2 * stats::pnorm(-abs(tab[, 3L]))
      ms$coefficients[[part]] <- tab
    }
  }
  out <- list(
    model_summary             = ms,
    cluster                   = object$cluster,
    gof                       = object$gof,
    marginal_effects          = object$marginal_effects,
    marginal_effects_rescaled = object$marginal_effects_rescaled,
    scale                     = object$scale,
    transform                 = object$transform,
    call                      = object$call
  )
  class(out) <- "summary.fs_betareg"
  out
}


#' @title Print the summary of a factorial survey beta regression
#'
#' @param x An object of class `summary.fs_betareg`.
#' @param digits Number of significant digits.
#' @param ... Ignored.
#'
#' @return `x`, invisibly. Called for the side effect of printing.
#' @export
#' @examples
#' set.seed(1)
#' d <- data.frame(id = rep(1:20, each = 3), x = rnorm(60))
#' d$rating <- 10 * plogis(0.5 * d$x + rnorm(60, sd = 0.4))
#' print(summary(fs_betareg(rating ~ x, data = d, cluster = ~ id,
#'                compute_me = FALSE)))
print.summary.fs_betareg <- function(x, digits = 4, ...) {
  cat("Factorial survey beta regression\n")
  cat("Call: ", deparse(x$call), "\n", sep = "")
  cat(se_label(x$cluster), "\n\n", sep = "")
  print(x$model_summary)

  g <- x$gof
  cat("\nFit statistics\n")
  cat("  Log-likelihood            :", fmt(g$logLik, digits), "\n")
  cat("  Log-likelihood (intercept):", fmt(g$logLik_null, digits), "\n")
  cat("  AIC                       :", fmt(g$AIC, digits), "\n")
  cat("  BIC                       :", fmt(g$BIC, digits), "\n")
  cat("  LR test vs intercept      :", fmt(g$lr_stat, digits),
      "on", g$lr_df, "df, p =", format.pval(g$lr_p_value, digits = 3), "\n")
  cat("  Pseudo R2 (Cox-Snell)     :", fmt(g$pseudo_r2_cox_snell, digits), "\n")
  cat("  Pseudo R2 (betareg)       :", fmt(g$pseudo_r2_betareg, digits), "\n")

  if (!is.null(x$marginal_effects)) {
    cat("\nAverage marginal effects, unit interval\n")
    print(summary(x$marginal_effects))
  }
  if (!is.null(x$marginal_effects_rescaled)) {
    cat("\nAverage marginal effects, rating scale [",
        x$scale[1L], ", ", x$scale[2L], "]\n", sep = "")
    print(x$marginal_effects_rescaled)
  }
  invisible(x)
}


#' @title Extract components from a factorial survey beta regression
#'
#' @description
#' Methods that forward to the underlying [betareg::betareg()] fit, so that
#' `fs_betareg` objects can be used with the usual extractor functions.
#' `vcov()` returns the covariance matrix used for inference, which is
#' cluster-robust when the model was fitted with `cluster`.
#'
#' @param object An object of class `fs_betareg`.
#' @param ... Passed on to the corresponding method for `betareg` objects.
#'
#' @return The value returned by the corresponding `betareg` method: a
#'   named numeric vector for `coef()`, a matrix for `vcov()`, an object of
#'   class `logLik` for `logLik()`, a single integer for `nobs()`, and a
#'   numeric vector for `fitted()`.
#'
#' @name fs_betareg-extractors
#' @examples
#' set.seed(1)
#' d <- data.frame(id = rep(1:20, each = 3), x = rnorm(60))
#' d$rating <- 10 * plogis(0.5 * d$x + rnorm(60, sd = 0.4))
#' fit <- fs_betareg(rating ~ x, data = d, cluster = ~ id,
#'                compute_me = FALSE)
#' coef(fit)
#' nobs(fit)
NULL

#' @rdname fs_betareg-extractors
#' @export
coef.fs_betareg <- function(object, ...) stats::coef(object$model, ...)

#' @rdname fs_betareg-extractors
#' @importFrom stats vcov
#' @export
vcov.fs_betareg <- function(object, ...) {
  if (!is.null(object$vcov)) object$vcov else stats::vcov(object$model, ...)
}

#' @rdname fs_betareg-extractors
#' @export
logLik.fs_betareg <- function(object, ...) stats::logLik(object$model, ...)

#' @rdname fs_betareg-extractors
#' @export
nobs.fs_betareg <- function(object, ...) stats::nobs(object$model, ...)

#' @rdname fs_betareg-extractors
#' @importFrom stats fitted
#' @export
fitted.fs_betareg <- function(object, ...) stats::fitted(object$model, ...)


#' Resolve the `cluster` argument of fs_betareg()
#'
#' @return A list with `values` (a vector aligned with the rows of `data`, or
#'   `NULL` for no clustering) and `name`.
#' @noRd
resolve_cluster <- function(cluster, data) {
  if (is.null(cluster)) {
    warning("Standard errors treat every rating as independent. In a ",
            "factorial survey each respondent rates several vignettes, so ",
            "these standard errors are not valid; for respondent ",
            "characteristics such as age or gender they are usually much ",
            "too small. ",
            "Supply the ",
            "respondent identifier, e.g. `cluster = ~ id`, or use ",
            "`cluster = FALSE` to silence this warning.", call. = FALSE)
    return(list(values = NULL, name = NULL))
  }
  if (isFALSE(cluster)) {
    return(list(values = NULL, name = NULL))
  }
  if (inherits(cluster, "formula")) {
    vars <- all.vars(cluster)
    if (length(cluster) != 2L || length(vars) != 1L) {
      stop("`cluster` must be a one-sided formula naming one variable, ",
           "such as `~ id`.", call. = FALSE)
    }
    cluster <- vars
  }
  if (is.character(cluster) && length(cluster) == 1L &&
      length(cluster) != nrow(data)) {
    if (!cluster %in% names(data)) {
      stop("Cluster variable '", cluster, "' not found in `data`.",
           call. = FALSE)
    }
    values <- data[[cluster]]
    name <- cluster
  } else {
    if (length(cluster) != nrow(data)) {
      stop("`cluster` must be a formula, a column name, FALSE, or a vector ",
           "with one element per row of `data`.", call. = FALSE)
    }
    values <- cluster
    name <- "the supplied vector"
  }
  if (anyNA(values)) {
    stop("The cluster variable contains missing values.", call. = FALSE)
  }
  if (length(unique(values)) < 2L) {
    stop("The cluster variable must have at least two distinct values.",
         call. = FALSE)
  }
  list(values = values, name = name)
}

#' Describe the standard errors in printed output
#' @noRd
se_label <- function(cluster) {
  if (is.null(cluster)) {
    "Standard errors: model-based (ratings treated as independent)"
  } else {
    paste0("Standard errors: clustered by ", cluster$name, " (",
           cluster$n_clusters, " clusters)")
  }
}


#' Fit a beta regression without the start-value warning
#'
#' betareg warns when its moment-based starting value for the precision
#' parameter is unusable and falls back to 1. This is common with ratings
#' piled up at the ends of the scale, does not affect the maximum likelihood
#' estimates, and only alarms users, so that one warning is muffled. All other
#' warnings pass through.
#' @noRd
betareg_quiet <- function(formula, work, ...) {
  ## The argument is called `work` so that the stored call reads
  ## `betareg(formula = formula, data = work)`, which is how the 'margins'
  ## package finds the data again from within fs_betareg().
  withCallingHandlers(
    betareg::betareg(formula = formula, data = work, ...),
    warning = function(w) {
      if (grepl("no valid starting value for precision parameter",
                conditionMessage(w), fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )
}
