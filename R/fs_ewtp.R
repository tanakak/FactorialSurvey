#' @title Expected willingness to pay for a policy scenario
#'
#' @description
#' Computes the expected willingness to pay (EWTP) for one or more policy
#' scenarios -- complete bundles of attribute levels -- from a model with a
#' logit link and a cost attribute. Where [fs_mwtp()] values a change in a
#' single attribute, `fs_ewtp()` values a whole scenario.
#'
#' @param object A fitted model with a logit link: an `fs_logit` object from
#'   [fs_logit()], an `fs_betareg` object from [fs_betareg()], or a plain
#'   binomial-logit [stats::glm()] or logit-link [betareg::betareg()] fit.
#'   The covariance matrix returned by `vcov(object)` is used, so a model
#'   fitted with `cluster` gives cluster-robust intervals.
#' @param newdata A data frame with one row per scenario, giving the level of
#'   every attribute (and any respondent covariate) in the model. The price
#'   column, if present, is ignored: EWTP is the payment the scenario is
#'   worth, not the response at a given price. Row names, when set, are used
#'   to label the scenarios.
#' @param price The name of the price (cost) variable. It must enter the model
#'   as a single numeric term with a negative coefficient.
#' @param method How the confidence interval is obtained: `"delta"` (the
#'   default) or `"krinsky-robb"` (simulation from the estimated sampling
#'   distribution of the coefficients).
#' @param average A logical value. If `TRUE`, the EWTP is averaged over the
#'   rows of `newdata` and a single estimate is returned. Use it with one row
#'   per respondent to obtain a sample mean of individual EWTP.
#' @param level The confidence level of the interval.
#' @param n_sim The number of draws used by `method = "krinsky-robb"`.
#' @param seed A single number making the Krinsky-Robb draws reproducible, or
#'   `NULL`. The caller's random number state is restored on exit.
#'
#' @return An object of class `fs_ewtp`, a list with components:
#'   \describe{
#'     \item{`estimates`}{A data frame with one row per scenario (or a single
#'       row when `average = TRUE`) and columns `scenario`, `index` (the
#'       utility index \eqn{x'\beta} at zero cost), `ewtp`, `std_error`,
#'       `conf_low` and `conf_high`.}
#'     \item{`price`}{The name of the price variable.}
#'     \item{`method`}{The interval method used.}
#'     \item{`level`}{The confidence level.}
#'     \item{`draws`}{The simulated EWTP values, for
#'       `method = "krinsky-robb"`; otherwise `NULL`.}
#'     \item{`n_invalid`}{The number of Krinsky-Robb draws discarded because
#'       the simulated price coefficient was not negative.}
#'   }
#'
#' @details
#' Write the model's linear predictor for a scenario \eqn{x} offered at cost
#' \eqn{R} as \eqn{x'\beta - \gamma R}, where \eqn{\gamma > 0} is minus the
#' price coefficient. With a logit link the probability of support (for
#' [fs_logit()]) or the expected rating on the unit interval (for
#' [fs_betareg()]) is \eqn{\Lambda(x'\beta - \gamma R)}. Treating this, as in
#' the utility-difference model of Hanemann (1984), as the probability that
#' willingness to pay exceeds \eqn{R}, and willingness to pay as
#' non-negative, the expected willingness to pay is
#' \deqn{E(\mathrm{WTP}) = \int_0^\infty \Lambda(x'\beta - \gamma R)\, dR =
#'   \frac{1}{\gamma} \ln\{1 + \exp(x'\beta)\},}
#' the mean of the distribution truncated at zero (Hanemann 1989). Tanaka
#' (2025) and Tanaka et al. (2027, Eq. 6) apply it to complete policy
#' scenarios in factorial surveys, including ratings analysed by beta
#' regression. It is expressed in the units of the price variable.
#'
#' The intercept and every attribute enter \eqn{x'\beta}, so EWTP depends on
#' the full scenario, not only on differences between scenarios, and it is
#' not additive: the EWTP of a bundle is not the sum of marginal WTPs. For a
#' beta regression the interpretation rests on reading the mean rating, after
#' mapping the scale to the unit interval, as a propensity to support the
#' scenario.
#'
#' The delta-method standard error uses the gradient
#' \eqn{\partial E / \partial \beta = \Lambda(x'\beta)\, x / \gamma} and
#' \eqn{\partial E / \partial \beta_{price} = E / \gamma}. Because EWTP is a
#' non-linear function with \eqn{\gamma} in the denominator, the
#' Krinsky-Robb interval is preferable when the price coefficient is
#' imprecise.
#'
#' @references
#' Hanemann, W. M. (1984). Welfare evaluations in contingent valuation
#' experiments with discrete responses. *American Journal of Agricultural
#' Economics*, 66(3), 332--341. \doi{10.2307/1240800}
#'
#' Hanemann, W. M. (1989). Welfare evaluations in contingent valuation
#' experiments with discrete response data: Reply. *American Journal of
#' Agricultural Economics*, 71(4), 1057--1061. \doi{10.2307/1242685}
#'
#' Tanaka, K. (2025). A unified framework for estimating willingness to pay
#' using factorial survey experiments. SSRN Working Paper.
#' \doi{10.2139/ssrn.5238343}
#'
#' Tanaka, K., Dill, J., McNeil, N., Geller, R. and Kondo, N. (2027). What are
#' safer streets worth? Bridging resident preferences and policy design in
#' traffic calming. *Transportation Research Part A*, 215, 105293.
#' \doi{10.1016/j.tra.2026.105293}
#'
#' @seealso [fs_mwtp()] for the willingness to pay for a single attribute.
#'
#' @export
#' @examples
#' set.seed(42)
#' n_resp <- 80
#' d <- data.frame(
#'   id      = rep(seq_len(n_resp), each = 4),
#'   quality = sample(0:1, 4 * n_resp, replace = TRUE),
#'   speed   = sample(0:1, 4 * n_resp, replace = TRUE),
#'   cost    = sample(c(500, 1000, 2000), 4 * n_resp, replace = TRUE)
#' )
#' d$support <- rbinom(nrow(d), 1, plogis(0.5 + 0.8 * d$quality +
#'                                          0.4 * d$speed - 0.001 * d$cost))
#' fit <- fs_logit(support ~ quality + speed + cost, data = d,
#'                 cluster = ~ id, compute_me = FALSE)
#'
#' scenarios <- data.frame(quality = c(0, 1, 1), speed = c(0, 0, 1),
#'                         row.names = c("status quo", "quality", "both"))
#' fs_ewtp(fit, scenarios, price = "cost")
fs_ewtp <- function(object,
                    newdata,
                    price,
                    method = c("delta", "krinsky-robb"),
                    average = FALSE,
                    level = 0.95,
                    n_sim = 5000,
                    seed = NULL) {
  method <- match.arg(method)
  if (missing(newdata) || !is.data.frame(newdata) || nrow(newdata) == 0L) {
    stop("`newdata` must be a data frame with one row per scenario.",
         call. = FALSE)
  }
  if (missing(price) || !is.character(price) || length(price) != 1L) {
    stop("`price` must be the name of the price variable.", call. = FALSE)
  }
  check_flag(average)
  if (!is.numeric(level) || length(level) != 1L || level <= 0 || level >= 1) {
    stop("`level` must be a single number strictly between 0 and 1.",
         call. = FALSE)
  }
  check_count(n_sim)

  X <- ewtp_model_matrix(object, newdata, price)
  beta_all <- stats::coef(object)
  vc_all <- stats::vcov(object)
  keep <- union(colnames(X), price)
  if (!all(keep %in% names(beta_all)) || !all(keep %in% rownames(vc_all))) {
    stop("The coefficients implied by `newdata` do not match those of the ",
         "model.", call. = FALSE)
  }
  beta <- beta_all[keep]
  vc <- vc_all[keep, keep, drop = FALSE]

  gamma <- -unname(beta[price])
  if (!(gamma > 0)) {
    stop("The price coefficient must be negative for EWTP to be defined; ",
         "it is ", fmt(-gamma), ".", call. = FALSE)
  }

  Xb <- X[, colnames(X), drop = FALSE]
  Xb[, price] <- 0
  index <- drop(Xb %*% beta[colnames(Xb)])
  ewtp <- softplus(index) / gamma

  ## Gradient with respect to `beta` (rows: scenarios, columns: coefficients)
  grad <- matrix(0, nrow = nrow(Xb), ncol = length(keep),
                 dimnames = list(NULL, keep))
  grad[, colnames(Xb)] <- stats::plogis(index) * Xb / gamma
  grad[, price] <- ewtp / gamma

  labels <- scenario_labels(newdata)
  if (average) {
    ewtp <- mean(ewtp)
    index <- mean(index)
    grad <- matrix(colMeans(grad), nrow = 1L, dimnames = list(NULL, keep))
    labels <- paste0("mean of ", nrow(Xb), " rows")
  }
  std_error <- sqrt(pmax(rowSums((grad %*% vc) * grad), 0))

  alpha <- 1 - level
  z <- stats::qnorm(1 - alpha / 2)
  draws <- NULL
  n_invalid <- 0L

  if (method == "delta") {
    lo <- ewtp - z * std_error
    hi <- ewtp + z * std_error
  } else {
    check_seed(seed)
    if (!is.null(seed)) {
      old_rng <- rng_state()
      on.exit(restore_rng(old_rng), add = TRUE)
      set.seed(seed)
    }
    b <- rmvnorm_chol(n_sim, beta, vc)
    g <- -b[, price]
    valid <- g > 0
    n_invalid <- sum(!valid)
    if (n_invalid / n_sim > 0.01) {
      warning(n_invalid, " of ", n_sim, " draws had a non-negative price ",
              "coefficient and were discarded; the price coefficient is ",
              "imprecise and the interval should be read with caution.",
              call. = FALSE)
    }
    idx <- tcrossprod(Xb, b[, colnames(Xb), drop = FALSE])
    sim <- sweep(softplus(idx), 2L, g, "/")
    sim[, !valid] <- NA_real_
    if (average) {
      sim <- matrix(colMeans(sim), nrow = 1L)
    }
    draws <- t(sim)
    colnames(draws) <- labels
    qs <- apply(draws, 2L, stats::quantile,
                probs = c(alpha / 2, 1 - alpha / 2), na.rm = TRUE,
                names = FALSE)
    lo <- qs[1L, ]
    hi <- qs[2L, ]
    std_error <- apply(draws, 2L, stats::sd, na.rm = TRUE)
  }

  estimates <- data.frame(
    scenario  = labels,
    index     = unname(index),
    ewtp      = unname(ewtp),
    std_error = unname(std_error),
    conf_low  = unname(lo),
    conf_high = unname(hi),
    stringsAsFactors = FALSE
  )

  out <- list(
    estimates = estimates,
    price     = price,
    method    = method,
    level     = level,
    draws     = draws,
    n_invalid = n_invalid
  )
  class(out) <- "fs_ewtp"
  out
}


#' Numerically stable log(1 + exp(x))
#' @noRd
softplus <- function(x) {
  ifelse(x > 30, x, log1p(exp(pmin(x, 30))))
}

#' Labels for the rows of newdata
#' @noRd
scenario_labels <- function(newdata) {
  rn <- rownames(newdata)
  if (is.null(rn) || identical(rn, as.character(seq_len(nrow(newdata))))) {
    as.character(seq_len(nrow(newdata)))
  } else {
    rn
  }
}

#' Model matrix of the mean equation for new scenarios
#'
#' @return The model matrix, with the price column present (its values are
#'   overwritten by the caller).
#' @noRd
ewtp_model_matrix <- function(object, newdata, price) {
  mod <- if (inherits(object, c("fs_betareg", "fs_logit"))) {
    object$model
  } else {
    object
  }

  if (inherits(mod, "betareg")) {
    if (!identical(mod$link$mean$name, "logit")) {
      stop("EWTP requires a logit link; the model uses '",
           mod$link$mean$name, "'.", call. = FALSE)
    }
    tt <- stats::delete.response(stats::terms(mod, model = "mean"))
    xlev <- mod$levels$mean
    contr <- mod$contrasts$mean
  } else if (inherits(mod, "glm")) {
    fam <- stats::family(mod)
    if (!identical(fam$family, "binomial") || !identical(fam$link, "logit")) {
      stop("EWTP requires a binomial model with a logit link.",
           call. = FALSE)
    }
    tt <- stats::delete.response(stats::terms(mod))
    xlev <- mod$xlevels
    contr <- mod$contrasts
  } else {
    stop("`object` must be an fs_logit or fs_betareg fit, or a logit-link ",
         "glm or betareg model.", call. = FALSE)
  }

  if (!price %in% attr(tt, "term.labels")) {
    stop("Price variable '", price, "' is not a term of the model.",
         call. = FALSE)
  }
  needed <- setdiff(all.vars(tt), price)
  absent <- setdiff(needed, names(newdata))
  if (length(absent) > 0L) {
    stop("`newdata` lacks variable(s) used by the model: ",
         paste(absent, collapse = ", "), ".", call. = FALSE)
  }

  nd <- newdata
  nd[[price]] <- 0
  mf <- stats::model.frame(tt, nd, xlev = xlev, na.action = stats::na.pass)
  X <- stats::model.matrix(tt, mf, contrasts.arg = contr)
  if (anyNA(X)) {
    stop("`newdata` contains missing values in the model variables.",
         call. = FALSE)
  }
  if (!price %in% colnames(X)) {
    stop("Price variable '", price, "' must enter the model as a single ",
         "numeric term.", call. = FALSE)
  }
  X
}


#' @title Print expected willingness to pay estimates
#'
#' @param x An object of class `fs_ewtp`, from [fs_ewtp()].
#' @param digits Number of significant digits.
#' @param ... Ignored.
#'
#' @return `x`, invisibly. Called for the side effect of printing.
#' @export
#' @examples
#' set.seed(1)
#' d <- data.frame(id = rep(1:40, each = 4), q = rbinom(160, 1, 0.5),
#'                 cost = sample(c(100, 300, 500), 160, replace = TRUE))
#' d$y <- rbinom(160, 1, plogis(0.5 + d$q - 0.004 * d$cost))
#' fit <- fs_logit(y ~ q + cost, data = d, cluster = ~ id, compute_me = FALSE)
#' print(fs_ewtp(fit, data.frame(q = 0:1), price = "cost"))
print.fs_ewtp <- function(x, digits = 4, ...) {
  label <- c(delta = "delta method",
             `krinsky-robb` = "Krinsky-Robb simulation")[x$method]
  cat("Expected willingness to pay\n")
  cat("  Price variable    :", x$price, "\n")
  cat("  Interval method   :", label, "\n")
  cat("  Confidence level  :", paste0(100 * x$level, "%"), "\n\n")

  tbl <- x$estimates
  out <- data.frame(
    scenario = tbl$scenario,
    EWTP     = signif(tbl$ewtp, digits),
    SE       = signif(tbl$std_error, digits),
    lower    = signif(tbl$conf_low, digits),
    upper    = signif(tbl$conf_high, digits),
    stringsAsFactors = FALSE
  )
  print(out, row.names = FALSE)
  if (x$n_invalid > 0L) {
    cat("\n", x$n_invalid, " draw(s) with a non-negative price coefficient ",
        "were discarded.\n", sep = "")
  }
  invisible(x)
}


#' @title Coerce expected willingness to pay estimates to a data frame
#'
#' @param x An object of class `fs_ewtp`, from [fs_ewtp()].
#' @param row.names Passed to [as.data.frame()]; normally left as `NULL`.
#' @param optional Passed to [as.data.frame()]; normally left as `FALSE`.
#' @param ... Ignored.
#'
#' @return A data frame with columns `scenario`, `index`, `ewtp`,
#'   `std_error`, `conf_low` and `conf_high`.
#' @export
#' @examples
#' set.seed(1)
#' d <- data.frame(id = rep(1:40, each = 4), q = rbinom(160, 1, 0.5),
#'                 cost = sample(c(100, 300, 500), 160, replace = TRUE))
#' d$y <- rbinom(160, 1, plogis(0.5 + d$q - 0.004 * d$cost))
#' fit <- fs_logit(y ~ q + cost, data = d, cluster = ~ id, compute_me = FALSE)
#' as.data.frame(fs_ewtp(fit, data.frame(q = 0:1), price = "cost"))
as.data.frame.fs_ewtp <- function(x, row.names = NULL, optional = FALSE, ...) {
  x$estimates
}
