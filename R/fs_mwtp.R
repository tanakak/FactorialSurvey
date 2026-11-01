#' @title Marginal willingness to pay from a fitted model
#'
#' @description
#' Converts a pair of estimated coefficients into a marginal willingness to
#' pay (MWTP), the amount of the price attribute a respondent would trade for
#' a one-unit improvement in another attribute:
#' \deqn{MWTP = -\beta_{attribute} / \beta_{price}.}
#'
#' Because MWTP is a ratio of two estimates its sampling distribution is
#' skewed, and more so the closer the price coefficient sits to zero. Three
#' interval methods are therefore offered, and it is worth comparing them
#' before reporting a result.
#'
#' @param object A fitted model: an `fs_betareg` object from [fs_betareg()]
#'   (whose cluster-robust covariance matrix, if any, is used),
#'   a [betareg::betareg()] fit, or any model for which [coef()] and [vcov()]
#'   return a named vector and a conformable matrix.
#' @param variable The name of the attribute coefficient, as it appears in
#'   `coef(object)`. If `NULL` (the default), every coefficient other than the
#'   intercept, the price coefficient and the precision parameter is used, and
#'   a table of MWTP values is returned.
#' @param price The name of the price coefficient.
#' @param method The interval method. One of `"delta"` (the delta method,
#'   the default), `"krinsky-robb"` (simulation from the estimated sampling
#'   distribution of the coefficients) or `"fieller"` (Fieller's theorem,
#'   which respects the skewness of a ratio).
#' @param level The confidence level. Defaults to `0.95`.
#' @param n_sim The number of draws used by `method = "krinsky-robb"`.
#' @param seed A single number making the Krinsky-Robb draws reproducible, or
#'   `NULL`. The caller's random number state is restored on exit.
#'
#' @return An object of class `fs_mwtp`, a list with components:
#'   \describe{
#'     \item{`estimates`}{A data frame with one row per attribute and columns
#'       `variable`, `mwtp`, `std_error`, `conf_low` and `conf_high`.}
#'     \item{`price`}{The name of the price coefficient.}
#'     \item{`method`}{The interval method used.}
#'     \item{`level`}{The confidence level.}
#'     \item{`draws`}{The simulated MWTP values, for
#'       `method = "krinsky-robb"`; otherwise `NULL`.}
#'   }
#'
#' @details
#' The delta method takes a first-order Taylor expansion of the ratio and
#' yields a symmetric interval. It is quick and familiar but understates
#' skewness, and can produce implausible intervals when the price coefficient
#' is imprecisely estimated.
#'
#' Krinsky and Robb (1986) draw repeatedly from a multivariate normal with the
#' estimated mean and covariance of the coefficients, form the ratio in each
#' draw, and read the interval off the empirical quantiles. This captures the
#' skewness and is the usual recommendation in the stated preference
#' literature.
#'
#' Fieller's theorem inverts the test that the ratio equals a given value. It
#' is exact under normality of the coefficients but the resulting region is
#' unbounded when the price coefficient is not clearly distinguishable from
#' zero; in that case the interval is returned as `NA` with a warning.
#'
#' The coefficients enter on the scale of the linear predictor. For a beta
#' regression with a logit link the ratio is therefore a ratio of effects on
#' the latent scale, which is the conventional definition of MWTP but is not
#' the same as the ratio of average marginal effects on the response scale.
#'
#' @references
#' Krinsky, I. and Robb, A. L. (1986). On approximating the statistical
#' properties of elasticities. *Review of Economics and Statistics*, 68(4),
#' 715--719. \doi{10.2307/1924536}
#'
#' Fieller, E. C. (1954). Some problems in interval estimation. *Journal of
#' the Royal Statistical Society, Series B*, 16(2), 175--185.
#' \doi{10.1111/j.2517-6161.1954.tb00159.x}
#'
#' @seealso [fs_betareg()]
#'
#' @importFrom stats coef vcov qnorm quantile rnorm
#' @export
#' @examples
#' set.seed(123)
#' n <- 200   # 50 respondents x 4 vignettes
#' d <- data.frame(
#'   id      = rep(1:50, each = 4),
#'   quality = sample(0:1, n, replace = TRUE),
#'   speed   = rnorm(n),
#'   price   = runif(n, 100, 1000)
#' )
#' d$rating <- 10 * plogis(0.8 * d$quality + 0.3 * d$speed -
#'                           0.0015 * d$price + rnorm(n, sd = 0.5))
#'
#' fit <- fs_betareg(rating ~ quality + speed + price, data = d,
#'                   cluster = ~ id, compute_me = FALSE)
#'
#' # All attributes at once
#' fs_mwtp(fit, price = "price")
#'
#' # A single attribute, with a simulation-based interval
#' fs_mwtp(fit, variable = "quality", price = "price",
#'         method = "krinsky-robb", n_sim = 500, seed = 1)
fs_mwtp <- function(object,
                    variable = NULL,
                    price,
                    method = c("delta", "krinsky-robb", "fieller"),
                    level = 0.95,
                    n_sim = 5000,
                    seed = NULL) {

  method <- match.arg(method)

  if (missing(price) || !is.character(price) || length(price) != 1L) {
    stop("`price` must be the name of a single coefficient.", call. = FALSE)
  }
  if (!is.numeric(level) || length(level) != 1L || level <= 0 || level >= 1) {
    stop("`level` must be a single number strictly between 0 and 1.",
         call. = FALSE)
  }
  check_count(n_sim)

  ## For an fs_betareg object, vcov() returns the matrix used for inference,
  ## which is cluster-robust when the model was fitted with `cluster`.
  beta <- stats::coef(object)
  vc <- stats::vcov(object)
  if (is.null(names(beta))) {
    stop("`coef(object)` must return a named vector.", call. = FALSE)
  }
  if (!identical(length(beta), nrow(vc))) {
    ## betareg returns the precision parameter in coef() but callers may pass
    ## a model whose vcov() covers the mean equation only.
    keep <- intersect(names(beta), rownames(vc))
    beta <- beta[keep]
    vc <- vc[keep, keep, drop = FALSE]
  }

  if (!price %in% names(beta)) {
    stop("Price coefficient '", price, "' not found. Available coefficients: ",
         paste(names(beta), collapse = ", "), call. = FALSE)
  }

  if (is.null(variable)) {
    drop <- c(price, "(Intercept)", "(phi)", "(Intercept).1",
              "(phi)_(Intercept)")
    variable <- setdiff(names(beta), drop)
    variable <- variable[!grepl("^\\(phi\\)", variable)]
    if (length(variable) == 0L) {
      stop("No attribute coefficients left once the intercept, the price ",
           "coefficient and the precision parameter are set aside.",
           call. = FALSE)
    }
  }
  unknown <- setdiff(variable, names(beta))
  if (length(unknown) > 0L) {
    stop("Coefficient(s) not found: ", paste(unknown, collapse = ", "),
         ". Available coefficients: ", paste(names(beta), collapse = ", "),
         call. = FALSE)
  }
  if (price %in% variable) {
    stop("`variable` must not include the price coefficient.", call. = FALSE)
  }

  draws <- NULL
  if (method == "krinsky-robb") {
    check_seed(seed)
    if (!is.null(seed)) {
      old_rng <- rng_state()
      on.exit(restore_rng(old_rng), add = TRUE)
      set.seed(seed)
    }
    draws_mat <- rmvnorm_chol(n_sim, beta, vc)
    draws <- matrix(NA_real_, nrow = n_sim, ncol = length(variable),
                    dimnames = list(NULL, variable))
    for (v in variable) {
      draws[, v] <- -draws_mat[, v] / draws_mat[, price]
    }
  }

  rows <- lapply(variable, function(v) {
    mwtp_one(v, price, beta, vc, method, level,
             draws = if (is.null(draws)) NULL else draws[, v])
  })
  estimates <- do.call(rbind, rows)
  rownames(estimates) <- NULL

  out <- list(
    estimates = estimates,
    price     = price,
    method    = method,
    level     = level,
    draws     = draws
  )
  class(out) <- "fs_mwtp"
  out
}


#' Compute MWTP and its interval for one attribute
#'
#' @param v Name of the attribute coefficient.
#' @param price Name of the price coefficient.
#' @param beta Named coefficient vector.
#' @param vc Covariance matrix of `beta`.
#' @param method Interval method.
#' @param level Confidence level.
#' @param draws Simulated MWTP values, for the Krinsky-Robb method.
#' @return A one-row data frame.
#' @noRd
mwtp_one <- function(v, price, beta, vc, method, level, draws = NULL) {
  b1 <- unname(beta[v])
  b2 <- unname(beta[price])
  v11 <- vc[v, v]
  v22 <- vc[price, price]
  v12 <- vc[v, price]

  estimate <- -b1 / b2

  ## Delta-method standard error of -b1 / b2.
  ## Gradient: d/db1 = -1 / b2, d/db2 = b1 / b2^2.
  g1 <- -1 / b2
  g2 <- b1 / b2^2
  variance <- g1^2 * v11 + 2 * g1 * g2 * v12 + g2^2 * v22
  std_error <- sqrt(max(variance, 0))

  alpha <- 1 - level
  z <- stats::qnorm(1 - alpha / 2)

  if (method == "delta") {
    lo <- estimate - z * std_error
    hi <- estimate + z * std_error
  } else if (method == "krinsky-robb") {
    qs <- stats::quantile(draws, probs = c(alpha / 2, 1 - alpha / 2),
                          names = FALSE, na.rm = TRUE)
    lo <- qs[1L]
    hi <- qs[2L]
    std_error <- stats::sd(draws, na.rm = TRUE)
  } else {
    fi <- fieller_interval(b1, b2, v11, v22, v12, z)
    lo <- fi[1L]
    hi <- fi[2L]
    if (anyNA(fi)) {
      warning("Fieller's interval for '", v, "' is unbounded: the price ",
              "coefficient is not clearly separated from zero. Consider ",
              "method = \"krinsky-robb\".", call. = FALSE)
    }
  }

  data.frame(
    variable  = v,
    mwtp      = estimate,
    std_error = std_error,
    conf_low  = lo,
    conf_high = hi,
    stringsAsFactors = FALSE
  )
}


#' Fieller interval for the ratio -b1 / b2
#'
#' @param b1,b2 Coefficient estimates.
#' @param v11,v22,v12 Elements of the covariance matrix.
#' @param z Normal quantile for the requested confidence level.
#' @return A numeric vector of length two, possibly `c(NA, NA)` when the
#'   region is unbounded.
#' @noRd
fieller_interval <- function(b1, b2, v11, v22, v12, z) {
  a <- b2^2 - z^2 * v22
  b <- -2 * (b1 * b2 - z^2 * v12)
  cc <- b1^2 - z^2 * v11

  if (a <= 0) {
    return(c(NA_real_, NA_real_))
  }
  disc <- b^2 - 4 * a * cc
  if (disc < 0) {
    return(c(NA_real_, NA_real_))
  }
  roots <- (-b + c(-1, 1) * sqrt(disc)) / (2 * a)
  ## roots bound b1 / b2; MWTP is minus that ratio.
  sort(-roots)
}


#' Draw from a multivariate normal using a Cholesky factor
#'
#' Avoids a dependency on \pkg{MASS}. Falls back to an eigen decomposition
#' when the covariance matrix is not numerically positive definite.
#'
#' @param n Number of draws.
#' @param mu Mean vector.
#' @param sigma Covariance matrix.
#' @return An `n` by `length(mu)` matrix with column names taken from `mu`.
#' @noRd
rmvnorm_chol <- function(n, mu, sigma) {
  p <- length(mu)
  factor <- tryCatch(chol(sigma), error = function(e) NULL)
  if (is.null(factor)) {
    ev <- eigen(sigma, symmetric = TRUE)
    values <- pmax(ev$values, 0)
    factor <- t(ev$vectors %*% diag(sqrt(values), p, p))
  }
  z <- matrix(stats::rnorm(n * p), nrow = n, ncol = p)
  out <- z %*% factor
  out <- sweep(out, 2L, mu, "+")
  colnames(out) <- names(mu)
  out
}


#' @title Print marginal willingness to pay estimates
#'
#' @param x An object of class `fs_mwtp`, from [fs_mwtp()].
#' @param digits Number of significant digits.
#' @param ... Ignored.
#'
#' @return `x`, invisibly. Called for the side effect of printing.
#' @export
#' @examples
#' set.seed(1)
#' d <- data.frame(id = rep(1:40, each = 3), x = rnorm(120),
#'                 price = runif(120, 100, 900))
#' d$rating <- 10 * plogis(0.7 * d$x - 0.002 * d$price + rnorm(120, sd = 0.4))
#' fit <- fs_betareg(rating ~ x + price, data = d, cluster = ~ id,
#'                   compute_me = FALSE)
#' print(fs_mwtp(fit, price = "price"))
print.fs_mwtp <- function(x, digits = 4, ...) {
  label <- c(delta = "delta method",
             `krinsky-robb` = "Krinsky-Robb simulation",
             fieller = "Fieller's theorem")[x$method]

  cat("Marginal willingness to pay\n")
  cat("  Price coefficient :", x$price, "\n")
  cat("  Interval method   :", label, "\n")
  cat("  Confidence level  :", paste0(100 * x$level, "%"), "\n\n")

  tbl <- x$estimates
  out <- data.frame(
    variable = tbl$variable,
    MWTP     = round(tbl$mwtp, digits),
    SE       = round(tbl$std_error, digits),
    lower    = round(tbl$conf_low, digits),
    upper    = round(tbl$conf_high, digits),
    stringsAsFactors = FALSE
  )
  print(out, row.names = FALSE)
  invisible(x)
}


#' @title Coerce marginal willingness to pay estimates to a data frame
#'
#' @param x An object of class `fs_mwtp`, from [fs_mwtp()].
#' @param row.names Passed to [as.data.frame()]; normally left as `NULL`.
#' @param optional Passed to [as.data.frame()]; normally left as `FALSE`.
#' @param ... Ignored.
#'
#' @return A data frame with columns `variable`, `mwtp`, `std_error`,
#'   `conf_low` and `conf_high`.
#' @export
#' @examples
#' set.seed(1)
#' d <- data.frame(id = rep(1:40, each = 3), x = rnorm(120),
#'                 price = runif(120, 100, 900))
#' d$rating <- 10 * plogis(0.7 * d$x - 0.002 * d$price + rnorm(120, sd = 0.4))
#' fit <- fs_betareg(rating ~ x + price, data = d, cluster = ~ id,
#'                   compute_me = FALSE)
#' as.data.frame(fs_mwtp(fit, price = "price"))
as.data.frame.fs_mwtp <- function(x, row.names = NULL, optional = FALSE, ...) {
  x$estimates
}
