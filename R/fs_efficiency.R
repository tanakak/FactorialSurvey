#' @title D- and A-efficiency of a factorial survey design
#'
#' @description
#' Computes the D-efficiency and A-efficiency of a set of vignettes, expressed
#' as a percentage of a hypothetical orthogonal and balanced design with the
#' same number of rows. A value of 100 means the design is as informative as
#' an orthogonal array; an orthogonal array, where one exists, attains 100.
#'
#' Use it to check a design produced by [fs_defficient()] or any design built
#' elsewhere, for example one used in an earlier survey.
#'
#' @param design A data frame with one row per vignette and one column per
#'   attribute. A `block` column, if present, is ignored. An `fs_design`
#'   object from [fs_defficient()] is accepted as well.
#' @param factors An optional named list giving the full set of levels of each
#'   attribute, in the form accepted by [fs_defficient()]. Supplying it is
#'   strongly recommended: if a level never appears in `design`, it can only
#'   be detected when the intended levels are known. When `NULL`, the levels
#'   are taken from `attr(design, "factors")` if present, and otherwise
#'   inferred from the values that occur in `design`.
#' @param model A one-sided formula giving the model the design is meant to
#'   estimate. The default `~ .` covers main effects; use `~ .^2` to include
#'   two-way interactions.
#' @param linear An optional character vector naming numeric attributes that
#'   enter the model as a single linear term rather than as a set of level
#'   effects. Defaults to `attr(design, "linear")` when present.
#'
#' @return A list of class `fs_efficiency` with components:
#'   \describe{
#'     \item{`d_efficiency`}{D-efficiency in percent.}
#'     \item{`a_efficiency`}{A-efficiency in percent.}
#'     \item{`n_vignettes`}{The number of rows, \eqn{N}.}
#'     \item{`n_parameters`}{The number of model parameters, \eqn{p},
#'       including the intercept.}
#'     \item{`estimable`}{`TRUE` when every parameter can be estimated, that
#'       is, when the information matrix is non-singular.}
#'     \item{`missing_levels`}{A named list of levels that never appear in
#'       `design`, with one element per attribute that has any.}
#'   }
#'
#' @details
#' Each attribute is coded so that, over a full factorial, its columns have
#' mean zero, unit mean square and are mutually orthogonal: orthogonal
#' polynomial contrasts scaled by \eqn{\sqrt{k}} for an attribute with
#' \eqn{k} levels, and the standardised value for an attribute listed in
#' `linear`. With this coding an orthogonal and balanced design has
#' information matrix \eqn{X'X/N = I}, and the efficiencies are
#' \deqn{D = 100 \, |X'X/N|^{1/p}, \qquad
#'       A = 100 \, p / \mathrm{tr}\{(X'X/N)^{-1}\},}
#' following Kuhfeld, Tobias and Garratt (1994). Both are zero when the design
#' cannot estimate the model.
#'
#' The D-efficiency is invariant to the choice of coding only up to this
#' normalisation, which is why the value reported by some optimisation
#' routines -- for instance the criterion `D` returned by
#' [AlgDesign::optFederov()], computed on treatment-coded dummies -- is not
#' comparable across designs and is not an efficiency in this sense.
#'
#' When restrictions exclude some combinations, 100 is usually out of reach;
#' the value then measures the cost of the restrictions as well as the
#' quality of the search.
#'
#' @references
#' Kuhfeld, W. F., Tobias, R. D. and Garratt, M. (1994). Efficient
#' experimental design with marketing research applications. *Journal of
#' Marketing Research*, 31(4), 545--557. \doi{10.1177/002224379403100408}
#'
#' @seealso [fs_defficient()], [fs_design_eval()]
#' @export
#' @examples
#' # A 3^4 orthogonal array (L9) is 100 percent efficient
#' l9 <- data.frame(a = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
#'                  b = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
#'                  c = c(1, 2, 3, 2, 3, 1, 3, 1, 2),
#'                  d = c(1, 2, 3, 3, 1, 2, 2, 3, 1))
#' fs_efficiency(l9)
#'
#' # A design that never shows the middle level cannot estimate its effect
#' bad <- l9
#' bad$a[bad$a == 2] <- 3
#' fs_efficiency(bad, factors = list(a = 1:3, b = 1:3, c = 1:3, d = 1:3))
fs_efficiency <- function(design, factors = NULL, model = ~ ., linear = NULL) {
  if (!is.data.frame(design)) {
    stop("`design` must be a data frame.", call. = FALSE)
  }
  if (!inherits(model, "formula") || length(model) != 2L) {
    stop("`model` must be a one-sided formula, such as `~ .` or `~ .^2`.",
         call. = FALSE)
  }
  if (is.null(factors)) {
    factors <- attr(design, "factors")
  }
  if (is.null(linear)) {
    linear <- attr(design, "linear")
  }

  body <- as.data.frame(design)
  body <- body[, setdiff(names(body), "block"), drop = FALSE]
  if (ncol(body) == 0L) {
    stop("`design` contains no attribute columns.", call. = FALSE)
  }

  if (is.null(factors)) {
    factors <- lapply(body, observed_levels)
  } else {
    check_factors(factors)
    absent <- setdiff(names(factors), names(body))
    if (length(absent) > 0L) {
      stop("Attribute(s) not found in `design`: ",
           paste(absent, collapse = ", "), ".", call. = FALSE)
    }
    body <- body[, names(factors), drop = FALSE]
  }
  linear <- check_linear(linear, factors)

  missing_levels <- list()
  for (v in setdiff(names(factors), linear)) {
    gone <- setdiff(as.character(factors[[v]]), as.character(body[[v]]))
    if (length(gone) > 0L) {
      missing_levels[[v]] <- gone
    }
  }

  X <- orthonormal_model_matrix(body, factors, model, linear)
  eff <- efficiency_from_matrix(X)

  out <- list(
    d_efficiency   = eff$d,
    a_efficiency   = eff$a,
    n_vignettes    = nrow(X),
    n_parameters   = ncol(X),
    estimable      = eff$estimable,
    missing_levels = missing_levels
  )
  class(out) <- "fs_efficiency"
  out
}


#' @title Print design efficiencies
#'
#' @param x An object of class `fs_efficiency`, from [fs_efficiency()].
#' @param ... Ignored.
#'
#' @return `x`, invisibly. Called for the side effect of printing.
#' @export
#' @examples
#' print(fs_efficiency(expand.grid(a = 1:2, b = 1:3)))
print.fs_efficiency <- function(x, ...) {
  cat("Design efficiency\n")
  cat("  Vignettes    :", x$n_vignettes, "\n")
  cat("  Parameters   :", x$n_parameters, "\n")
  cat("  D-efficiency :", fmt_pct(x$d_efficiency), "\n")
  cat("  A-efficiency :", fmt_pct(x$a_efficiency), "\n")
  if (!x$estimable) {
    cat("\nThe design cannot estimate every parameter of the model.\n")
  }
  if (length(x$missing_levels) > 0L) {
    cat("\nLevels that never appear:\n")
    for (v in names(x$missing_levels)) {
      cat("  ", v, ": ", paste(x$missing_levels[[v]], collapse = ", "), "\n",
          sep = "")
    }
  }
  invisible(x)
}


## ---- Internal helpers -------------------------------------------------------

#' Levels observed in a column, in a natural order
#' @noRd
observed_levels <- function(x) {
  if (is.factor(x)) {
    lv <- levels(droplevels(x))
    return(lv)
  }
  sort(unique(x))
}

#' Validate a list of attributes and levels
#' @noRd
check_factors <- function(factors) {
  if (!is.list(factors) || length(factors) == 0L) {
    stop("`factors` must be a non-empty list.", call. = FALSE)
  }
  if (is.null(names(factors)) || any(names(factors) == "")) {
    stop("Every element of `factors` must be named.", call. = FALSE)
  }
  if (any(vapply(factors, function(l) length(unique(l)), integer(1L)) < 2L)) {
    stop("Every attribute in `factors` must have at least two levels.",
         call. = FALSE)
  }
  invisible(TRUE)
}

#' Validate the names of attributes to be treated as linear
#' @noRd
check_linear <- function(linear, factors) {
  if (is.null(linear) || length(linear) == 0L) {
    return(character(0L))
  }
  if (!is.character(linear)) {
    stop("`linear` must be a character vector of attribute names.",
         call. = FALSE)
  }
  unknown <- setdiff(linear, names(factors))
  if (length(unknown) > 0L) {
    stop("`linear` names attribute(s) not in `factors`: ",
         paste(unknown, collapse = ", "), ".", call. = FALSE)
  }
  not_num <- linear[!vapply(factors[linear], is.numeric, logical(1L))]
  if (length(not_num) > 0L) {
    stop("Attributes in `linear` must have numeric levels: ",
         paste(not_num, collapse = ", "), ".", call. = FALSE)
  }
  unique(linear)
}

#' Convert attribute columns to the form used for searching and coding
#'
#' Every attribute becomes a factor with the levels in `factors`, except those
#' named in `linear`, which stay numeric.
#' @noRd
as_search_data <- function(df, factors, linear = character(0L)) {
  for (v in names(factors)) {
    if (v %in% linear) {
      df[[v]] <- as.numeric(df[[v]])
    } else {
      df[[v]] <- factor(as.character(df[[v]]),
                        levels = unique(as.character(factors[[v]])))
    }
  }
  df
}

#' Restore attribute columns to the types implied by `factors`
#'
#' Numeric levels come back as numbers; anything else comes back as a factor
#' whose levels follow the order given in `factors`.
#' @noRd
restore_types <- function(df, factors) {
  for (v in names(factors)) {
    lv <- factors[[v]]
    if (is.numeric(lv)) {
      df[[v]] <- as.numeric(as.character(df[[v]]))
    } else {
      df[[v]] <- factor(as.character(df[[v]]),
                        levels = unique(as.character(lv)))
    }
  }
  df
}

#' Model matrix with orthonormal coding
#'
#' Categorical attributes use orthogonal polynomial contrasts scaled by
#' sqrt(k), so that over a balanced full factorial each column has mean zero
#' and unit mean square. Linear attributes are standardised over their levels.
#' @noRd
orthonormal_model_matrix <- function(df, factors, model,
                                     linear = character(0L)) {
  df <- as_search_data(df[, names(factors), drop = FALSE], factors, linear)
  for (v in linear) {
    lv <- as.numeric(unique(factors[[v]]))
    s <- sqrt(mean((lv - mean(lv))^2))
    df[[v]] <- (df[[v]] - mean(lv)) / s
  }
  cat_vars <- setdiff(names(factors), linear)
  contr <- lapply(cat_vars, function(v) {
    k <- nlevels(df[[v]])
    stats::contr.poly(k) * sqrt(k)
  })
  names(contr) <- cat_vars
  stats::model.matrix(model, data = df,
                      contrasts.arg = if (length(contr)) contr else NULL)
}

#' D- and A-efficiency from an orthonormally coded model matrix
#' @noRd
efficiency_from_matrix <- function(X) {
  n <- nrow(X)
  p <- ncol(X)
  M <- crossprod(X) / n
  ev <- eigen(M, symmetric = TRUE, only.values = TRUE)$values
  tol <- max(dim(X)) * max(ev) * .Machine$double.eps * 100
  if (n < p || min(ev) <= tol) {
    return(list(d = 0, a = 0, estimable = FALSE))
  }
  list(
    d = 100 * exp(mean(log(ev))),
    a = 100 * p / sum(1 / ev),
    estimable = TRUE
  )
}

#' Format a percentage for display
#' @noRd
fmt_pct <- function(x, digits = 1) {
  paste0(formatC(x, format = "f", digits = digits), "%")
}
