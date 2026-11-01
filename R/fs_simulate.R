#' @title Simulate responses to a factorial survey
#'
#' @description
#' Generates artificial responses to a design from a model whose coefficients
#' you choose. Because the true values are known, the result is useful for
#' checking that an analysis recovers them, for teaching, and -- through
#' [fs_power()] -- for deciding how many respondents a survey needs.
#'
#' @param design Either an `fs_design` object from [fs_defficient()] (or any
#'   data frame of vignettes with a `block` column), in which case blocks and
#'   presentation orders are allocated with [fs_assign()], or an
#'   `fs_assignment` schedule from [fs_assign()], which is used as it is.
#' @param n_respondents The number of respondents. Required when `design` is
#'   a design; ignored for a schedule.
#' @param coef A named numeric vector of coefficients on the logit scale. The
#'   names must match the columns of the model matrix implied by `formula`,
#'   including `"(Intercept)"`; the error message lists them if they do not.
#' @param formula A one-sided formula giving the systematic part of the
#'   model, such as `~ damage + cost`. Character attributes become factors
#'   with the levels in the design, so their coefficients are named as in
#'   [stats::model.matrix()], for example `"damagesevere"`.
#' @param response The kind of response to generate: `"rating"` (the
#'   default) for a bounded rating, or `"binary"` for a yes/no answer.
#' @param phi The precision of the beta distribution for ratings. Larger
#'   values give less noise around the mean. Ignored for binary responses.
#' @param sd_respondent The standard deviation of a normally distributed
#'   respondent-specific shift on the logit scale. It makes the ratings of one
#'   respondent correlated, as they are in real data. Use `0` for independent
#'   responses.
#' @param scale A numeric vector of length two giving the bounds of the
#'   rating scale.
#' @param round A logical value. If `TRUE` (the default), ratings are rounded
#'   to whole numbers, as on a 0 to 10 scale.
#' @param name The name of the response column.
#' @param seed A single number making the simulation reproducible, or `NULL`.
#'   The caller's random number state is restored on exit.
#'
#' @return The schedule as a data frame (class `fs_assignment` retained) with
#'   the response column added. The coefficients, `phi`, `sd_respondent` and
#'   `scale` used are attached as the `truth` attribute.
#'
#' @details
#' For respondent \eqn{i} and vignette \eqn{j} the linear predictor is
#' \eqn{\eta_{ij} = x_{ij}'\beta + u_i} with
#' \eqn{u_i \sim N(0, \sigma_u^2)}, and \eqn{\mu_{ij} = \Lambda(\eta_{ij})}.
#' A rating is drawn from a beta distribution with mean \eqn{\mu_{ij}} and
#' precision \eqn{\phi} and mapped onto `scale`; a binary response is drawn
#' from a Bernoulli distribution with probability \eqn{\mu_{ij}}.
#'
#' Because of the respondent shift, coefficients estimated by [fs_betareg()]
#' or [fs_logit()], which do not model it, are attenuated towards zero
#' relative to `coef`; ratios such as willingness to pay are much less
#' affected.
#'
#' @seealso [fs_power()], which repeats the simulation to estimate power.
#'
#' @export
#' @examples
#' design <- fs_defficient(
#'   list(damage = c("minor", "severe"), cost = c(100, 300, 500)),
#'   n_vignettes = 6, n_blocks = 2, seed = 1, verbose = FALSE
#' )
#' sim <- fs_simulate_ratings(
#'   design, n_respondents = 60,
#'   coef = c("(Intercept)" = 1, damagesevere = 0.8, cost = -0.004),
#'   formula = ~ damage + cost, seed = 1
#' )
#' head(sim)
#'
#' fit <- fs_betareg(rating ~ damage + cost, data = sim,
#'                   cluster = ~ respondent, compute_me = FALSE)
#' fs_mwtp(fit, price = "cost")   # true value: 0.8 / 0.004 = 200
fs_simulate_ratings <- function(design,
                                n_respondents = NULL,
                                coef,
                                formula,
                                response = c("rating", "binary"),
                                phi = 10,
                                sd_respondent = 0.5,
                                scale = c(0, 10),
                                round = TRUE,
                                name = if (response == "rating") {
                                  "rating"
                                } else {
                                  "response"
                                },
                                seed = NULL) {
  response <- match.arg(response)
  if (!is.data.frame(design)) {
    stop("`design` must be a data frame, an fs_design or an fs_assignment.",
         call. = FALSE)
  }
  if (!inherits(formula, "formula") || length(formula) != 2L) {
    stop("`formula` must be a one-sided formula, such as `~ a + b`.",
         call. = FALSE)
  }
  if (!is.numeric(coef) || is.null(names(coef)) || anyNA(coef)) {
    stop("`coef` must be a named numeric vector without missing values.",
         call. = FALSE)
  }
  check_positive(phi, "phi")
  if (!is.numeric(sd_respondent) || length(sd_respondent) != 1L ||
      is.na(sd_respondent) || sd_respondent < 0) {
    stop("`sd_respondent` must be a single non-negative number.",
         call. = FALSE)
  }
  if (!is.numeric(scale) || length(scale) != 2L || anyNA(scale) ||
      scale[2L] <= scale[1L]) {
    stop("`scale` must be a numeric vector of length two with ",
         "scale[1] < scale[2].", call. = FALSE)
  }
  check_flag(round)
  if (!is.character(name) || length(name) != 1L || !nzchar(name)) {
    stop("`name` must be a single non-empty string.", call. = FALSE)
  }

  check_seed(seed)
  if (!is.null(seed)) {
    old_rng <- rng_state()
    on.exit(restore_rng(old_rng), add = TRUE)
    set.seed(seed)
  }

  if (inherits(design, "fs_assignment")) {
    schedule <- design
  } else {
    if (is.null(n_respondents)) {
      stop("`n_respondents` is required when `design` is a design rather ",
           "than an fs_assignment schedule.", call. = FALSE)
    }
    check_count(n_respondents)
    schedule <- suppressMessages(fs_assign(design, n_respondents))
  }
  if (!"respondent" %in% names(schedule)) {
    stop("The schedule must have a `respondent` column.", call. = FALSE)
  }

  X <- simulation_matrix(schedule, formula, design)
  missing_coef <- setdiff(colnames(X), names(coef))
  extra_coef <- setdiff(names(coef), colnames(X))
  if (length(missing_coef) > 0L || length(extra_coef) > 0L) {
    stop("The names of `coef` must match the model matrix columns: ",
         paste0("\"", colnames(X), "\"", collapse = ", "), ".",
         call. = FALSE)
  }

  resp <- schedule$respondent
  ids <- unique(resp)
  u <- stats::rnorm(length(ids), sd = sd_respondent)[match(resp, ids)]
  mu <- stats::plogis(drop(X %*% coef[colnames(X)]) + u)

  if (response == "rating") {
    y01 <- stats::rbeta(length(mu), mu * phi, (1 - mu) * phi)
    y <- scale[1L] + y01 * (scale[2L] - scale[1L])
    if (round) {
      y <- base::round(y)
    }
  } else {
    y <- stats::rbinom(length(mu), 1L, mu)
  }

  schedule[[name]] <- y
  attr(schedule, "truth") <- list(
    coef = coef[colnames(X)], phi = phi, sd_respondent = sd_respondent,
    scale = scale, response = response
  )
  schedule
}


#' Model matrix for simulation, with character attributes as factors
#' @noRd
simulation_matrix <- function(schedule, formula, design) {
  vars <- all.vars(formula)
  absent <- setdiff(vars, names(schedule))
  if (length(absent) > 0L) {
    stop("Variable(s) in `formula` not found in the design: ",
         paste(absent, collapse = ", "), ".", call. = FALSE)
  }
  df <- as.data.frame(schedule)[, vars, drop = FALSE]
  factors <- attr(design, "factors")
  for (v in vars) {
    if (is.character(df[[v]])) {
      lv <- if (!is.null(factors[[v]])) {
        unique(as.character(factors[[v]]))
      } else {
        unique(df[[v]])
      }
      df[[v]] <- factor(df[[v]], levels = lv)
    }
  }
  stats::model.matrix(formula, data = df)
}

#' Check that a value is a single positive number
#' @noRd
check_positive <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x <= 0) {
    stop("`", arg, "` must be a single positive number.", call. = FALSE)
  }
  invisible(TRUE)
}
