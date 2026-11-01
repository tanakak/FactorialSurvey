#' @title Power and precision of a factorial survey by simulation
#'
#' @description
#' Answers the two questions asked when a factorial survey is planned: with
#' this design and this many respondents, how likely is each attribute effect
#' to be detected, and how precisely will willingness to pay be estimated?
#' Data are simulated repeatedly with [fs_simulate_ratings()] and analysed
#' exactly as real data would be -- [fs_betareg()] or [fs_logit()] with
#' standard errors clustered by respondent -- and the results are summarised.
#'
#' @inheritParams fs_simulate_ratings
#' @param n_respondents A vector of sample sizes to evaluate.
#' @param price Optional name of the price variable. When given, the
#'   precision of the marginal willingness to pay for each other attribute is
#'   also reported.
#' @param alpha The significance level of the two-sided tests.
#' @param n_sim The number of simulated surveys per sample size.
#' @param seed A single number making the simulation reproducible, or `NULL`.
#'   The caller's random number state is restored on exit.
#' @param verbose A logical value. If `TRUE`, progress is reported through
#'   [message()].
#'
#' @return An object of class `fs_power`, a data frame with one row per
#'   sample size and model term, and the columns:
#'   \describe{
#'     \item{`n_respondents`}{The sample size.}
#'     \item{`term`}{The model term (a column of the model matrix).}
#'     \item{`coef`}{The true coefficient on the logit scale.}
#'     \item{`power`}{The share of simulated surveys in which the term was
#'       significant at level `alpha`.}
#'     \item{`mwtp`}{The true marginal willingness to pay, when `price` is
#'       given.}
#'     \item{`mwtp_halfwidth`}{The average half-width of the delta-method
#'       confidence interval for the MWTP, in units of the price variable.}
#'     \item{`mwtp_relative`}{That half-width as a share of the true MWTP:
#'       0.2 means the interval is roughly the estimate plus or minus 20
#'       percent.}
#'   }
#'   The settings, the number of failed fits and `n_sim` are attached as
#'   attributes.
#'
#' @details
#' Each simulated survey uses the same design and allocation rules as
#' [fs_assign()]. Effects are tested with cluster-robust Wald tests, so the
#' respondent-level correlation introduced by `sd_respondent` is taken into
#' account, as it would be in the real analysis.
#'
#' Because coefficients estimated without a respondent effect are attenuated
#' when `sd_respondent > 0`, power refers to detecting the effect that the
#' analysis actually estimates. The true MWTP is the ratio of the supplied
#' coefficients, which the attenuation leaves almost unchanged.
#'
#' Choosing `coef` is the hard part. Values from a pilot study or a similar
#' published survey are best. Run time grows with `n_sim` times the number of
#' sample sizes; 200 to 500 simulations are usually enough to compare
#' designs.
#'
#' @seealso [fs_simulate_ratings()], [fs_defficient()]
#'
#' @export
#' @examples
#' design <- fs_defficient(
#'   list(damage = c("minor", "severe"), cost = c(100, 300, 500)),
#'   n_vignettes = 6, n_blocks = 2, seed = 1, verbose = FALSE
#' )
#' pw <- fs_power(design, n_respondents = c(50, 150),
#'                coef = c("(Intercept)" = 1, damagesevere = 0.4,
#'                         cost = -0.003),
#'                formula = ~ damage + cost, price = "cost",
#'                n_sim = 20, seed = 1, verbose = FALSE)
#' pw
#' \donttest{
#' plot(pw)
#' }
fs_power <- function(design,
                     n_respondents,
                     coef,
                     formula,
                     price = NULL,
                     response = c("rating", "binary"),
                     phi = 10,
                     sd_respondent = 0.5,
                     scale = c(0, 10),
                     round = TRUE,
                     alpha = 0.05,
                     n_sim = 200,
                     seed = NULL,
                     verbose = TRUE) {
  response <- match.arg(response)
  if (!is.numeric(n_respondents) || length(n_respondents) == 0L ||
      anyNA(n_respondents) || any(n_respondents < 2) ||
      any(n_respondents != base::round(n_respondents))) {
    stop("`n_respondents` must be whole numbers of at least 2.",
         call. = FALSE)
  }
  n_respondents <- sort(unique(as.integer(n_respondents)))
  if (!is.numeric(alpha) || length(alpha) != 1L || alpha <= 0 || alpha >= 1) {
    stop("`alpha` must be a single number strictly between 0 and 1.",
         call. = FALSE)
  }
  check_count(n_sim)
  check_flag(verbose)
  terms <- setdiff(names(coef), "(Intercept)")
  if (!is.null(price)) {
    if (!is.character(price) || length(price) != 1L ||
        !price %in% names(coef)) {
      stop("`price` must name one of the coefficients in `coef`.",
           call. = FALSE)
    }
    if (coef[[price]] >= 0) {
      stop("The price coefficient in `coef` must be negative.",
           call. = FALSE)
    }
  }

  check_seed(seed)
  if (!is.null(seed)) {
    old_rng <- rng_state()
    on.exit(restore_rng(old_rng), add = TRUE)
    set.seed(seed)
  }

  ## Validate the coefficient names once, with a clear error, before looping.
  invisible(fs_simulate_ratings(design, n_respondents = n_respondents[1L],
                                coef = coef, formula = formula,
                                response = response, phi = phi,
                                sd_respondent = sd_respondent, scale = scale,
                                round = round))

  y_name <- ".fs_y"
  fit_formula <- stats::as.formula(
    paste(y_name, "~", paste(deparse(formula[[2L]]), collapse = " ")),
    env = environment(formula)
  )
  crit <- stats::qnorm(1 - alpha / 2)
  z95 <- stats::qnorm(0.975)
  wtp_terms <- if (is.null(price)) character(0L) else setdiff(terms, price)

  results <- list()
  n_failed <- 0L
  for (n in n_respondents) {
    say(verbose, "Simulating ", n_sim, " surveys with ", n, " respondents ...")
    sig <- matrix(NA, nrow = n_sim, ncol = length(terms),
                  dimnames = list(NULL, terms))
    hw <- matrix(NA_real_, nrow = n_sim, ncol = length(wtp_terms),
                 dimnames = list(NULL, wtp_terms))
    for (s in seq_len(n_sim)) {
      sim <- fs_simulate_ratings(design, n_respondents = n, coef = coef,
                                 formula = formula, response = response,
                                 phi = phi, sd_respondent = sd_respondent,
                                 scale = scale, round = round,
                                 name = y_name)
      sim <- as.data.frame(sim)
      fit <- tryCatch(
        suppressWarnings(
          if (response == "rating") {
            fs_betareg(fit_formula, data = sim, scale = scale,
                       cluster = ~ respondent, compute_me = FALSE)
          } else {
            fs_logit(fit_formula, data = sim, cluster = ~ respondent,
                     compute_me = FALSE)
          }
        ),
        error = function(e) NULL
      )
      if (is.null(fit)) {
        n_failed <- n_failed + 1L
        next
      }
      b <- stats::coef(fit)
      V <- stats::vcov(fit)
      if (!all(terms %in% names(b))) {
        n_failed <- n_failed + 1L
        next
      }
      se <- sqrt(diag(V))[terms]
      sig[s, ] <- abs(b[terms] / se) > crit
      for (v in wtp_terms) {
        b1 <- b[[v]]
        b2 <- b[[price]]
        g <- c(-1 / b2, b1 / b2^2)
        idx <- c(v, price)
        hw[s, v] <- z95 * sqrt(max(drop(t(g) %*% V[idx, idx] %*% g), 0))
      }
    }
    rows <- data.frame(
      n_respondents = n,
      term          = terms,
      coef          = unname(coef[terms]),
      power         = unname(colMeans(sig, na.rm = TRUE)),
      stringsAsFactors = FALSE
    )
    if (!is.null(price)) {
      true_wtp <- ifelse(rows$term == price, NA_real_,
                         -coef[rows$term] / coef[[price]])
      half <- rep(NA_real_, nrow(rows))
      half[match(wtp_terms, rows$term)] <- colMeans(hw, na.rm = TRUE)
      rows$mwtp <- unname(true_wtp)
      rows$mwtp_halfwidth <- half
      rows$mwtp_relative <- unname(half / abs(true_wtp))
    }
    results[[length(results) + 1L]] <- rows
  }

  out <- do.call(rbind, results)
  rownames(out) <- NULL
  if (n_failed > 0L) {
    warning(n_failed, " simulated fit(s) failed and were skipped.",
            call. = FALSE)
  }
  attr(out, "n_sim") <- n_sim
  attr(out, "n_failed") <- n_failed
  attr(out, "alpha") <- alpha
  attr(out, "settings") <- list(response = response, phi = phi,
                                sd_respondent = sd_respondent, scale = scale,
                                price = price)
  class(out) <- c("fs_power", "data.frame")
  out
}


#' @title Print a power analysis
#'
#' @param x An object of class `fs_power`, from [fs_power()].
#' @param digits Number of digits.
#' @param ... Ignored.
#'
#' @return `x`, invisibly. Called for the side effect of printing.
#' @export
#' @examples
#' design <- fs_defficient(list(a = c("x", "y"), cost = c(1, 2, 3)),
#'                         n_vignettes = 6, seed = 1, verbose = FALSE)
#' print(fs_power(design, 40, coef = c("(Intercept)" = 0, ay = 0.5,
#'                                     cost = -0.3),
#'                formula = ~ a + cost, n_sim = 10, seed = 1,
#'                verbose = FALSE))
print.fs_power <- function(x, digits = 2, ...) {
  cat("Power analysis for a factorial survey\n")
  st <- attr(x, "settings")
  cat("  Response      :", st$response, "\n")
  cat("  Simulations   :", attr(x, "n_sim"), "per sample size\n")
  cat("  Significance  :", attr(x, "alpha"), "(two-sided, clustered SE)\n")
  if (attr(x, "n_failed") > 0L) {
    cat("  Failed fits   :", attr(x, "n_failed"), "\n")
  }
  cat("\n")
  body <- as.data.frame(x)
  body$power <- round(body$power, digits)
  if (!is.null(body$mwtp)) {
    body$mwtp <- signif(body$mwtp, 4)
    body$mwtp_halfwidth <- signif(body$mwtp_halfwidth, 3)
    body$mwtp_relative <- round(body$mwtp_relative, digits)
  }
  print(body, row.names = FALSE)
  invisible(x)
}


#' @title Plot a power analysis
#'
#' @description
#' Draws power against the number of respondents, one panel per model term,
#' with a dashed line at 0.8.
#'
#' @param x An object of class `fs_power`, from [fs_power()].
#' @param ... Ignored.
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' design <- fs_defficient(list(a = c("x", "y"), cost = c(1, 2, 3)),
#'                         n_vignettes = 6, seed = 1, verbose = FALSE)
#' pw <- fs_power(design, c(20, 60), coef = c("(Intercept)" = 0, ay = 0.5,
#'                                            cost = -0.3),
#'                formula = ~ a + cost, n_sim = 10, seed = 1,
#'                verbose = FALSE)
#' plot(pw)
plot.fs_power <- function(x, ...) {
  df <- as.data.frame(x)
  df$term <- factor(df$term, levels = unique(df$term))
  ink <- "#1F2328"
  ggplot2::ggplot(df, ggplot2::aes(x = .data$n_respondents, y = .data$power)) +
    ggplot2::geom_hline(yintercept = 0.8, linetype = "dashed",
                        colour = "#8C959F", linewidth = 0.4) +
    ggplot2::geom_line(colour = ink, linewidth = 0.6) +
    ggplot2::geom_point(colour = ink, size = 2.2) +
    ggplot2::facet_wrap(~ term) +
    ggplot2::scale_y_continuous(limits = c(0, 1),
                                breaks = seq(0, 1, by = 0.2)) +
    ggplot2::labs(x = "Number of respondents", y = "Power") +
    theme_fs() +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_line(colour = "#EAEEF2",
                                                 linewidth = 0.4),
      panel.spacing = ggplot2::unit(12, "pt")
    )
}
