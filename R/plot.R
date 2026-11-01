#' @title Plot estimates with confidence intervals
#'
#' @description
#' Draws a forest plot -- one point and interval per row -- of willingness to
#' pay or of the effects of the attributes. Each method returns a
#' [ggplot2::ggplot()] object, so titles, labels and themes can be changed
#' with the usual `+` syntax.
#'
#' @param x An object of class `fs_mwtp` (from [fs_mwtp()]), `fs_ewtp` (from
#'   [fs_ewtp()]), `fs_betareg` (from [fs_betareg()]) or `fs_logit` (from
#'   [fs_logit()]).
#' @param type For model fits only: `"ame"` plots average marginal effects --
#'   on the rating scale for `fs_betareg`, on the probability scale for
#'   `fs_logit` -- and requires that they were computed when the model was
#'   fitted; `"coef"` plots the coefficients of the mean equation on the logit
#'   scale with Wald intervals from `vcov(x)`. The default uses marginal
#'   effects when they are available and coefficients otherwise.
#' @param level For `type = "coef"`, the confidence level of the intervals.
#' @param ... Ignored.
#'
#' @return A `ggplot` object.
#'
#' @details
#' Rows appear from top to bottom in the order of the estimates. A dashed
#' vertical line marks zero. The plots use a single dark colour and no fill,
#' so they reproduce in black and white.
#'
#' @name fs_plot
#' @examples
#' set.seed(1)
#' n <- 240
#' d <- data.frame(id      = rep(1:60, each = 4),
#'                 quality = rbinom(n, 1, 0.5),
#'                 speed   = rbinom(n, 1, 0.5),
#'                 price   = sample(c(100, 300, 500), n, replace = TRUE))
#' d$rating <- round(10 * plogis(0.8 * d$quality + 0.4 * d$speed -
#'                                 0.004 * d$price + rnorm(n, sd = 0.5)))
#' fit <- fs_betareg(rating ~ quality + speed + price, data = d,
#'                   cluster = ~ id, compute_me = FALSE)
#'
#' plot(fs_mwtp(fit, price = "price"))
#' plot(fit, type = "coef")
#' plot(fs_ewtp(fit, data.frame(quality = c(0, 1, 1), speed = c(0, 0, 1),
#'                              row.names = c("none", "quality", "both")),
#'              price = "price")) +
#'   ggplot2::labs(x = "Expected WTP (yen)")
NULL

#' @rdname fs_plot
#' @export
plot.fs_mwtp <- function(x, ...) {
  e <- x$estimates
  forest_plot(e$variable, e$mwtp, e$conf_low, e$conf_high,
              xlab = paste0("Marginal willingness to pay (units of ",
                            x$price, ")"))
}

#' @rdname fs_plot
#' @export
plot.fs_ewtp <- function(x, ...) {
  e <- x$estimates
  forest_plot(e$scenario, e$ewtp, e$conf_low, e$conf_high,
              xlab = paste0("Expected willingness to pay (units of ",
                            x$price, ")"))
}

#' @rdname fs_plot
#' @export
plot.fs_betareg <- function(x, type = c("auto", "ame", "coef"),
                            level = 0.95, ...) {
  type <- match.arg(type)
  me <- x$marginal_effects_rescaled
  if (type == "auto") {
    type <- if (is.null(me)) "coef" else "ame"
  }
  if (type == "ame") {
    if (is.null(me)) {
      stop("Marginal effects were not computed. Refit with ",
           "`compute_me = TRUE` (requires the 'margins' package), or use ",
           "`type = \"coef\"`.", call. = FALSE)
    }
    return(forest_plot(as.character(me$factor), me$AME, me$lower, me$upper,
                       xlab = paste0("Average marginal effect (rating scale ",
                                     x$scale[1L], " to ", x$scale[2L], ")")))
  }
  beta <- stats::coef(x$model, model = "mean")
  coef_plot(beta, stats::vcov(x), level)
}

#' @rdname fs_plot
#' @export
plot.fs_logit <- function(x, type = c("auto", "ame", "coef"),
                          level = 0.95, ...) {
  type <- match.arg(type)
  me <- x$marginal_effects
  if (type == "auto") {
    type <- if (is.null(me)) "coef" else "ame"
  }
  if (type == "ame") {
    if (is.null(me)) {
      stop("Marginal effects were not computed. Refit with ",
           "`compute_me = TRUE` (requires the 'margins' package), or use ",
           "`type = \"coef\"`.", call. = FALSE)
    }
    s <- summary(me)
    return(forest_plot(as.character(s$factor), s$AME, s$lower, s$upper,
                       xlab = "Average marginal effect (probability)"))
  }
  coef_plot(stats::coef(x$model), stats::vcov(x), level)
}


#' Forest plot of coefficients with Wald intervals, intercept omitted
#' @noRd
coef_plot <- function(beta, vc, level) {
  if (!is.numeric(level) || length(level) != 1L || level <= 0 || level >= 1) {
    stop("`level` must be a single number strictly between 0 and 1.",
         call. = FALSE)
  }
  beta <- beta[names(beta) != "(Intercept)"]
  se <- sqrt(diag(vc))[names(beta)]
  z <- stats::qnorm(1 - (1 - level) / 2)
  forest_plot(names(beta), unname(beta), unname(beta - z * se),
              unname(beta + z * se),
              xlab = "Coefficient (logit scale)")
}

#' Shared forest-plot builder
#' @noRd
forest_plot <- function(label, estimate, lower, upper, xlab) {
  df <- data.frame(
    label    = factor(label, levels = rev(unique(label))),
    estimate = estimate,
    lower    = lower,
    upper    = upper
  )
  ink <- "#1F2328"
  ggplot2::ggplot(df, ggplot2::aes(y = .data$label)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                        colour = "#8C959F", linewidth = 0.4) +
    ggplot2::geom_segment(
      ggplot2::aes(x = .data$lower, xend = .data$upper,
                   yend = .data$label),
      colour = ink, linewidth = 0.6, na.rm = TRUE
    ) +
    ggplot2::geom_point(ggplot2::aes(x = .data$estimate), colour = ink,
                        size = 2.6) +
    ggplot2::labs(x = xlab, y = NULL) +
    theme_fs()
}

#' @title A plain theme for factorial survey plots
#'
#' @description
#' The theme used by the package's plot methods: white background, light
#' horizontal guides, no minor grid and no vertical grid. Add it to your own
#' plots for a consistent look.
#'
#' @param base_size Base font size, in points.
#' @param base_family Base font family.
#'
#' @return A [ggplot2::theme()] object.
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg)) + geom_point() + theme_fs()
theme_fs <- function(base_size = 11, base_family = "") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = "#EAEEF2",
                                                 linewidth = 0.4),
      axis.line.x        = ggplot2::element_line(colour = "#57606A",
                                                 linewidth = 0.4),
      axis.ticks.x       = ggplot2::element_line(colour = "#57606A",
                                                 linewidth = 0.4),
      axis.text          = ggplot2::element_text(colour = "#24292F"),
      axis.title.x       = ggplot2::element_text(colour = "#24292F",
                                                 margin = ggplot2::margin(t = 6)),
      plot.margin        = ggplot2::margin(8, 12, 8, 8)
    )
}
