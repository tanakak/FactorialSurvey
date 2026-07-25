#' Capture the current state of the random number generator
#'
#' @return The value of `.Random.seed`, or `NULL` when the generator has not
#'   been initialised yet.
#' @noRd
rng_state <- function() {
  if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    get(".Random.seed", envir = globalenv(), inherits = FALSE)
  } else {
    NULL
  }
}

#' Restore a previously captured random number generator state
#'
#' Used with [on.exit()] so that seeding inside a function does not disturb
#' the caller's random number stream, which CRAN policy discourages.
#'
#' @param state A value produced by `rng_state()`.
#' @return `NULL`, invisibly. Called for its side effect.
#' @noRd
restore_rng <- function(state) {
  if (is.null(state)) {
    if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
  } else {
    assign(".Random.seed", state, envir = globalenv())
  }
  invisible(NULL)
}

#' Check that a value can be used as a seed
#'
#' @param seed A single number, or `NULL`.
#' @return `TRUE`, invisibly, if `seed` is usable.
#' @noRd
check_seed <- function(seed) {
  if (!is.null(seed) &&
      (!is.numeric(seed) || length(seed) != 1L || is.na(seed))) {
    stop("`seed` must be a single non-missing number, or NULL.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Check that a value is a single logical flag
#'
#' @param x Value to check.
#' @param arg Name of the argument, used in the error message.
#' @return `TRUE`, invisibly, if `x` is a single non-missing logical.
#' @noRd
check_flag <- function(x, arg = deparse(substitute(x))) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Check that a value is a single positive whole number
#'
#' @param x Value to check.
#' @param arg Name of the argument, used in the error message.
#' @return `TRUE`, invisibly, if `x` is a single positive whole number.
#' @noRd
check_count <- function(x, arg = deparse(substitute(x))) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) ||
      x < 1 || x != round(x)) {
    stop("`", arg, "` must be a single positive whole number.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Format a numeric vector for console display
#'
#' @param x Numeric vector.
#' @param digits Number of significant digits.
#' @return A character vector of the same length as `x`.
#' @noRd
fmt <- function(x, digits = 4) {
  format(x, digits = digits, trim = TRUE)
}

#' Emit a progress message when `verbose` is TRUE
#'
#' @param verbose Logical flag.
#' @param ... Passed to [message()].
#' @return `NULL`, invisibly.
#' @noRd
say <- function(verbose, ...) {
  if (isTRUE(verbose)) {
    message(...)
  }
  invisible(NULL)
}
