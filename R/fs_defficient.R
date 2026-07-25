#' @title Generate a D-efficient set of vignettes for a factorial survey
#'
#' @description
#' Selects a subset of the full factorial that is efficient for estimating the
#' effects of the attributes, using the exchange algorithm of
#' [AlgDesign::optFederov()], and splits the selected vignettes into blocks
#' that can be administered to different respondents.
#'
#' @param factors A named list defining the attributes and their levels, in
#'   the form accepted by [expand.grid()]. Each element may be numeric,
#'   character or a factor.
#' @param n_vignettes The number of vignettes to select. Must not exceed the
#'   number of distinct combinations, and must be at least the number of
#'   parameters implied by `model`.
#' @param n_blocks The number of blocks the vignettes are divided into.
#'   `n_vignettes` must be divisible by `n_blocks`.
#' @param model A one-sided formula giving the model the design should be
#'   efficient for. The default `~ .` covers main effects only; use `~ .^2` to
#'   make the design efficient for two-way interactions as well.
#' @param restrictions An optional one-sided formula, or a list of such
#'   formulas, describing combinations that must **not** appear. Each formula
#'   is evaluated within the full factorial and rows for which it is `TRUE`
#'   are removed before the search, for example
#'   `~ tenure == "new" & condition == "derelict"`.
#' @param n_repeats The number of random starts used by the exchange
#'   algorithm. Larger values search harder at the cost of run time.
#' @param shuffle_within_blocks A logical value. If `TRUE` (the default), the
#'   row order within each block is randomised, which helps to break up order
#'   effects in the questionnaire.
#' @param seed A single number used to make the search and the shuffling
#'   reproducible, or `NULL` for no seed. The state of the caller's random
#'   number generator is restored when the function returns.
#' @param max_candidates The largest candidate set the function will build
#'   before giving up, as a guard against an accidentally enormous factorial.
#' @param verbose A logical value. If `TRUE` (the default), progress is
#'   reported through [message()].
#'
#' @return An object of class `fs_design`, which inherits from `data.frame`.
#'   It has one row per selected vignette, the attribute columns, and a
#'   `block` column. The following attributes are attached:
#'   \describe{
#'     \item{`factors`}{The list of attributes and levels supplied.}
#'     \item{`d_efficiency`}{The D-efficiency reported by
#'       [AlgDesign::optFederov()], relative to a hypothetical orthogonal
#'       design.}
#'     \item{`d_criterion`}{The determinant-based criterion value.}
#'     \item{`n_candidates`}{The number of candidate combinations searched.}
#'     \item{`model`}{The model formula the design was optimised for.}
#'   }
#'
#' @details
#' Blocks are formed by shuffling the selected vignettes and dealing them out
#' in equal numbers. This keeps block sizes exactly equal and is robust for
#' any combination of attributes, but it does not optimise efficiency *within*
#' each block. When every respondent sees one block and effects are estimated
#' by pooling across respondents, this is the relevant behaviour. Optimal
#' blocking through [AlgDesign::optBlock()] is planned for a future release.
#'
#' The function does not write files. To save a design, pass the returned
#' object to [utils::write.csv()] yourself.
#'
#' @seealso [fs_design_eval()] for level balance and correlation diagnostics,
#'   [set_labels()] for attaching the wording shown to respondents.
#'
#' @references
#' Federov, V. V. (1972). *Theory of Optimal Experiments*. Academic Press.
#'
#' @importFrom stats model.matrix update
#' @export
#' @examples
#' attrs <- list(
#'   damage   = c("minor", "moderate", "severe"),
#'   distance = c("near", "far"),
#'   cost     = c(100, 500, 1000)
#' )
#'
#' d <- fs_defficient(attrs, n_vignettes = 12, n_blocks = 2,
#'                    seed = 42, verbose = FALSE)
#' head(as.data.frame(d))
#' table(d$block)
#'
#' # Exclude a combination that would not make sense to a respondent
#' d2 <- fs_defficient(attrs, n_vignettes = 9,
#'                     restrictions = ~ damage == "minor" & cost == 1000,
#'                     seed = 42, verbose = FALSE)
#' any(d2$damage == "minor" & d2$cost == 1000)
fs_defficient <- function(factors,
                          n_vignettes,
                          n_blocks = 1,
                          model = ~ .,
                          restrictions = NULL,
                          n_repeats = 50,
                          shuffle_within_blocks = TRUE,
                          seed = NULL,
                          max_candidates = 1e5,
                          verbose = TRUE) {

  ## ---- Input validation ---------------------------------------------------
  if (!is.list(factors) || length(factors) == 0L) {
    stop("`factors` must be a non-empty list.", call. = FALSE)
  }
  if (is.null(names(factors)) || any(names(factors) == "")) {
    stop("Every element of `factors` must be named.", call. = FALSE)
  }
  if (any(vapply(factors, length, integer(1L)) < 2L)) {
    stop("Every attribute in `factors` must have at least two levels.",
         call. = FALSE)
  }
  check_count(n_vignettes)
  check_count(n_blocks)
  check_count(n_repeats)
  check_flag(shuffle_within_blocks)
  check_flag(verbose)
  if (!inherits(model, "formula") || length(model) != 2L) {
    stop("`model` must be a one-sided formula, such as `~ .` or `~ .^2`.",
         call. = FALSE)
  }
  if (n_vignettes %% n_blocks != 0L) {
    stop("`n_vignettes` (", n_vignettes, ") must be divisible by `n_blocks` (",
         n_blocks, ").", call. = FALSE)
  }

  check_seed(seed)
  if (!is.null(seed)) {
    old_rng <- rng_state()
    on.exit(restore_rng(old_rng), add = TRUE)
    set.seed(seed)
  }

  ## ---- Candidate set ------------------------------------------------------
  n_full <- prod(vapply(factors, length, integer(1L)))
  if (n_full > max_candidates) {
    stop("The full factorial has ", n_full, " combinations, which exceeds ",
         "`max_candidates` (", max_candidates, "). Reduce the number of ",
         "attributes or levels, or raise `max_candidates` if you are ",
         "prepared to wait.", call. = FALSE)
  }

  candidates <- expand.grid(factors, KEEP.OUT.ATTRS = FALSE,
                            stringsAsFactors = TRUE)
  say(verbose, "Full factorial: ", nrow(candidates), " combinations.")

  if (!is.null(restrictions)) {
    candidates <- apply_restrictions(candidates, restrictions, verbose)
  }
  n_candidates <- nrow(candidates)

  if (n_vignettes > n_candidates) {
    stop("`n_vignettes` (", n_vignettes, ") exceeds the number of available ",
         "combinations (", n_candidates, ").", call. = FALSE)
  }

  n_par <- ncol(stats::model.matrix(model, data = candidates))
  if (n_vignettes < n_par) {
    stop("`n_vignettes` (", n_vignettes, ") is smaller than the number of ",
         "parameters implied by `model` (", n_par, "). The design would not ",
         "be estimable.", call. = FALSE)
  }

  ## ---- D-efficient search -------------------------------------------------
  say(verbose, "Searching for ", n_vignettes,
      " vignettes by the D-efficiency criterion ...")

  search <- AlgDesign::optFederov(
    frml      = model,
    data      = candidates,
    nTrials   = n_vignettes,
    criterion = "D",
    nRepeats  = n_repeats
  )
  design <- as.data.frame(search$design)
  rownames(design) <- NULL

  say(verbose, "D-efficiency: ", round(search$Dea, 4))

  ## ---- Blocking -----------------------------------------------------------
  if (n_blocks > 1L) {
    say(verbose, "Dividing into ", n_blocks, " blocks.")
    design <- design[sample.int(nrow(design)), , drop = FALSE]
    design$block <- rep(seq_len(n_blocks), each = n_vignettes / n_blocks)
  } else {
    design$block <- 1L
  }

  ## ---- Shuffle within blocks ----------------------------------------------
  if (shuffle_within_blocks) {
    parts <- split(design, design$block)
    parts <- lapply(parts, function(part) {
      part[sample.int(nrow(part)), , drop = FALSE]
    })
    design <- do.call(rbind, parts)
  }
  rownames(design) <- NULL

  ## ---- Assemble -----------------------------------------------------------
  attr(design, "factors")      <- factors
  attr(design, "d_efficiency") <- search$Dea
  attr(design, "d_criterion")  <- search$D
  attr(design, "n_candidates") <- n_candidates
  attr(design, "model")        <- model
  class(design) <- c("fs_design", "data.frame")

  say(verbose, "Done.")
  design
}


#' Remove restricted combinations from a candidate set
#'
#' @param candidates A data frame of candidate combinations.
#' @param restrictions A one-sided formula or a list of one-sided formulas.
#' @param verbose Logical flag controlling progress messages.
#' @return The candidate set with restricted rows removed.
#' @noRd
apply_restrictions <- function(candidates, restrictions, verbose = FALSE) {
  if (inherits(restrictions, "formula")) {
    restrictions <- list(restrictions)
  }
  if (!is.list(restrictions) ||
      !all(vapply(restrictions, inherits, logical(1L), "formula"))) {
    stop("`restrictions` must be a one-sided formula or a list of one-sided ",
         "formulas.", call. = FALSE)
  }

  drop <- rep(FALSE, nrow(candidates))
  for (i in seq_along(restrictions)) {
    rule <- restrictions[[i]]
    if (length(rule) != 2L) {
      stop("Restriction ", i, " must be a one-sided formula, such as ",
           "`~ a == 1 & b == 2`.", call. = FALSE)
    }
    hit <- eval(rule[[2L]], envir = candidates, enclos = environment(rule))
    if (!is.logical(hit) || length(hit) != nrow(candidates)) {
      stop("Restriction ", i, " did not evaluate to a logical vector of ",
           "length ", nrow(candidates), ".", call. = FALSE)
    }
    hit[is.na(hit)] <- FALSE
    drop <- drop | hit
  }

  if (all(drop)) {
    stop("The restrictions rule out every combination.", call. = FALSE)
  }
  say(verbose, "Restrictions removed ", sum(drop), " combination(s); ",
      sum(!drop), " remain.")
  candidates[!drop, , drop = FALSE]
}


#' @title Print a factorial survey design
#'
#' @param x An object of class `fs_design`, from [fs_defficient()].
#' @param n The number of rows to show.
#' @param ... Ignored.
#'
#' @return `x`, invisibly. Called for the side effect of printing.
#' @export
#' @examples
#' d <- fs_defficient(list(a = 1:2, b = 1:3), n_vignettes = 6,
#'                    seed = 1, verbose = FALSE)
#' print(d)
print.fs_design <- function(x, n = 10L, ...) {
  cat("Factorial survey design\n")
  cat("  Vignettes    :", nrow(x), "\n")
  cat("  Blocks       :", length(unique(x$block)), "\n")
  cat("  Attributes   :", length(attr(x, "factors")), "\n")
  cat("  Candidates   :", attr(x, "n_candidates"), "\n")
  cat("  D-efficiency :", fmt(attr(x, "d_efficiency")), "\n\n")

  body <- as.data.frame(x)
  n <- min(n, nrow(body))
  print(utils::head(body, n))
  if (nrow(body) > n) {
    cat("... ", nrow(body) - n, " more row(s)\n", sep = "")
  }
  invisible(x)
}


#' @title Diagnostics for a factorial survey design
#'
#' @description
#' Reports how evenly the levels of each attribute appear in a design and how
#' strongly the attributes are correlated with one another. A well-behaved
#' design has near-equal level counts and near-zero correlations; large
#' correlations mean the effects of the attributes involved will be hard to
#' separate.
#'
#' @param design An object of class `fs_design`, from [fs_defficient()], or a
#'   plain data frame of attribute columns.
#' @param by_block A logical value. If `TRUE`, level counts are reported
#'   separately for each block as well as overall.
#'
#' @return An object of class `fs_design_eval`, a list with components:
#'   \describe{
#'     \item{`balance`}{A list of tables of level counts, one per attribute.}
#'     \item{`balance_by_block`}{A list of tables of level counts by block, or
#'       `NULL` when `by_block` is `FALSE` or there is a single block.}
#'     \item{`correlation`}{The correlation matrix of the attributes, computed
#'       on their numeric codes.}
#'     \item{`max_abs_correlation`}{The largest absolute off-diagonal
#'       correlation.}
#'     \item{`d_efficiency`}{The D-efficiency of the design, when available.}
#'     \item{`n_vignettes`}{The number of vignettes.}
#'   }
#'
#' @seealso [fs_defficient()]
#'
#' @importFrom stats cor
#' @export
#' @examples
#' d <- fs_defficient(list(damage   = c("low", "high"),
#'                         distance = c("near", "far"),
#'                         cost     = c(100, 500, 1000)),
#'                    n_vignettes = 12, n_blocks = 2,
#'                    seed = 7, verbose = FALSE)
#' fs_design_eval(d)
fs_design_eval <- function(design, by_block = TRUE) {
  if (!is.data.frame(design)) {
    stop("`design` must be a data frame or an `fs_design` object.",
         call. = FALSE)
  }
  check_flag(by_block)

  body <- as.data.frame(design)
  block <- if ("block" %in% names(body)) body$block else NULL
  attrs <- body[, setdiff(names(body), "block"), drop = FALSE]
  if (ncol(attrs) == 0L) {
    stop("`design` contains no attribute columns.", call. = FALSE)
  }

  balance <- lapply(attrs, table)

  balance_by_block <- NULL
  if (by_block && !is.null(block) && length(unique(block)) > 1L) {
    balance_by_block <- lapply(attrs, function(col) table(col, block))
  }

  codes <- vapply(attrs, function(col) as.numeric(as.factor(col)),
                  numeric(nrow(attrs)))
  codes <- matrix(codes, nrow = nrow(attrs),
                  dimnames = list(NULL, names(attrs)))

  correlation <- if (ncol(codes) > 1L) {
    suppressWarnings(stats::cor(codes))
  } else {
    matrix(1, 1L, 1L, dimnames = list(names(attrs), names(attrs)))
  }
  off_diag <- correlation
  diag(off_diag) <- NA_real_
  max_abs <- if (all(is.na(off_diag))) 0 else max(abs(off_diag), na.rm = TRUE)

  out <- list(
    balance             = balance,
    balance_by_block    = balance_by_block,
    correlation         = correlation,
    max_abs_correlation = max_abs,
    d_efficiency        = attr(design, "d_efficiency"),
    n_vignettes         = nrow(body)
  )
  class(out) <- "fs_design_eval"
  out
}


#' @title Print design diagnostics
#'
#' @param x An object of class `fs_design_eval`, from [fs_design_eval()].
#' @param digits Number of digits used for the correlation matrix.
#' @param ... Ignored.
#'
#' @return `x`, invisibly. Called for the side effect of printing.
#' @export
#' @examples
#' d <- fs_defficient(list(a = 1:2, b = 1:3), n_vignettes = 6,
#'                    seed = 1, verbose = FALSE)
#' print(fs_design_eval(d))
print.fs_design_eval <- function(x, digits = 3, ...) {
  cat("Design diagnostics\n")
  cat("  Vignettes    :", x$n_vignettes, "\n")
  if (!is.null(x$d_efficiency)) {
    cat("  D-efficiency :", fmt(x$d_efficiency), "\n")
  }
  cat("\nLevel balance\n")
  for (nm in names(x$balance)) {
    cat("  ", nm, ":\n", sep = "")
    print(x$balance[[nm]])
  }
  if (!is.null(x$balance_by_block)) {
    cat("\nLevel balance by block\n")
    for (nm in names(x$balance_by_block)) {
      cat("  ", nm, ":\n", sep = "")
      print(x$balance_by_block[[nm]])
    }
  }
  cat("\nAttribute correlations\n")
  print(round(x$correlation, digits))
  cat("\nLargest absolute off-diagonal correlation:",
      fmt(x$max_abs_correlation), "\n")
  if (x$max_abs_correlation > 0.3) {
    cat("Note: correlations above 0.3 make the effects of the attributes",
        "involved\n  harder to separate. Consider more vignettes or fewer",
        "restrictions.\n")
  }
  invisible(x)
}
