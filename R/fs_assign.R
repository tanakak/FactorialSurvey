#' @title Construct a Williams design
#'
#' @description
#' Builds a set of presentation orders in which every item appears equally
#' often in every position **and** every ordered pair of items appears equally
#' often as immediate neighbours. The second property is what distinguishes a
#' Williams design from an ordinary Latin square: it balances first-order
#' carryover, so that the effect of having just seen one vignette on the rating
#' of the next does not confound any attribute.
#'
#' @param k The number of items to be ordered -- in a factorial survey, the
#'   number of vignettes one respondent sees.
#'
#' @return An integer matrix with `k` columns. Each row is one presentation
#'   order, giving the item shown in each position. There are `k` rows when `k`
#'   is even and `2 * k` rows when `k` is odd, which is the smallest number of
#'   orders that achieves carryover balance.
#'
#' @details
#' The first order is built as \eqn{1, 2, k, 3, k-1, 4, \ldots}, and the
#' remaining orders are cyclic shifts of it. For even `k` this single square is
#' already balanced. For odd `k` no single square can be, so the square is
#' paired with its mirror image, and each ordered pair of neighbours then
#' occurs exactly twice.
#'
#' @references
#' Williams, E. J. (1949). Experimental designs balanced for the estimation of
#' residual effects of treatments. *Australian Journal of Scientific Research,
#' Series A*, 2(2), 149--168. \doi{10.1071/CH9490149}
#'
#' @seealso [fs_assign()], which uses this to allocate orders to respondents.
#'
#' @export
#' @examples
#' # Even k: one square is enough
#' fs_williams(4)
#'
#' # Odd k: the square is paired with its mirror image
#' fs_williams(3)
#'
#' # Every item appears once in every position
#' table(fs_williams(4)[, 1])
fs_williams <- function(k) {
  check_count(k)
  k <- as.integer(k)
  if (k < 2L) {
    stop("`k` must be at least 2.", call. = FALSE)
  }

  ## First order: 1, 2, k, 3, k-1, 4, ...
  first <- integer(k)
  for (i in seq_len(k)) {
    first[i] <- if (i %% 2L == 1L) (i + 1L) %/% 2L else k + 1L - i %/% 2L
  }

  ## Remaining orders are cyclic shifts
  square <- t(vapply(seq_len(k) - 1L,
                     function(shift) ((first - 1L + shift) %% k) + 1L,
                     integer(k)))

  ## An odd number of items needs the mirror image as well
  if (k %% 2L == 1L) {
    square <- rbind(square, square[, rev(seq_len(k)), drop = FALSE])
  }

  dimnames(square) <- list(NULL, paste0("pos", seq_len(k)))
  square
}


#' @title Allocate blocks and presentation orders to respondents
#'
#' @description
#' Turns a design into the schedule a survey platform actually needs: one row
#' per respondent per vignette, stating which vignette that respondent sees in
#' which position. Blocks are dealt out evenly across respondents, and the
#' order within a block can be balanced with a Williams design so that
#' position and carryover effects do not confound the attributes.
#'
#' @param design An object of class `fs_design` from [fs_defficient()], or a
#'   data frame with a `block` column.
#' @param n_respondents The number of respondents to schedule.
#' @param order How the vignettes within a block are ordered for each
#'   respondent. One of `"williams"` (the default, a carryover-balanced set of
#'   orders from [fs_williams()]), `"random"` (an independent random order per
#'   respondent) or `"as_is"` (the order the rows appear in the design).
#' @param seed A single number making the allocation reproducible, or `NULL`.
#'   The caller's random number state is restored on exit.
#'
#' @return An object of class `fs_assignment`, which inherits from
#'   `data.frame`, with one row per respondent per vignette and the columns:
#'   \describe{
#'     \item{`respondent`}{Respondent identifier, `1` to `n_respondents`.}
#'     \item{`block`}{The block that respondent was given.}
#'     \item{`position`}{Position in the questionnaire, `1` upwards.}
#'     \item{`vignette`}{Row number of the vignette in `design`, so that
#'       responses can be matched back to the design.}
#'     \item{...}{Every column of `design`, including any text column added by
#'       [fs_vignette_text()].}
#'   }
#'   The number of distinct orders used is attached as the `n_orders`
#'   attribute, and the ordering method as `order`.
#'
#' @details
#' Respondents are assigned to blocks by dealing round: respondent 1 gets block
#' 1, respondent 2 block 2, and so on, wrapping around. When
#' `n_respondents` is not a multiple of the number of blocks, the earlier
#' blocks receive one extra respondent and a message says so.
#'
#' With `order = "williams"` the respondents within a block cycle through the
#' rows of [fs_williams()]. Carryover balance is only fully achieved once the
#' number of respondents per block is a multiple of the number of orders, which
#' is the block size for an even block size and twice it for an odd one; a
#' message reports the number of orders so this can be checked when choosing
#' the sample size.
#'
#' @seealso [fs_williams()], [fs_defficient()], [fs_export()]
#'
#' @export
#' @examples
#' d <- fs_defficient(list(damage = c("minor", "severe"),
#'                         cost   = c(100, 500, 1000)),
#'                    n_vignettes = 6, n_blocks = 2,
#'                    seed = 1, verbose = FALSE)
#'
#' schedule <- fs_assign(d, n_respondents = 8, seed = 1)
#' head(schedule)
#'
#' # Every respondent sees every vignette in their block exactly once
#' table(schedule$respondent)
#'
#' # Across respondents, each vignette appears in each position equally often
#' with(subset(schedule, block == 1), table(vignette, position))
fs_assign <- function(design,
                      n_respondents,
                      order = c("williams", "random", "as_is"),
                      seed = NULL) {

  order <- match.arg(order)
  if (!is.data.frame(design)) {
    stop("`design` must be a data frame or an `fs_design` object.",
         call. = FALSE)
  }
  check_count(n_respondents)
  check_seed(seed)

  body <- as.data.frame(design)
  if (!"block" %in% names(body)) {
    body$block <- 1L
  }
  body$vignette <- seq_len(nrow(body))

  blocks <- sort(unique(body$block))
  n_blocks <- length(blocks)

  sizes <- vapply(blocks, function(b) sum(body$block == b), integer(1L))
  if (length(unique(sizes)) > 1L) {
    stop("Blocks must all be the same size; sizes are ",
         paste(sizes, collapse = ", "), ".", call. = FALSE)
  }
  k <- sizes[[1L]]
  if (order == "williams" && k < 2L) {
    stop("A Williams order needs at least two vignettes per block.",
         call. = FALSE)
  }

  if (!is.null(seed)) {
    old_rng <- rng_state()
    on.exit(restore_rng(old_rng), add = TRUE)
    set.seed(seed)
  }

  ## ---- Respondents to blocks ------------------------------------------------
  block_of <- blocks[((seq_len(n_respondents) - 1L) %% n_blocks) + 1L]
  if (n_respondents %% n_blocks != 0L) {
    message("`n_respondents` (", n_respondents, ") is not a multiple of the ",
            "number of blocks (", n_blocks, "); the first ",
            n_respondents %% n_blocks, " block(s) receive one extra ",
            "respondent.")
  }

  ## ---- Orders ---------------------------------------------------------------
  orders <- switch(
    order,
    williams = fs_williams(k),
    as_is    = matrix(seq_len(k), nrow = 1L),
    random   = NULL
  )
  n_orders <- if (is.null(orders)) NA_integer_ else nrow(orders)

  if (order == "williams") {
    message("Using ", n_orders, " Williams order(s) of ", k,
            " vignettes. Carryover is fully balanced when the number of ",
            "respondents per block is a multiple of ", n_orders, ".")
  }

  ## ---- Build the schedule ---------------------------------------------------
  ## Respondents are dealt round-robin, so respondent r is the
  ## ((r - 1) %/% n_blocks + 1)-th person to receive their block. Orders must
  ## cycle on that within-block position, not on r: cycling on r would use
  ## only every n_blocks-th order and destroy the balance.
  rows <- lapply(seq_len(n_respondents), function(r) {
    b <- block_of[[r]]
    in_block <- body[body$block == b, , drop = FALSE]
    rownames(in_block) <- NULL

    within_block <- (r - 1L) %/% n_blocks
    idx <- if (order == "random") {
      sample.int(k)
    } else {
      orders[(within_block %% nrow(orders)) + 1L, ]
    }

    out <- in_block[idx, , drop = FALSE]
    out <- cbind(respondent = r, position = seq_len(k), out)
    rownames(out) <- NULL
    out
  })

  schedule <- do.call(rbind, rows)

  front <- c("respondent", "block", "position", "vignette")
  schedule <- schedule[, c(front, setdiff(names(schedule), front)),
                       drop = FALSE]
  rownames(schedule) <- NULL

  attr(schedule, "n_orders") <- n_orders
  attr(schedule, "order") <- order
  attr(schedule, "factors") <- attr(design, "factors")
  class(schedule) <- c("fs_assignment", "data.frame")
  schedule
}


#' @title Print a respondent schedule
#'
#' @param x An object of class `fs_assignment`, from [fs_assign()].
#' @param n The number of rows to show.
#' @param ... Ignored.
#'
#' @return `x`, invisibly. Called for the side effect of printing.
#' @export
#' @examples
#' d <- fs_defficient(list(a = 1:2, b = 1:3), n_vignettes = 6,
#'                    seed = 1, verbose = FALSE)
#' print(fs_assign(d, n_respondents = 4, seed = 1))
print.fs_assignment <- function(x, n = 10L, ...) {
  body <- as.data.frame(x)
  ## Subsetting with `[` keeps the class but drops the metadata and possibly
  ## the columns summarised below; print such objects as plain data frames.
  if (is.null(attr(x, "order")) ||
      !all(c("respondent", "block", "vignette", "position") %in% names(body))) {
    print(body)
    return(invisible(x))
  }
  cat("Respondent schedule\n")
  cat("  Respondents :", length(unique(body$respondent)), "\n")
  cat("  Blocks      :", length(unique(body$block)), "\n")
  cat("  Vignettes   :", length(unique(body$vignette)),
      "(", max(body$position), "per respondent )\n")
  cat("  Ordering    :", attr(x, "order"),
      if (isTRUE(!is.na(attr(x, "n_orders")))) {
        paste0("(", attr(x, "n_orders"), " distinct orders)")
      } else {
        ""
      }, "\n\n")

  n <- min(n, nrow(body))
  print(utils::head(body, n))
  if (nrow(body) > n) {
    cat("... ", nrow(body) - n, " more row(s)\n", sep = "")
  }
  invisible(x)
}
