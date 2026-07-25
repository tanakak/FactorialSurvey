# --- fs_williams ------------------------------------------------------------

# Independent check: every ordered pair of neighbours, counted across all rows
carryover_counts <- function(m) {
  k <- ncol(m)
  pairs <- character(0)
  for (r in seq_len(nrow(m))) {
    for (j in seq_len(k - 1L)) {
      pairs <- c(pairs, paste(m[r, j], m[r, j + 1L], sep = "->"))
    }
  }
  table(pairs)
}

test_that("fs_williams returns a Latin square of the right shape", {
  for (k in 2:6) {
    m <- fs_williams(k)
    expect_equal(ncol(m), k)
    expect_equal(nrow(m), if (k %% 2 == 0) k else 2 * k)

    # Each row is a permutation of 1..k
    for (r in seq_len(nrow(m))) {
      expect_setequal(m[r, ], seq_len(k))
    }
  }
})

test_that("every item appears equally often in every position", {
  for (k in 2:6) {
    m <- fs_williams(k)
    for (j in seq_len(k)) {
      counts <- table(m[, j])
      expect_length(counts, k)
      expect_equal(length(unique(as.integer(counts))), 1L)
    }
  }
})

test_that("first-order carryover is balanced", {
  # This is the property that distinguishes a Williams design from any
  # other Latin square: every ordered pair of neighbours occurs equally often.
  for (k in 2:7) {
    counts <- carryover_counts(fs_williams(k))
    expect_equal(length(counts), k * (k - 1L),
                 info = paste("k =", k))
    expect_equal(length(unique(as.integer(counts))), 1L,
                 info = paste("k =", k))
  }
})

test_that("an ordinary Latin square is not carryover balanced", {
  # Guards the test above against being vacuously true
  k <- 4L
  plain <- t(vapply(0:(k - 1L),
                    function(s) as.integer(((seq_len(k) - 1L + s) %% k) + 1L),
                    integer(k)))
  counts <- carryover_counts(plain)
  expect_lt(length(counts), k * (k - 1))
})

test_that("fs_williams rejects bad input", {
  expect_error(fs_williams(1), "at least 2")
  expect_error(fs_williams(0), "positive whole number")
  expect_error(fs_williams(2.5), "positive whole number")
  expect_error(fs_williams("4"), "positive whole number")
})


# --- fs_assign --------------------------------------------------------------

make_design <- function(n_vignettes = 6, n_blocks = 2) {
  fs_defficient(list(damage = c("minor", "severe"),
                     cost   = c(100, 500, 1000)),
                n_vignettes = n_vignettes, n_blocks = n_blocks,
                seed = 1, verbose = FALSE)
}

test_that("fs_assign returns one row per respondent per vignette", {
  d <- make_design()
  s <- suppressMessages(fs_assign(d, n_respondents = 6, seed = 1))

  expect_s3_class(s, "fs_assignment")
  expect_s3_class(s, "data.frame")
  expect_equal(nrow(s), 6 * 3)          # 6 respondents, 3 vignettes per block
  expect_true(all(c("respondent", "block", "position", "vignette") %in%
                    names(s)))
  expect_equal(names(s)[1:4],
               c("respondent", "block", "position", "vignette"))
})

test_that("each respondent sees each vignette in their block exactly once", {
  d <- make_design()
  s <- suppressMessages(fs_assign(d, n_respondents = 6, seed = 1))

  by_resp <- split(s, s$respondent)
  for (r in by_resp) {
    expect_equal(anyDuplicated(r$vignette), 0L)
    expect_equal(sort(r$position), seq_len(nrow(r)))
    expect_equal(length(unique(r$block)), 1L)
  }
})

test_that("blocks are dealt out evenly", {
  d <- make_design(n_vignettes = 6, n_blocks = 2)
  s <- suppressMessages(fs_assign(d, n_respondents = 8, seed = 1))

  per_block <- table(unique(s[, c("respondent", "block")])$block)
  expect_equal(as.integer(per_block), c(4L, 4L))
})

test_that("an uneven respondent count is reported, not silently absorbed", {
  d <- make_design(n_vignettes = 6, n_blocks = 2)
  expect_message(fs_assign(d, n_respondents = 5, seed = 1),
                 "not a multiple of the number of blocks")
})

test_that("Williams ordering balances position across respondents", {
  # 3 vignettes per block, odd, so 6 orders; 6 respondents per block
  d <- make_design(n_vignettes = 6, n_blocks = 2)
  s <- suppressMessages(fs_assign(d, n_respondents = 12, seed = 1))

  for (b in unique(s$block)) {
    tab <- table(subset(s, s$block == b)$vignette,
                 subset(s, s$block == b)$position)
    expect_equal(length(unique(as.integer(tab))), 1L)
  }
  expect_equal(attr(s, "n_orders"), 6L)
  expect_equal(attr(s, "order"), "williams")
})

test_that("random ordering is reproducible and varies across respondents", {
  d <- make_design()
  a <- suppressMessages(fs_assign(d, 6, order = "random", seed = 3))
  b <- suppressMessages(fs_assign(d, 6, order = "random", seed = 3))
  expect_equal(as.data.frame(a), as.data.frame(b))

  c2 <- suppressMessages(fs_assign(d, 6, order = "random", seed = 99))
  expect_false(identical(as.data.frame(a), as.data.frame(c2)))
})

test_that("as_is ordering keeps the design order", {
  d <- make_design()
  s <- suppressMessages(fs_assign(d, 2, order = "as_is"))

  first <- subset(s, s$respondent == 1)
  expect_equal(first$vignette, sort(first$vignette))
})

test_that("the caller's RNG state is left untouched", {
  d <- make_design()
  set.seed(777)
  before <- .Random.seed
  invisible(suppressMessages(fs_assign(d, 6, order = "random", seed = 5)))
  expect_identical(.Random.seed, before)
})

test_that("a text column is carried through to the schedule", {
  d <- fs_vignette_text(make_design(), "Damage {{damage}} at {{cost}}.")
  s <- suppressMessages(fs_assign(d, 4, seed = 1))

  expect_true("vignette_text" %in% names(s))
  expect_false(anyNA(s$vignette_text))
})

test_that("fs_assign rejects bad input", {
  d <- make_design()

  expect_error(fs_assign(1:5, 4), "data frame")
  expect_error(fs_assign(d, 0), "positive whole number")
  expect_error(fs_assign(d, 4, order = "nope"), "should be one of")

  uneven <- as.data.frame(d)
  uneven$block <- c(1, 1, 1, 1, 2, 2)
  expect_error(suppressMessages(fs_assign(uneven, 4)), "same size")
})

test_that("print.fs_assignment produces output", {
  d <- make_design()
  s <- suppressMessages(fs_assign(d, 6, seed = 1))
  expect_output(print(s), "Respondent schedule")
  expect_output(print(s, n = 2), "more row")
})
