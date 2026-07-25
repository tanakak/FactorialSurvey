attrs <- list(
  damage   = c("minor", "moderate", "severe"),
  distance = c("near", "far"),
  cost     = c(100, 500, 1000)
)

test_that("fs_defficient returns an fs_design of the requested size", {
  d <- fs_defficient(attrs, n_vignettes = 12, seed = 1, verbose = FALSE)

  expect_s3_class(d, "fs_design")
  expect_s3_class(d, "data.frame")
  expect_equal(nrow(d), 12L)
  expect_true(all(names(attrs) %in% names(d)))
  expect_true("block" %in% names(d))
  expect_equal(unique(d$block), 1L)
})

test_that("attributes carry the design metadata", {
  d <- fs_defficient(attrs, n_vignettes = 12, seed = 1, verbose = FALSE)

  expect_equal(attr(d, "factors"), attrs)
  expect_equal(attr(d, "n_candidates"), 18L)
  expect_true(is.numeric(attr(d, "d_efficiency")))
  expect_gt(attr(d, "d_efficiency"), 0)
  expect_s3_class(attr(d, "model"), "formula")
})

test_that("blocks are equal in size and cover every vignette", {
  for (method in c("optimal", "random")) {
    d <- fs_defficient(attrs, n_vignettes = 12, n_blocks = 3,
                       blocking = method, seed = 2, verbose = FALSE)

    expect_equal(as.integer(table(d$block)), rep(4L, 3L))
    expect_setequal(unique(d$block), 1:3)

    # No vignette is duplicated or lost by the allocation
    body <- as.data.frame(d)[, names(attrs)]
    expect_equal(anyDuplicated(body), 0L, info = method)
    expect_equal(nrow(body), 12L, info = method)
  }
})

test_that("optimal blocking is the default and is recorded", {
  d <- fs_defficient(attrs, n_vignettes = 12, n_blocks = 3,
                     seed = 2, verbose = FALSE)

  expect_equal(attr(d, "blocking"), "optimal")
  expect_true(is.numeric(attr(d, "block_criterion")))
  expect_false(is.na(attr(d, "block_criterion")))
})

test_that("random blocking is recorded and has no block criterion", {
  d <- fs_defficient(attrs, n_vignettes = 12, n_blocks = 3,
                     blocking = "random", seed = 2, verbose = FALSE)

  expect_equal(attr(d, "blocking"), "random")
  expect_true(is.na(attr(d, "block_criterion")))
})

test_that("a single block records no blocking method", {
  d <- fs_defficient(attrs, n_vignettes = 12, seed = 2, verbose = FALSE)

  expect_equal(attr(d, "blocking"), "none")
  expect_equal(unique(d$block), 1L)
})

test_that("optimal blocking spreads levels across blocks better than random", {
  # The point of optBlock(): each block should look like the whole design.
  # Measured as total absolute deviation from an equal split.
  imbalance <- function(d) {
    ev <- fs_design_eval(d)
    sum(vapply(ev$balance_by_block, function(tab) {
      sum(abs(tab - mean(tab)))
    }, numeric(1L)))
  }

  opt <- fs_defficient(attrs, n_vignettes = 12, n_blocks = 3,
                       blocking = "optimal", seed = 11, verbose = FALSE)
  rnd <- fs_defficient(attrs, n_vignettes = 12, n_blocks = 3,
                       blocking = "random", seed = 11, verbose = FALSE)

  expect_lte(imbalance(opt), imbalance(rnd))
})

test_that("the seed makes the design reproducible", {
  d1 <- fs_defficient(attrs, n_vignettes = 12, n_blocks = 2,
                      seed = 99, verbose = FALSE)
  d2 <- fs_defficient(attrs, n_vignettes = 12, n_blocks = 2,
                      seed = 99, verbose = FALSE)

  expect_equal(as.data.frame(d1), as.data.frame(d2))
})

test_that("the caller's RNG state is left untouched", {
  set.seed(4321)
  before <- .Random.seed
  invisible(fs_defficient(attrs, n_vignettes = 12, seed = 7, verbose = FALSE))
  expect_identical(.Random.seed, before)

  # And the stream continues as if nothing had happened
  set.seed(4321)
  expected <- runif(3)
  set.seed(4321)
  invisible(fs_defficient(attrs, n_vignettes = 12, seed = 7, verbose = FALSE))
  expect_equal(runif(3), expected)
})

test_that("restrictions remove the forbidden combinations", {
  d <- fs_defficient(attrs, n_vignettes = 9,
                     restrictions = ~ damage == "minor" & cost == 1000,
                     seed = 3, verbose = FALSE)

  expect_false(any(d$damage == "minor" & d$cost == 1000))
  expect_equal(attr(d, "n_candidates"), 16L)
})

test_that("several restrictions can be supplied at once", {
  d <- fs_defficient(
    attrs, n_vignettes = 8,
    restrictions = list(~ damage == "minor" & cost == 1000,
                        ~ damage == "severe" & cost == 100),
    seed = 3, verbose = FALSE
  )

  expect_equal(attr(d, "n_candidates"), 14L)
  expect_false(any(d$damage == "severe" & d$cost == 100))
})

test_that("an interaction model yields an estimable design", {
  d <- fs_defficient(attrs, n_vignettes = 16, model = ~ .^2,
                     seed = 5, verbose = FALSE)

  expect_equal(nrow(d), 16L)
  mm <- model.matrix(~ .^2, data = as.data.frame(d)[, names(attrs)])
  expect_equal(qr(mm)$rank, ncol(mm))
})

test_that("invalid input is rejected", {
  expect_error(fs_defficient("not a list", 6), "non-empty list")
  expect_error(fs_defficient(list(1:2, 1:3), 6), "must be named")
  expect_error(fs_defficient(list(a = 1, b = 1:3), 6), "at least two levels")
  expect_error(fs_defficient(attrs, 10, n_blocks = 3), "divisible")
  expect_error(fs_defficient(attrs, 100), "exceeds the number of available")
  expect_error(fs_defficient(attrs, 3), "smaller than the number of")
  expect_error(fs_defficient(attrs, 12, model = y ~ x), "one-sided formula")
  expect_error(fs_defficient(attrs, 12, verbose = "yes"), "must be TRUE or FALSE")
  expect_error(fs_defficient(attrs, 12, max_candidates = 5),
               "exceeds .*max_candidates")
  expect_error(
    fs_defficient(attrs, 12, restrictions = ~ damage %in% attrs$damage),
    "rule out every combination"
  )
})

test_that("verbose emits messages and the default is silent", {
  expect_message(fs_defficient(attrs, 12, seed = 1, verbose = TRUE),
                 "Full factorial")
  expect_silent(fs_defficient(attrs, 12, seed = 1, verbose = FALSE))
})

test_that("print.fs_design shows a header and rows", {
  d <- fs_defficient(attrs, n_vignettes = 12, seed = 1, verbose = FALSE)
  expect_output(print(d), "Factorial survey design")
  expect_output(print(d), "D-efficiency")
  expect_output(print(d, n = 3), "9 more row")
  expect_identical(withVisible(print(d))$visible, FALSE)
})


# --- fs_design_eval ---------------------------------------------------------

test_that("fs_design_eval reports balance and correlations", {
  d <- fs_defficient(attrs, n_vignettes = 12, n_blocks = 2,
                     seed = 1, verbose = FALSE)
  ev <- fs_design_eval(d)

  expect_s3_class(ev, "fs_design_eval")
  expect_named(ev$balance, names(attrs))
  expect_equal(sum(ev$balance$damage), 12L)
  expect_equal(dim(ev$correlation), c(3L, 3L))
  expect_equal(diag(ev$correlation), rep(1, 3L),
               ignore_attr = TRUE)
  expect_true(ev$max_abs_correlation >= 0 && ev$max_abs_correlation <= 1)
  expect_equal(ev$n_vignettes, 12L)
  expect_false(is.null(ev$balance_by_block))
})

test_that("fs_design_eval can skip the by-block breakdown", {
  d <- fs_defficient(attrs, n_vignettes = 12, n_blocks = 2,
                     seed = 1, verbose = FALSE)
  expect_null(fs_design_eval(d, by_block = FALSE)$balance_by_block)
})

test_that("fs_design_eval accepts a plain data frame", {
  df <- data.frame(a = rep(1:2, 6), b = rep(1:3, 4))
  ev <- fs_design_eval(df)

  expect_s3_class(ev, "fs_design_eval")
  expect_null(ev$d_efficiency)
})

test_that("fs_design_eval rejects unusable input", {
  expect_error(fs_design_eval(1:10), "data frame")
  expect_error(fs_design_eval(data.frame(block = 1:3)), "no attribute columns")
})

test_that("print.fs_design_eval produces output", {
  d <- fs_defficient(attrs, n_vignettes = 12, n_blocks = 2,
                     seed = 1, verbose = FALSE)
  expect_output(print(fs_design_eval(d)), "Level balance")
  expect_output(print(fs_design_eval(d)), "Attribute correlations")
})
