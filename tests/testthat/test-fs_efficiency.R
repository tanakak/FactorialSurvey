l9 <- data.frame(a = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
                 b = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
                 c = c(1, 2, 3, 2, 3, 1, 3, 1, 2),
                 d = c(1, 2, 3, 3, 1, 2, 2, 3, 1))
f34 <- list(a = 1:3, b = 1:3, c = 1:3, d = 1:3)

test_that("an orthogonal array is 100 percent D- and A-efficient", {
  e <- fs_efficiency(l9)
  expect_s3_class(e, "fs_efficiency")
  expect_equal(e$d_efficiency, 100)
  expect_equal(e$a_efficiency, 100)
  expect_equal(e$n_parameters, 9L)
  expect_true(e$estimable)

  # A full factorial is also 100 percent
  ff <- expand.grid(a = c("x", "y"), b = c("p", "q", "r"), c = 1:4)
  expect_equal(fs_efficiency(ff)$d_efficiency, 100)
  expect_equal(fs_efficiency(ff, model = ~ .^2)$d_efficiency, 100)
})

test_that("efficiency does not depend on how levels are labelled", {
  lab <- l9
  lab$a <- c("low", "mid", "high")[lab$a]
  lab$d <- c(400, 600, 800)[lab$d]
  f <- list(a = c("low", "mid", "high"), b = 1:3, c = 1:3,
            d = c(400, 600, 800))
  expect_equal(fs_efficiency(lab, factors = f)$d_efficiency, 100)
})

test_that("a design that drops a level is not estimable and is flagged", {
  bad <- l9
  bad$a[bad$a == 2] <- 3
  e <- fs_efficiency(bad, factors = f34)
  expect_false(e$estimable)
  expect_equal(e$d_efficiency, 0)
  expect_equal(e$missing_levels, list(a = "2"))
  expect_output(print(e), "never appear")
})

test_that("a non-orthogonal design is less than 100 percent efficient", {
  set.seed(3)
  ff <- expand.grid(f34)
  e <- fs_efficiency(ff[sample(nrow(ff), 12), ], factors = f34)
  expect_lt(e$d_efficiency, 100)
  expect_gt(e$d_efficiency, 0)
  expect_lte(e$a_efficiency, e$d_efficiency + 1e-8)
})

test_that("linear attributes are coded as a single standardised term", {
  ff <- expand.grid(a = 1:3, price = c(400, 600, 800))
  e <- fs_efficiency(ff, linear = "price")
  expect_equal(e$n_parameters, 4L)
  expect_equal(e$d_efficiency, 100)
  expect_error(fs_efficiency(ff, linear = "nope"), "not in `factors`")
  expect_error(
    fs_efficiency(data.frame(a = c("x", "y")), linear = "a"),
    "numeric levels"
  )
})

test_that("fs_efficiency validates its input", {
  expect_error(fs_efficiency(1:3), "data frame")
  expect_error(fs_efficiency(l9, model = y ~ .), "one-sided")
  expect_error(fs_efficiency(l9, factors = list(z = 1:2)), "not found")
  expect_error(fs_efficiency(data.frame(block = 1:2)), "no attribute")
})

## ---- Regression tests for fs_defficient() ---------------------------------

test_that("fs_defficient finds an orthogonal array and reports 100 percent", {
  d <- fs_defficient(f34, n_vignettes = 9, seed = 1, verbose = FALSE)
  expect_equal(attr(d, "d_efficiency"), 100)
  expect_equal(fs_efficiency(d)$d_efficiency, 100)

  f27 <- setNames(rep(list(c("no", "yes")), 7), letters[1:7])
  d2 <- fs_defficient(f27, n_vignettes = 8, seed = 1, verbose = FALSE)
  expect_equal(attr(d2, "d_efficiency"), 100)
})

test_that("numerically coded levels are all used (no dropped middle level)", {
  f <- list(x1 = 1:3, x2 = 1:2, x3 = 1:2, x4 = c(400, 600, 800))
  d <- fs_defficient(f, n_vignettes = 12, seed = 1, verbose = FALSE)

  expect_setequal(unique(d$x1), 1:3)
  expect_setequal(unique(d$x4), c(400, 600, 800))
  expect_true(is.numeric(d$x1))
  expect_true(is.numeric(d$x4))
  expect_true(fs_efficiency(d)$estimable)
  expect_gt(attr(d, "d_efficiency"), 90)
})

test_that("`linear` optimises a slope and is recorded", {
  f <- list(type = c("a", "b"), price = c(400, 600, 800))
  d <- fs_defficient(f, n_vignettes = 4, linear = "price",
                     seed = 1, verbose = FALSE)
  expect_equal(attr(d, "linear"), "price")
  expect_setequal(unique(d$price), c(400, 800))
  expect_error(
    fs_defficient(f, n_vignettes = 6, linear = "type", verbose = FALSE),
    "numeric levels"
  )
})

test_that("restrictions with numeric comparisons still work", {
  f <- list(damage = c("minor", "severe"), cost = c(100, 500, 1000))
  d <- fs_defficient(f, n_vignettes = 5, restrictions = ~ cost > 500 &
                       damage == "minor", seed = 1, verbose = FALSE)
  expect_false(any(d$damage == "minor" & d$cost == 1000))
  expect_equal(attr(d, "n_candidates"), 5L)
})
