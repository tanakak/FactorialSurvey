make_d <- function() {
  data.frame(damage = c("minor", "severe"),
             cost   = c(100, 1000),
             stringsAsFactors = FALSE)
}

test_that("placeholders are replaced row by row", {
  out <- fs_vignette_text(make_d(), "Damage {{damage}}, cost {{cost}}.")

  expect_true("vignette_text" %in% names(out))
  expect_equal(out$vignette_text,
               c("Damage minor, cost 100.", "Damage severe, cost 1000."))
})

test_that("whitespace inside the braces is ignored", {
  a <- fs_vignette_text(make_d(), "{{damage}}/{{cost}}")$vignette_text
  b <- fs_vignette_text(make_d(), "{{ damage }}/{{  cost  }}")$vignette_text
  expect_equal(a, b)
})

test_that("a placeholder used twice is replaced everywhere", {
  out <- fs_vignette_text(make_d(), "{{damage}} and {{damage}} again")
  expect_equal(out$vignette_text[1], "minor and minor again")
})

test_that("the column name can be chosen and the design is not disturbed", {
  d <- make_d()
  out <- fs_vignette_text(d, "{{cost}}", column = "blurb")

  expect_true("blurb" %in% names(out))
  expect_false("vignette_text" %in% names(out))
  expect_equal(out$cost, d$cost)
  expect_equal(out$damage, d$damage)
})

test_that("formatting functions render values without changing the data", {
  d <- make_d()
  out <- fs_vignette_text(
    d, "costs {{cost}}",
    format = list(cost = function(x) paste0("$", formatC(x, big.mark = ",",
                                                         format = "d")))
  )

  expect_equal(out$vignette_text, c("costs $100", "costs $1,000"))
  expect_type(out$cost, "double")
  expect_equal(out$cost, d$cost)
})

test_that("factor levels are used, not the integer codes", {
  d <- data.frame(size = factor(c(1, 2), labels = c("small", "large")))
  out <- fs_vignette_text(d, "a {{size}} one")
  expect_equal(out$vignette_text, c("a small one", "a large one"))
})

test_that("missing values become na_text", {
  d <- data.frame(x = c("a", NA), stringsAsFactors = FALSE)
  expect_equal(fs_vignette_text(d, "[{{x}}]")$vignette_text, c("[a]", "[]"))
  expect_equal(fs_vignette_text(d, "[{{x}}]", na_text = "n/a")$vignette_text,
               c("[a]", "[n/a]"))
})

test_that("non-ASCII templates and column names survive intact", {
  # Written with \u escapes so the test means the same thing in any locale,
  # as R CMD check requires. The strings are Japanese: "damage", "cost",
  # "minor", "severe", and a sentence template.
  damage <- "\u88ab\u5bb3"
  cost <- "\u8cbb\u7528"
  minor <- "\u8efd\u5fae"
  severe <- "\u751a\u5927"

  d <- data.frame(c(minor, severe), c(1000, 30000),
                  stringsAsFactors = FALSE)
  names(d) <- c(damage, cost)

  template <- paste0(damage, "\u306f{{", damage, "}}\u3067\u3059\u3002",
                     "\u984d\u306f{{", cost, "}}\u5186\u3002")

  fmt <- list(function(x) formatC(x, big.mark = ",", format = "d"))
  names(fmt) <- cost

  out <- fs_vignette_text(d, template, format = fmt)

  expect_true(all(validUTF8(out$vignette_text)))
  expect_equal(Encoding(out$vignette_text), rep("UTF-8", 2L))
  expect_true(grepl(minor, out$vignette_text[1], fixed = TRUE))
  expect_true(grepl(severe, out$vignette_text[2], fixed = TRUE))
  expect_true(grepl("30,000", out$vignette_text[2], fixed = TRUE))

  # The placeholder braces are fully consumed, nothing is left behind
  expect_false(any(grepl("{{", out$vignette_text, fixed = TRUE)))
})

test_that("regular expression metacharacters in values are taken literally", {
  d <- data.frame(x = c("a.b", "c$d"), stringsAsFactors = FALSE)
  out <- fs_vignette_text(d, "value {{x}} end")
  expect_equal(out$vignette_text, c("value a.b end", "value c$d end"))
})

test_that("an fs_design keeps its class and attributes", {
  d <- fs_defficient(list(a = 1:2, b = 1:3), n_vignettes = 6,
                     seed = 1, verbose = FALSE)
  out <- fs_vignette_text(d, "a is {{a}}")

  expect_s3_class(out, "fs_design")
  expect_equal(attr(out, "d_efficiency"), attr(d, "d_efficiency"))
  expect_equal(attr(out, "template"), "a is {{a}}")
})

test_that("bad input is rejected", {
  d <- make_d()

  expect_error(fs_vignette_text(1:3, "{{x}}"), "must be a data frame")
  expect_error(fs_vignette_text(d, c("a", "b")), "single character string")
  expect_error(fs_vignette_text(d, "no placeholders here"), "no \\{\\{")
  expect_error(fs_vignette_text(d, "{{nope}}"), "no matching column")
  expect_error(fs_vignette_text(d, "{{cost}}", format = list(zzz = identity)),
               "no matching column")
  expect_error(fs_vignette_text(d, "{{cost}}", format = list(cost = "x")),
               "must be a function")
  expect_error(fs_vignette_text(d, "{{cost}}",
                                format = list(cost = function(x) "one")),
               "returned 1 value")
})
