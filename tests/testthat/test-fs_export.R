make_export_design <- function() {
  d <- fs_defficient(list(damage = c("minor", "severe"),
                          cost   = c(100, 500, 1000)),
                     n_vignettes = 6, n_blocks = 2, seed = 1, verbose = FALSE)
  fs_vignette_text(d, "Damage {{damage}} at {{cost}}.")
}

test_that("csv export round-trips", {
  d <- make_export_design()
  path <- withr::local_tempfile(fileext = ".csv")

  out <- fs_export(d, path)
  expect_equal(out, path)
  expect_true(file.exists(path))

  back <- read.csv(path, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)
  expect_equal(nrow(back), nrow(d))
  expect_equal(back$damage, as.character(d$damage))
  expect_equal(back$vignette_text, d$vignette_text)
})

test_that("the BOM is written by default and can be switched off", {
  d <- make_export_design()
  with_bom <- withr::local_tempfile(fileext = ".csv")
  without <- withr::local_tempfile(fileext = ".csv")

  fs_export(d, with_bom)
  fs_export(d, without, bom = FALSE)

  expect_equal(readBin(with_bom, "raw", 3L), as.raw(c(0xEF, 0xBB, 0xBF)))
  expect_false(identical(readBin(without, "raw", 3L),
                         as.raw(c(0xEF, 0xBB, 0xBF))))
})

test_that("non-ASCII content survives the round trip", {
  # Japanese, written with \u escapes so the test is locale-independent
  damage <- "\u88ab\u5bb3"
  d <- data.frame(c("\u8efd\u5fae", "\u751a\u5927"),
                  stringsAsFactors = FALSE)
  names(d) <- damage
  d <- fs_vignette_text(
    d, paste0(damage, "\u306f{{", damage, "}}\u3067\u3059\u3002")
  )
  path <- withr::local_tempfile(fileext = ".csv")

  fs_export(d, path)

  # Compare the bytes on disk rather than reading back through read.csv().
  # A reader running in a non-UTF-8 locale re-encodes the text into <U+XXXX>
  # escapes, which would make this test fail for reasons that have nothing
  # to do with what fs_export() wrote.
  on_disk <- rawToChar(readBin(path, "raw", file.info(path)$size))

  expect_true(validUTF8(on_disk))
  for (sentence in d$vignette_text) {
    expect_true(
      grepl(enc2utf8(sentence), on_disk, fixed = TRUE, useBytes = TRUE),
      info = "the UTF-8 bytes of each vignette appear in the file"
    )
  }
})

test_that("tsv uses tabs", {
  d <- make_export_design()
  path <- withr::local_tempfile(fileext = ".tsv")
  fs_export(d, path, format = "tsv")

  lines <- readLines(path, warn = FALSE)
  expect_true(grepl("\t", lines[1], fixed = TRUE))
})

test_that("by_block writes one file per block and drops the block column", {
  d <- make_export_design()
  base <- withr::local_tempfile(fileext = ".csv")

  paths <- fs_export(d, base, by_block = TRUE)
  expect_length(paths, 2L)
  expect_true(all(file.exists(paths)))
  expect_true(all(grepl("_block[12]\\.csv$", paths)))

  back <- read.csv(paths[1], fileEncoding = "UTF-8-BOM")
  expect_false("block" %in% names(back))
  expect_equal(nrow(back), 3L)

  unlink(paths)
})

test_that("qualtrics adds a Loop column and splits by block automatically", {
  d <- make_export_design()
  base <- withr::local_tempfile(fileext = ".csv")

  paths <- fs_export(d, base, format = "qualtrics")
  expect_length(paths, 2L)

  back <- read.csv(paths[1], fileEncoding = "UTF-8-BOM")
  expect_equal(names(back)[1], "Loop")
  expect_equal(back$Loop, seq_len(nrow(back)))
  expect_false("block" %in% names(back))

  unlink(paths)
})

test_that("a single-block design stays in one qualtrics file", {
  d <- fs_defficient(list(a = 1:2, b = 1:3), n_vignettes = 6,
                     seed = 1, verbose = FALSE)
  path <- withr::local_tempfile(fileext = ".csv")

  paths <- fs_export(d, path, format = "qualtrics")
  expect_length(paths, 1L)
})

test_that("columns can be selected and reordered", {
  d <- make_export_design()
  path <- withr::local_tempfile(fileext = ".csv")

  fs_export(d, path, columns = c("cost", "damage"))
  back <- read.csv(path, fileEncoding = "UTF-8-BOM")

  expect_equal(names(back), c("cost", "damage"))
})

test_that("existing files are protected unless overwrite is set", {
  d <- make_export_design()
  path <- withr::local_tempfile(fileext = ".csv")

  fs_export(d, path)
  expect_error(fs_export(d, path), "already exists")
  expect_silent(fs_export(d, path, overwrite = TRUE))
})

test_that("a respondent schedule can be exported", {
  d <- make_export_design()
  s <- suppressMessages(fs_assign(d, n_respondents = 4, seed = 1))
  path <- withr::local_tempfile(fileext = ".csv")

  fs_export(s, path)
  back <- read.csv(path, fileEncoding = "UTF-8-BOM")

  expect_equal(nrow(back), nrow(s))
  expect_true(all(c("respondent", "position", "vignette") %in% names(back)))
})

test_that("bad input is rejected", {
  d <- make_export_design()
  path <- withr::local_tempfile(fileext = ".csv")

  expect_error(fs_export(1:5, path), "must be a data frame")
  expect_error(fs_export(d, 1), "single file path")
  expect_error(fs_export(d, path, format = "docx"), "should be one of")
  expect_error(fs_export(d, path, columns = "nope"), "not found in")
  expect_error(fs_export(d[0, ], path), "no rows")
})

test_that("xlsx writes one sheet per block", {
  skip_if_not_installed("openxlsx")

  d <- make_export_design()
  path <- withr::local_tempfile(fileext = ".xlsx")
  fs_export(d, path, format = "xlsx")

  expect_true(file.exists(path))
  expect_equal(openxlsx::getSheetNames(path), c("block1", "block2"))

  sheet <- openxlsx::read.xlsx(path, sheet = "block1")
  expect_equal(nrow(sheet), 3L)
  expect_false("block" %in% names(sheet))
})

test_that("xlsx respects the overwrite guard", {
  skip_if_not_installed("openxlsx")

  d <- make_export_design()
  path <- withr::local_tempfile(fileext = ".xlsx")
  fs_export(d, path, format = "xlsx")

  expect_error(fs_export(d, path, format = "xlsx"), "already exists")
  expect_silent(fs_export(d, path, format = "xlsx", overwrite = TRUE))
})
