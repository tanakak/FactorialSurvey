#' @title Write a design or schedule out for a survey platform
#'
#' @description
#' Saves a design from [fs_defficient()] or a respondent schedule from
#' [fs_assign()] in a form a survey tool or a fieldwork agency will accept.
#' Text is always written as UTF-8, optionally with a byte order mark so that
#' Excel on Windows opens non-ASCII files correctly instead of showing
#' mojibake.
#'
#' @param x An `fs_design`, an `fs_assignment`, or any data frame.
#' @param file The path to write to. When the output is split by block, the
#'   block number is inserted before the file extension, so `design.csv`
#'   becomes `design_block1.csv`, `design_block2.csv` and so on.
#' @param format The output format. One of `"csv"`, `"tsv"`, `"qualtrics"` or
#'   `"xlsx"`. See Details.
#' @param by_block A logical value. If `TRUE`, one file is written per block.
#'   Ignored by `"xlsx"`, which always writes one sheet per block, and forced
#'   to `TRUE` by `"qualtrics"` when there is more than one block. Defaults to
#'   `FALSE`.
#' @param columns An optional character vector naming the columns to write, in
#'   the order wanted. The default writes every column except `block`, which
#'   is redundant once the output is split.
#' @param bom A logical value. If `TRUE` (the default), a UTF-8 byte order mark
#'   is written at the start of `"csv"` and `"tsv"` files. This is what makes
#'   Japanese, accented and other non-ASCII text display correctly when the
#'   file is double-clicked into Excel on Windows. Set it to `FALSE` for tools
#'   that mistake the mark for data.
#' @param overwrite A logical value. If `FALSE` (the default), the function
#'   refuses to replace an existing file.
#'
#' @return A character vector of the paths written, invisibly.
#'
#' @details
#' `"csv"` and `"tsv"` write the object as it stands, one row per vignette or
#' per respondent-position.
#'
#' `"qualtrics"` writes a loop table: one row per loop iteration, a leading
#' `Loop` column numbering the iterations, and one column per field. This is
#' the shape the Loop & Merge editor expects, so the file can be opened in a
#' spreadsheet and pasted in, with field 1 of the loop referring to the first
#' attribute column. Qualtrics does not publish a documented file-import
#' specification for Loop & Merge, so this is a table shaped for pasting rather
#' than a format guaranteed to import unchanged; check the field order once
#' before fielding. When the design has several blocks, one file per block is
#' written, since a loop belongs to a single block.
#'
#' `"xlsx"` writes a single workbook with one sheet per block, named
#' `block1`, `block2` and so on. This is the format fieldwork agencies most
#' often ask for. It requires the \pkg{openxlsx} package.
#'
#' @seealso [fs_defficient()], [fs_assign()], [fs_vignette_text()]
#'
#' @importFrom tools file_ext
#' @export
#' @examples
#' d <- fs_defficient(list(damage = c("minor", "severe"),
#'                         cost   = c(100, 500, 1000)),
#'                    n_vignettes = 6, n_blocks = 2,
#'                    seed = 1, verbose = FALSE)
#' d <- fs_vignette_text(d, "Damage is {{damage}}; the cost is {{cost}}.")
#'
#' # Write into the session's temporary directory
#' path <- file.path(tempdir(), "design.csv")
#' fs_export(d, path)
#' read.csv(path, fileEncoding = "UTF-8-BOM")[1:2, ]
#'
#' # One Qualtrics loop table per block
#' files <- fs_export(d, file.path(tempdir(), "loop.csv"), format = "qualtrics")
#' basename(files)
#'
#' unlink(c(path, files))
fs_export <- function(x,
                      file,
                      format = c("csv", "tsv", "qualtrics", "xlsx"),
                      by_block = FALSE,
                      columns = NULL,
                      bom = TRUE,
                      overwrite = FALSE) {

  format <- match.arg(format)
  if (!is.data.frame(x)) {
    stop("`x` must be a data frame, an `fs_design` or an `fs_assignment`.",
         call. = FALSE)
  }
  if (!is.character(file) || length(file) != 1L || is.na(file)) {
    stop("`file` must be a single file path.", call. = FALSE)
  }
  check_flag(by_block)
  check_flag(bom)
  check_flag(overwrite)

  body <- as.data.frame(x)
  if (nrow(body) == 0L) {
    stop("`x` has no rows to write.", call. = FALSE)
  }
  if (!is.null(columns)) {
    unknown <- setdiff(columns, names(body))
    if (length(unknown) > 0L) {
      stop("Column(s) not found in `x`: ", paste(unknown, collapse = ", "),
           call. = FALSE)
    }
  }

  has_block <- "block" %in% names(body)
  blocks <- if (has_block) sort(unique(body$block)) else 1L
  if (!has_block) {
    body$block <- 1L
  }

  if (format == "qualtrics" && length(blocks) > 1L) {
    by_block <- TRUE
  }
  if (format == "xlsx") {
    return(invisible(
      export_xlsx(body, file, blocks, columns, overwrite)
    ))
  }

  ## ---- Split and write ------------------------------------------------------
  paths <- if (by_block && length(blocks) > 1L) {
    vapply(blocks, function(b) block_path(file, b), character(1L))
  } else {
    file
  }
  parts <- if (by_block && length(blocks) > 1L) {
    lapply(blocks, function(b) body[body$block == b, , drop = FALSE])
  } else {
    list(body)
  }

  for (p in paths) {
    if (file.exists(p) && !overwrite) {
      stop("'", p, "' already exists. Use `overwrite = TRUE` to replace it.",
           call. = FALSE)
    }
  }

  for (i in seq_along(parts)) {
    tbl <- prepare_table(parts[[i]], columns, format,
                         drop_block = by_block && length(blocks) > 1L)
    write_delim_utf8(tbl, paths[[i]],
                     sep = if (format == "tsv") "\t" else ",",
                     bom = bom)
  }

  invisible(paths)
}


#' Select and reshape the columns for one output file
#'
#' @param part A data frame for one block, or the whole object.
#' @param columns Optional column selection.
#' @param format The output format.
#' @param drop_block Whether the `block` column is redundant.
#' @return A data frame ready to be written.
#' @noRd
prepare_table <- function(part, columns, format, drop_block) {
  keep <- if (!is.null(columns)) {
    columns
  } else {
    setdiff(names(part), if (drop_block) "block" else character(0L))
  }
  tbl <- part[, keep, drop = FALSE]
  rownames(tbl) <- NULL

  if (format == "qualtrics") {
    tbl <- cbind(Loop = seq_len(nrow(tbl)), tbl)
  }
  tbl
}


#' Insert a block number before a file extension
#'
#' @param file The base path.
#' @param b The block number.
#' @return The path with `_block<b>` inserted.
#' @noRd
block_path <- function(file, b) {
  ext <- tools::file_ext(file)
  if (nzchar(ext)) {
    sub(paste0("\\.", ext, "$"), paste0("_block", b, ".", ext), file)
  } else {
    paste0(file, "_block", b)
  }
}


#' Write a data frame as UTF-8 delimited text, optionally with a BOM
#'
#' The rows are assembled as UTF-8 text and written as raw bytes.
#' [utils::write.table()] cannot be used here: it converts strings to the
#' session's native encoding first, so on a machine whose locale cannot
#' represent the characters -- a C locale, or a Japanese Windows session for
#' text outside its code page -- it emits `<U+XXXX>` escapes or, worse, drops
#' the field entirely with a conversion warning. Writing the bytes directly
#' makes the output identical in every locale.
#'
#' @param tbl The data frame.
#' @param path The destination path.
#' @param sep The field separator.
#' @param bom Whether to prepend a UTF-8 byte order mark.
#' @return `path`, invisibly.
#' @noRd
write_delim_utf8 <- function(tbl, path, sep = ",", bom = TRUE) {
  ## The names are dropped before do.call(): they would be matched as argument
  ## names to paste(), and R translates argument names to the native encoding,
  ## which warns whenever a column name is outside it.
  fields <- unname(lapply(tbl, format_field))
  header <- quote_field(enc2utf8(names(tbl)))

  lines <- c(
    paste(header, collapse = sep),
    do.call(paste, c(fields, list(sep = sep)))
  )

  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  if (bom) {
    writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con)
  }
  writeLines(enc2utf8(lines), con, useBytes = TRUE)

  invisible(path)
}


#' Render one column as delimited-text fields
#'
#' Character and factor columns are quoted, as [utils::write.csv()] does;
#' numbers and logicals are not. Missing values become empty fields.
#'
#' @param x A column of a data frame.
#' @return A character vector of the same length.
#' @noRd
format_field <- function(x) {
  if (is.factor(x)) {
    x <- as.character(x)
  }
  if (is.character(x)) {
    out <- quote_field(enc2utf8(x))
  } else if (is.logical(x)) {
    out <- ifelse(x, "TRUE", "FALSE")
  } else {
    out <- vapply(x, function(v) {
      if (is.na(v)) NA_character_ else format(v, trim = TRUE,
                                              scientific = FALSE)
    }, character(1L))
  }
  out[is.na(x)] <- ""
  out
}


#' Wrap text in double quotes, doubling any quotes inside
#'
#' @param x A character vector, already UTF-8.
#' @return A character vector of quoted fields.
#' @noRd
quote_field <- function(x) {
  escaped <- gsub('"', '""', x, fixed = TRUE, useBytes = TRUE)
  paste0('"', escaped, '"')
}


#' Write one workbook with a sheet per block
#'
#' @param body The full data frame, carrying a `block` column.
#' @param file The destination path.
#' @param blocks The block identifiers.
#' @param columns Optional column selection.
#' @param overwrite Whether an existing file may be replaced.
#' @return The path written.
#' @noRd
export_xlsx <- function(body, file, blocks, columns, overwrite) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required for `format = \"xlsx\"`. Install it ",
         "with install.packages('openxlsx'), or export to CSV instead.",
         call. = FALSE)
  }
  if (file.exists(file) && !overwrite) {
    stop("'", file, "' already exists. Use `overwrite = TRUE` to replace it.",
         call. = FALSE)
  }

  wb <- openxlsx::createWorkbook()
  for (b in blocks) {
    part <- body[body$block == b, , drop = FALSE]
    tbl <- prepare_table(part, columns, "xlsx",
                         drop_block = length(blocks) > 1L)
    sheet <- paste0("block", b)
    openxlsx::addWorksheet(wb, sheet)
    openxlsx::writeData(wb, sheet, tbl)
    openxlsx::freezePane(wb, sheet, firstRow = TRUE)
    openxlsx::setColWidths(wb, sheet, cols = seq_len(ncol(tbl)),
                           widths = "auto")
  }
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)

  file
}
