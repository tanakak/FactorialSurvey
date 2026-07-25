#' @title Turn design rows into vignette prose
#'
#' @description
#' Fills a sentence template with the attribute levels of each row, producing
#' the text that will be shown to respondents. Placeholders are written
#' `{{attribute}}`, and optional formatting functions let numbers be rendered
#' as currency, dates, or anything else, without changing the design itself.
#'
#' @param design An object of class `fs_design` from [fs_defficient()], an
#'   `fs_assignment` from [fs_assign()], or any data frame.
#' @param template A single character string containing the sentence, with
#'   `{{attribute}}` where a level should appear. Whitespace inside the braces
#'   is ignored, so `{{ cost }}` and `{{cost}}` are the same placeholder.
#'   Non-ASCII templates are supported and are treated as UTF-8.
#' @param column The name of the column to add. Defaults to `"vignette_text"`.
#' @param format An optional named list of functions, one per attribute, each
#'   taking the column's values and returning a character vector of the same
#'   length. Use this to render numbers the way respondents should read them.
#' @param na_text The text substituted for missing values.
#'
#' @return The input data frame with the text column added, keeping its class
#'   and attributes. The template is attached as the `template` attribute.
#'
#' @details
#' Substitution is literal: whatever the formatting function returns is placed
#' in the sentence unchanged. Word order, particles and agreement are the
#' template author's responsibility, which keeps the behaviour predictable in
#' any language. Where a level needs different wording in different sentences,
#' the usual approach is to apply [set_labels()] first, or to pass a formatting
#' function here.
#'
#' @seealso [set_labels()] for attaching level labels, [fs_export()] for
#'   writing the result out.
#'
#' @export
#' @examples
#' d <- fs_defficient(list(damage = c("minor", "severe"),
#'                         cost   = c(100, 1000)),
#'                    n_vignettes = 4, seed = 1, verbose = FALSE)
#'
#' d <- fs_vignette_text(
#'   d,
#'   "This building suffered {{damage}} damage. Repair costs {{cost}}."
#' )
#' d$vignette_text
#'
#' # Render the cost as currency without altering the design column
#' d2 <- fs_vignette_text(
#'   d,
#'   "Repairs cost {{cost}} per year.",
#'   column = "cost_sentence",
#'   format = list(cost = function(x) paste0("$", formatC(x, big.mark = ",")))
#' )
#' d2$cost_sentence
fs_vignette_text <- function(design,
                             template,
                             column = "vignette_text",
                             format = NULL,
                             na_text = "") {

  if (!is.data.frame(design)) {
    stop("`design` must be a data frame.", call. = FALSE)
  }
  if (!is.character(template) || length(template) != 1L || is.na(template)) {
    stop("`template` must be a single character string.", call. = FALSE)
  }
  if (!is.character(column) || length(column) != 1L || is.na(column)) {
    stop("`column` must be a single column name.", call. = FALSE)
  }
  if (!is.character(na_text) || length(na_text) != 1L) {
    stop("`na_text` must be a single character string.", call. = FALSE)
  }
  if (!is.null(format)) {
    if (!is.list(format) || is.null(names(format)) || any(names(format) == "")) {
      stop("`format` must be a named list of functions.", call. = FALSE)
    }
    if (!all(vapply(format, is.function, logical(1L)))) {
      stop("Every element of `format` must be a function.", call. = FALSE)
    }
  }

  template <- enc2utf8(template)
  placeholders <- extract_placeholders(template)

  if (length(placeholders) == 0L) {
    stop("`template` contains no {{placeholder}}.", call. = FALSE)
  }
  missing_cols <- setdiff(placeholders, names(design))
  if (length(missing_cols) > 0L) {
    stop("Placeholder(s) with no matching column in `design`: ",
         paste(missing_cols, collapse = ", "),
         ". Available columns: ", paste(names(design), collapse = ", "),
         call. = FALSE)
  }
  unknown_format <- setdiff(names(format), names(design))
  if (length(unknown_format) > 0L) {
    stop("`format` names with no matching column: ",
         paste(unknown_format, collapse = ", "), call. = FALSE)
  }

  ## Normalise `{{ x }}` to `{{x}}` so the substitution can be literal
  normalised <- template
  for (ph in placeholders) {
    normalised <- gsub(placeholder_pattern(ph), paste0("{{", ph, "}}"),
                       normalised, perl = TRUE)
  }

  out <- rep(normalised, nrow(design))
  for (ph in placeholders) {
    values <- design[[ph]]
    fun <- format[[ph]]
    rendered <- if (is.null(fun)) {
      as.character(values)
    } else {
      as.character(fun(values))
    }
    if (length(rendered) != nrow(design)) {
      stop("The formatting function for '", ph, "' returned ",
           length(rendered), " value(s) for ", nrow(design), " row(s).",
           call. = FALSE)
    }
    rendered[is.na(rendered)] <- na_text
    rendered <- enc2utf8(rendered)

    target <- paste0("{{", ph, "}}")
    out <- vapply(
      seq_along(out),
      function(i) gsub(target, rendered[[i]], out[[i]], fixed = TRUE),
      character(1L)
    )
  }

  design[[column]] <- enc2utf8(out)
  attr(design, "template") <- template
  design
}


#' Extract placeholder names from a template
#'
#' @param template A single character string.
#' @return A character vector of unique placeholder names, trimmed.
#' @noRd
extract_placeholders <- function(template) {
  hits <- regmatches(template,
                     gregexpr("\\{\\{\\s*[^{}]+?\\s*\\}\\}", template,
                              perl = TRUE))[[1L]]
  if (length(hits) == 0L) {
    return(character(0L))
  }
  names <- sub("^\\{\\{\\s*", "", hits, perl = TRUE)
  names <- sub("\\s*\\}\\}$", "", names, perl = TRUE)
  unique(names)
}


#' Build a regular expression matching one placeholder, spaces allowed
#'
#' @param name The placeholder name.
#' @return A single regular expression string.
#' @noRd
placeholder_pattern <- function(name) {
  paste0("\\{\\{\\s*", escape_regex(name), "\\s*\\}\\}")
}


#' Escape regular expression metacharacters in a literal string
#'
#' @param x A character vector.
#' @return `x` with metacharacters escaped.
#' @noRd
escape_regex <- function(x) {
  gsub("([.\\\\|()\\[\\]{}^$*+?])", "\\\\\\1", x, perl = TRUE)
}
