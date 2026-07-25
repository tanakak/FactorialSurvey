#' @title Attach readable level labels to design or survey variables
#'
#' @description
#' Converts one or more columns of a data frame into factors with explicit
#' labels. This is normally applied to a design produced by
#' [fs_defficient()], whose columns carry the level *codes* used in the
#' questionnaire rather than the wording shown to respondents, or to a returned
#' data set in which the attributes are stored as integer codes.
#'
#' @param df A data frame.
#' @param label_list A named list of character vectors. Each name must match a
#'   column of `df`, and each element gives the labels for that column's
#'   levels, in the order given by `levels_list`.
#' @param levels_list An optional named list giving, for each variable, the
#'   values in `df` that the labels correspond to. Names must be a subset of
#'   the names of `label_list`. When a variable is not listed, the values are
#'   taken to be `1, 2, ..., length(labels)` if the column is numeric, and
#'   `sort(unique(x))` otherwise.
#' @param strict A logical value. If `TRUE` (the default), an error is thrown
#'   when a column contains values that are not covered by the supplied levels.
#'   If `FALSE`, uncovered values become `NA` with a warning.
#'
#' @return The input data frame, with the named columns replaced by labelled
#'   factors. Other columns and all attributes of `df` are left unchanged.
#'
#' @seealso [fs_defficient()] for generating the design these labels describe.
#'
#' @examples
#' d <- data.frame(alert = c(1, 2, 1), timing = c(2, 1, 2), rating = c(7, 3, 8))
#'
#' # Numeric codes 1, 2 are matched to labels in order
#' set_labels(d, list(alert = c("Low", "High"),
#'                    timing = c("Morning", "Evening")))
#'
#' # Codes that do not start at 1 are given explicitly
#' d0 <- data.frame(alert = c(0, 1, 0))
#' set_labels(d0, list(alert = c("Absent", "Present")),
#'            levels_list = list(alert = c(0, 1)))
#'
#' @export
set_labels <- function(df, label_list, levels_list = NULL, strict = TRUE) {
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.", call. = FALSE)
  }
  if (!is.list(label_list) || is.null(names(label_list)) ||
      any(names(label_list) == "")) {
    stop("`label_list` must be a named list of label vectors.", call. = FALSE)
  }
  if (!is.null(levels_list)) {
    if (!is.list(levels_list) || is.null(names(levels_list))) {
      stop("`levels_list` must be a named list, or NULL.", call. = FALSE)
    }
    unknown <- setdiff(names(levels_list), names(label_list))
    if (length(unknown) > 0L) {
      stop("`levels_list` names not found in `label_list`: ",
           paste(unknown, collapse = ", "), call. = FALSE)
    }
  }
  check_flag(strict)

  missing_vars <- setdiff(names(label_list), colnames(df))
  if (length(missing_vars) > 0L) {
    stop("Variable(s) not found in `df`: ",
         paste(missing_vars, collapse = ", "), call. = FALSE)
  }

  for (var in names(label_list)) {
    labels <- label_list[[var]]
    x <- df[[var]]

    if (var %in% names(levels_list)) {
      lev <- levels_list[[var]]
    } else if (is.factor(x)) {
      lev <- levels(x)
    } else if (is.numeric(x)) {
      lev <- seq_along(labels)
    } else {
      lev <- sort(unique(x))
    }

    if (length(lev) != length(labels)) {
      stop("For variable '", var, "', ", length(labels), " label(s) were ",
           "supplied but ", length(lev), " level(s) were found. ",
           "Use `levels_list` to state the levels explicitly.", call. = FALSE)
    }

    uncovered <- setdiff(unique(x[!is.na(x)]), lev)
    if (length(uncovered) > 0L) {
      msg <- paste0("Variable '", var, "' contains value(s) not covered by ",
                    "the supplied levels: ",
                    paste(utils::head(uncovered, 5L), collapse = ", "))
      if (strict) {
        stop(msg, call. = FALSE)
      }
      warning(msg, "; these become NA.", call. = FALSE)
    }

    df[[var]] <- factor(x, levels = lev, labels = labels)
  }

  df
}
