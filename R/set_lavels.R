#' set_lavels: Assign factor labels to multiple variables
#'
#' @param df A data.frame.
#' @param label_list A named list of label vectors. Each name must match a column in df.
#'
#' @return A data.frame with labeled factor variables.
#' @export
set_lavels <- function(df, label_list) {
  for (var in names(label_list)) {
    if (!var %in% colnames(df)) stop(paste("Variable", var, "not found in data frame."))
    df[[var]] <- factor(df[[var]], levels = seq_along(label_list[[var]]), labels = label_list[[var]])
  }
  return(df)
}
