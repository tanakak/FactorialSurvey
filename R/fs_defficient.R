#' Generate a D-efficient fractional factorial design and assign to groups
#'
#' @param levels A numeric vector indicating the number of levels for each attribute.
#' @param n_designs Integer. The number of profiles to select (e.g., 18).
#' @param n_groups Integer. The number of groups to split the design into.
#' @param var_names A character vector of names for the variables (same length as levels).
#' @param seed Integer. Seed for reproducibility (default: NULL).
#' @param check_balance Logical. If TRUE, prints the distribution of attribute levels by group.
#'
#' @return A data.frame containing the selected design with a "group" column.
#' @export
#'
#' @examples
#' fs_defficient(levels = c(3, 2, 2), n_designs = 12, n_groups = 3,
#'               var_names = c("危険度", "時間", "避難行動"), seed = 123)
fs_defficient <- function(levels,
                          n_designs,
                          n_groups,
                          var_names = NULL,
                          seed = NULL,
                          check_balance = FALSE) {

  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Generate full factorial design
  level_lists <- lapply(levels, function(l) seq_len(l))
  full_design <- expand.grid(level_lists)
  colnames(full_design) <- if (!is.null(var_names)) var_names else paste0("X", seq_along(levels))

  # Check if requested number of designs exceeds possible combinations
  if (n_designs > nrow(full_design)) {
    stop("n_designs must not be greater than the number of possible combinations.")
  }

  # Use D-efficient design selection
  result <- AlgDesign::optFederov(data = full_design,
                                  nTrials = n_designs,
                                  criterion = "D",
                                  approximate = FALSE)

  design <- result$design
  d_efficiency <- result$D

  # Assign groups evenly
  design$group <- rep(1:n_groups, length.out = n_designs)

  # Optional: check attribute balance across groups
  if (check_balance) {
    cat("=== Group Balance Check ===\n")
    for (col in seq_len(ncol(design) - 1)) {
      tab <- table(design[[col]], design$group)
      print(tab)
      cat("\n")
    }
  }

  # Add D-efficiency as attribute
  attr(design, "D_efficiency") <- round(d_efficiency, 6)

  return(design)
}
