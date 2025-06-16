#' Efficient Experimental Design Generator for Factorial Survey
#'
#' Generates a D-efficient fractional factorial design based on given attribute levels.
#' Allows group assignment with approximately balanced attribute-level combinations.
#'
#' @param levels Integer vector specifying the number of levels for each attribute.
#' @param n_designs Number of designs (vignettes) to be generated.
#' @param n_groups Number of groups to divide the vignettes into (optional).
#' @param var_names Optional character vector specifying names for the attributes.
#' @param seed Optional seed for reproducibility.
#' @param check_balance Logical, if TRUE, print cross-tabulation of level distribution by group.
#'
#' @return A data.frame of D-efficient design with optional group assignment.
#' @export
fs_defficient <- function(levels, n_designs = 18, n_groups = NULL,
                          var_names = NULL, seed = NULL, check_balance = FALSE) {
  if (!requireNamespace("AlgDesign", quietly = TRUE)) {
    stop("Please install the 'AlgDesign' package first: install.packages('AlgDesign')")
  }

  if (!is.numeric(levels) || any(levels < 2)) {
    stop("All elements in 'levels' must be integers >= 2.")
  }

  if (!is.null(var_names) && length(var_names) != length(levels)) {
    stop("Length of 'var_names' must match the length of 'levels'.")
  }

  if (!is.null(n_groups) && (n_designs %% n_groups != 0)) {
    warning("'n_designs' is not divisible by 'n_groups'. Groups will be approximately equal.")
  }

  if (!is.null(seed)) set.seed(seed)

  if (is.null(var_names)) {
    var_names <- paste0("x", seq_along(levels))
  }

  level_list <- lapply(levels, function(l) factor(seq_len(l)))
  names(level_list) <- var_names
  full_design <- expand.grid(level_list)

  design <- AlgDesign::optFederov(
    data = full_design,
    nTrials = n_designs,
    criterion = "D"
  )$design

  if (!is.null(n_groups)) {
    design$group <- rep(1:n_groups, length.out = nrow(design))

    if (check_balance) {
      message("=== Group Balance Check ===")
      for (v in var_names) {
        print(table(design[[v]], design$group))
      }
    }
  }

  rownames(design) <- NULL
  return(design)
}
