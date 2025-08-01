#' @title Generate D-Efficient Designs for Factorial Surveys
#' @description Generates an optimal set of profiles (vignettes) for a factorial
#'   survey experiment based on the D-efficiency criterion. This function
#'   handles the extraction, blocking, and shuffling of vignettes.
#'
#' @param factors A list defining the factors and their levels.
#' @param n_vignettes The total number of profiles (vignettes) to generate.
#' @param n_blocks The number of blocks (sets to be shown to respondents).
#'   \code{n_vignettes} must be divisible by \code{n_blocks}.
#' @param seed A random seed for reproducibility. Set to \code{NULL} for no seed.
#' @param n_repeats The number of search attempts for the D-efficient design.
#' @param shuffle_within_blocks A logical value indicating whether to randomize
#'   the row order within each block.
#' @param export_csv A logical value indicating whether to save the results as a
#'   CSV file.
#' @param file_name The file name for the CSV. If not specified, it will be
#'   named automatically.
#' @param verbose A logical value. If TRUE, prints progress updates.
#'
#' @return A data frame (of class \code{fse_design}) containing the D-efficiently
#'   selected profiles and a block column.
#'
#' @importFrom utils write.csv
#' @export
fs_defficient <- function(
    factors,
    n_vignettes,
    n_blocks = 1,
    seed = NULL,
    n_repeats = 50,
    shuffle_within_blocks = TRUE,
    export_csv = FALSE,
    file_name = NULL,
    verbose = TRUE
) {
  # --- Input Validation ---
  if (!is.list(factors)) {
    stop("Argument 'factors' must be a list.")
  }
  if (n_vignettes %% n_blocks != 0) {
    stop("Argument 'n_vignettes' must be divisible by 'n_blocks'.")
  }

  # --- Full Factorial Design ---
  full_factorial <- expand.grid(factors, KEEP.OUT.ATTRS = FALSE)
  total_combinations <- nrow(full_factorial)
  if (verbose) {
    message("Total number of combinations: ", total_combinations)
  }

  if (n_vignettes > total_combinations) {
    stop("The number of vignettes to generate exceeds the total number of combinations.")
  }

  # --- D-Efficient Design Selection ---
  if (!is.null(seed)) {
    set.seed(seed)
  }
  if (verbose) {
    message("Extracting ", n_vignettes, " profiles based on D-efficiency criterion...")
  }

  efficient_result <- AlgDesign::optFederov(
    frml = ~.,
    data = full_factorial,
    nTrials = n_vignettes,
    criterion = "D",
    nRepeats = n_repeats
  )
  design <- as.data.frame(efficient_result$design)

  if (verbose) {
    message("D-efficiency: ", round(efficient_result$D, 4))
  }

  # --- Manual Blocking (Robust Method) ---
  if (n_blocks > 1) {
    if (verbose) {
      message("Dividing into ", n_blocks, " blocks...")
    }
    # Randomly shuffle the rows before assigning blocks
    shuffled_rows <- design[sample(nrow(design)), ]
    
    # Assign block numbers
    vignettes_per_block <- n_vignettes / n_blocks
    shuffled_rows$block <- rep(1:n_blocks, each = vignettes_per_block)
    design <- shuffled_rows
  } else {
    design$block <- 1
  }

  # --- Shuffle within blocks (to avoid order effects) ---
  if (shuffle_within_blocks) {
    # Use a stable Base R method for shuffling within groups
    design_list <- split(design, design$block)
    shuffled_list <- lapply(design_list, function(sub_df) {
      sub_df[sample(nrow(sub_df)), , drop = FALSE]
    })
    design <- do.call(rbind, shuffled_list)
    rownames(design) <- NULL # Reset row names for consistency
  }

  # --- Assign attributes ---
  attr(design, "factors") <- factors
  class(design) <- c("fse_design", "data.frame")

  # --- CSV output ---
  if (export_csv) {
    if (is.null(file_name)) {
      file_name <- paste0("fse_design_", format(Sys.Date(), "%Y%m%d"), ".csv")
    }
    write.csv(design, file = file_name, row.names = FALSE, fileEncoding = "UTF-8")
    if (verbose) {
      message("Saved as CSV: ", file_name)
    }
  }

  if (verbose) {
    message("Processing complete.\n")
  }
  return(design)
}