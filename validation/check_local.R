## =============================================================================
## Check that the local copy of FactorialSurvey works on this computer.
##
## In RStudio: set the working directory to the package folder (the one that
## contains DESCRIPTION), then run
##
##     source("validation/check_local.R")
##
## Steps (about 5-10 minutes in total):
##   1. install any missing packages that FactorialSurvey needs
##   2. R CMD check --as-cran on the local folder (as CRAN would)
##   3. install the local version
##   4. run the validation script in a fresh R session (quick mode)
## =============================================================================

if (!file.exists("DESCRIPTION") ||
    !any(grepl("^Package: FactorialSurvey", readLines("DESCRIPTION")))) {
  stop("Set the working directory to the FactorialSurvey folder first, e.g.\n",
       "  setwd(\"~/Library/CloudStorage/GoogleDrive-katsuyat@gmail.com/",
       "マイドライブ/Projects/FactorialSurvey\")",
       call. = FALSE)
}

cat("== Step 1/4: packages ==\n")
## Every package FactorialSurvey imports or suggests, read from DESCRIPTION,
## plus the two used by this script.
deps <- read.dcf("DESCRIPTION", fields = c("Imports", "Suggests"))
deps <- unlist(strsplit(paste(deps[!is.na(deps)], collapse = ","), ","))
deps <- trimws(sub("\\(.*\\)", "", deps))
base_pkgs <- rownames(installed.packages(priority = "base"))
needed <- setdiff(unique(c(deps[nzchar(deps)], "rcmdcheck", "pkgbuild")),
                  base_pkgs)
missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  cat("Installing:", paste(missing, collapse = ", "), "\n")
  install.packages(missing)
} else {
  cat("All required packages are installed.\n")
}

cat("\n== Step 2/4: R CMD check --as-cran (takes a few minutes) ==\n")
chk <- rcmdcheck::rcmdcheck(".", args = c("--as-cran", "--no-manual"),
                            error_on = "never", quiet = TRUE,
                            check_dir = tempfile("fs_check_"))
cat(sprintf("Errors: %d  Warnings: %d  Notes: %d\n",
            length(chk$errors), length(chk$warnings), length(chk$notes)))
for (x in c(chk$errors, chk$warnings, chk$notes)) cat("---\n", x, "\n")

cat("\n== Step 3/4: install the local version ==\n")
tarball <- pkgbuild::build(".", dest_path = tempdir(), vignettes = FALSE,
                           quiet = TRUE)
install.packages(tarball, repos = NULL, type = "source", quiet = TRUE)
cat("Installed FactorialSurvey",
    as.character(utils::packageDescription("FactorialSurvey")$Version), "\n")

cat("\n== Step 4/4: validation script (quick mode, about 3 minutes) ==\n")
rscript <- file.path(R.home("bin"), "Rscript")
out <- system2(rscript, "validation/validate_FactorialSurvey.R",
               env = "FS_VALIDATION_QUICK=true", stdout = TRUE, stderr = TRUE)
cat(out, sep = "\n")

cat("\n== Summary ==\n")
passed_line <- grep("checks passed", out, value = TRUE)
cat("R CMD check :", sprintf("%d errors, %d warnings, %d notes",
                             length(chk$errors), length(chk$warnings),
                             length(chk$notes)), "\n")
cat("Validation  :", if (length(passed_line)) passed_line else "did not finish", "\n")
cat("NOTEs about 'New submission', 'Version contains large components'",
    "(the development version number) or 'future file timestamps' are",
    "expected and harmless. Before the package is pushed to GitHub, the",
    "README badge URL is also reported as 404; that goes away after the push.\n")
cat("After this, restart R (Session > Restart R) before using the package.\n")
