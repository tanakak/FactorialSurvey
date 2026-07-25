# cran-comments

## Submission summary

This is a new submission.

## Test environments

- local: Ubuntu 24.04, R 4.3.3
- GitHub Actions: ubuntu-latest (devel, release, oldrel-1), macOS-latest
  (release), windows-latest (release)
- win-builder: R-devel and R-release
- R-hub: (fill in once run)

## R CMD check results

0 errors | 0 warnings | 1 note

> New submission

## Notes for the reviewers

- `fs_defficient()` and `fs_mwtp()` accept a `seed` argument. Both capture
  `.Random.seed` on entry and restore it with `on.exit()`, so the caller's
  random number stream is left untouched.
- No function writes to the file system, the user's home directory, or the
  global environment.
- `margins` is used conditionally and guarded with
  `requireNamespace("margins", quietly = TRUE)`; the package checks cleanly
  with `_R_CHECK_DEPENDS_ONLY_=true`.
- Examples run in well under five seconds each.

## Downstream dependencies

There are no reverse dependencies.
