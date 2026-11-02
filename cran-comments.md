# cran-comments

## Submission summary

This is a new submission.

## Test environments

- local: macOS, R 4.6.2
- local: Ubuntu 24.04, R 4.3.3
- GitHub Actions: ubuntu-latest (devel, release, oldrel-1), macos-latest
  (release), windows-latest (release)
- win-builder: R-devel (to be run before submission)

## R CMD check results

0 errors | 0 warnings | 1 note

> New submission

## Notes for the reviewers

- Functions with a `seed` argument (`fs_defficient()`, `fs_assign()`,
  `fs_simulate_ratings()`, `fs_power()`, `fs_mwtp()` and `fs_ewtp()`)
  save `.Random.seed` on entry and restore it with `on.exit()`, so the
  caller's random number stream is left as it was.
- `fs_export()` writes only to the file path given by the user. Examples,
  tests and the vignette write to `tempdir()` or not at all. No function
  writes to the user's home directory or changes options or the working
  directory.
- The suggested packages 'margins' and 'openxlsx' are used conditionally,
  guarded by `requireNamespace()`; the package checks cleanly with
  `_R_CHECK_DEPENDS_ONLY_=true`.
- Every example runs in under three seconds.
- The documentation uses British spelling, declared with `Language: en-GB`.

## Downstream dependencies

There are no reverse dependencies.
