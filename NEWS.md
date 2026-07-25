# FactorialSurvey 0.2.0

The design side of the workflow is now complete: a design can be built,
allocated to respondents in a balanced order, rendered as prose, and written
out for a survey platform without leaving R.

## New features

* `fs_defficient()` gains a `blocking` argument. The new default,
  `"optimal"`, allocates vignettes to blocks with `AlgDesign::optBlock()`, so
  each block is as informative as it can be on its own; `"random"` keeps the
  previous deal-them-out behaviour. The method used and the blocked
  D-criterion are recorded on the returned object. When the search fails the
  function falls back to random allocation and says so rather than erroring.

* `fs_williams()` constructs a Williams design: a set of presentation orders
  in which every vignette appears equally often in every position **and**
  every ordered pair of neighbours occurs equally often. The second property
  is what an ordinary Latin square lacks, and it is what keeps a
  carryover effect from confounding an attribute.

* `fs_assign()` turns a design into a respondent schedule, one row per
  respondent per vignette, dealing blocks out evenly and cycling through
  Williams orders. Returns an `fs_assignment` object with a `print()` method.

* `fs_vignette_text()` fills a `{{attribute}}` template with each row's levels
  to produce the sentence shown to respondents. Per-attribute formatting
  functions allow numbers to be rendered as currency and the like without
  touching the design columns.

* `fs_export()` writes a design or schedule out as CSV, TSV, a Qualtrics
  Loop & Merge table, or an Excel workbook with one sheet per block. A UTF-8
  byte order mark is written by default so that non-ASCII text opens
  correctly in Excel on Windows.

## Bug fixes

* Text export no longer depends on the session locale. The previous approach
  went through `utils::write.table(fileEncoding = "UTF-8")`, which converts
  strings to the native encoding first: in a C locale, or on a Windows
  session whose code page cannot represent the text, that emitted
  `<U+XXXX>` escapes or silently dropped fields with a conversion warning.
  Rows are now assembled as UTF-8 and written as raw bytes, so the output is
  byte-identical in every locale.

* Column names outside the native encoding no longer trigger an "unable to
  translate to native encoding" warning during export.

## Notes

* `openxlsx` is a new optional dependency, in `Suggests`, needed only for
  `format = "xlsx"`.

* Non-ASCII text in the test suite is written with `\uxxxx` escapes so the
  tests mean the same thing under any locale. The suite passes identically
  under `C` and `en_US.UTF-8`.


# FactorialSurvey 0.1.0

First release with a stable public interface. Everything below is relative to
the unreleased development code.

## Bug fixes

* `fs_betareg()` reported a broken pseudo R-squared. It read a
  `loglik.null` component that `betareg` objects do not have, so the value was
  `numeric(0)` and nothing was printed -- silently, with no error. The null
  log-likelihood now comes from an intercept-only model that the function fits
  for the purpose.

* The reported pseudo R-squared is no longer McFadden's. McFadden's
  \eqn{1 - \ell / \ell_0} assumes negative log-likelihoods, which holds for
  discrete outcomes but not for a continuous density like the beta. The
  measure of Cox and Snell is reported instead, alongside the squared
  correlation that `betareg` itself reports, and a likelihood-ratio test
  against the intercept-only model.

* `fs_defficient()` called `set.seed()` and left the caller's random number
  stream altered. The previous state of `.Random.seed` is now restored when
  the function returns, as CRAN policy requires. The same applies to the new
  simulation-based interval in `fs_mwtp()`.

* A stray duplicate of `fs_betareg()` under a second file name defined the
  function twice. It has been removed.

* `tests/testthat/test-fs_defficient.R` contained an old copy of the function
  source rather than any tests, so running the suite silently redefined
  `fs_defficient()`. It has been replaced with real tests.

## Renamed

* `set_lavels()` is now `set_labels()`. The old name was a typo. Because the
  package has not been released, no deprecated alias is provided.

## New features

* `fs_defficient()` gains `model`, for making a design efficient for
  interactions as well as main effects; `restrictions`, for excluding
  combinations that would not make sense to a respondent; and
  `max_candidates`, which stops an accidentally enormous factorial from
  exhausting memory. It returns an object of class `fs_design` with a `print()`
  method and the D-efficiency attached.

* `fs_design_eval()` reports level balance, level balance by block, and the
  correlation matrix of the attributes, with a note when correlations are
  large enough to make effects hard to separate.

* `fs_betareg()` gains a `scale` argument, so rating scales other than 0--10
  are supported, including 1--7 Likert items and 0--100 sliders. It returns an
  object of class `fs_betareg` with `print()`, `summary()`, `coef()`,
  `vcov()`, `logLik()`, `nobs()` and `fitted()` methods, and no longer prints
  to the console as a side effect of fitting.

* `fs_mwtp()` computes willingness to pay for every attribute at once when
  `variable` is left unspecified, adds Krinsky-Robb and Fieller intervals
  alongside the delta method, gains a `level` argument, and accepts a plain
  `betareg` fit as well as an `fs_betareg` object. It returns an object of
  class `fs_mwtp` with `print()` and `as.data.frame()` methods.

* `set_labels()` gains `levels_list`, for level codes that do not start at
  one, and `strict`, controlling whether values outside the supplied levels
  are an error or a warning. Factor and character columns are handled as well
  as numeric codes.

## Other changes

* The `margins` and `msm` packages moved from `Imports` to `Suggests`.
  `margins` has not been updated since 2017 and is used conditionally;
  `msm::deltamethod` was replaced by a direct calculation of the gradient,
  which removes the dependency altogether.

* Neither `fs_defficient()` nor any other function writes files. Pass the
  design to `utils::write.csv()` yourself if you want it on disk.

* Package documentation, a README, and a test suite covering all exported
  functions were added.
