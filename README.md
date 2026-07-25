# FactorialSurvey

<!-- badges: start -->
[![R-CMD-check](https://github.com/tanakak/FactorialSurvey/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tanakak/FactorialSurvey/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

An integrated workflow for **factorial survey (vignette) experiments** in R.

A factorial survey shows respondents short descriptions whose attributes are
experimentally varied, and asks for a rating -- typically on a bounded scale
such as 0 to 10. Because the attributes are randomised by design, their
effects on the rating carry a causal interpretation. `FactorialSurvey` covers
the stages of such a study in one place: building an efficient set of
vignettes, estimating a model suited to bounded ratings, and translating the
estimates into quantities that can be reported, including marginal
willingness to pay.

## Installation

```r
# install.packages("remotes")
remotes::install_github("tanakak/FactorialSurvey")
```

## A worked example

### 1. Build the design

`fs_defficient()` picks a D-efficient subset of the full factorial and splits
it into blocks, one per respondent group.

```r
library(FactorialSurvey)

attributes <- list(
  damage   = c("minor", "moderate", "severe"),
  distance = c("near", "far"),
  cost     = c(100, 500, 1000, 3000)
)

design <- fs_defficient(
  attributes,
  n_vignettes = 24,
  n_blocks    = 4,
  seed        = 2026,
  verbose     = FALSE
)

design
```

Combinations that would confuse a respondent can be ruled out before the
search:

```r
design <- fs_defficient(
  attributes,
  n_vignettes  = 24,
  n_blocks     = 4,
  restrictions = ~ damage == "minor" & cost == 3000,
  seed         = 2026,
  verbose      = FALSE
)
```

### 2. Check the design

`fs_design_eval()` reports how evenly the levels appear and how strongly the
attributes are correlated. Large correlations mean the effects involved will
be hard to separate.

```r
fs_design_eval(design)
```

`set_labels()` attaches the wording the respondent will actually see:

```r
design <- set_labels(
  as.data.frame(design),
  list(damage = c("Minor", "Moderate", "Severe"),
       distance = c("Nearby", "Far away"))
)
```

### 3. Fit the model

`fs_betareg()` maps the rating scale onto the open unit interval and fits a
beta regression, which respects the bounds of the scale instead of assuming
an unbounded normal error.

```r
fit <- fs_betareg(rating ~ damage + distance + cost,
                  data  = survey_data,
                  scale = c(0, 10))

summary(fit)
```

Average marginal effects are reported both on the unit interval and rescaled
back onto the original 0--10 scale, so they can be read as "this attribute
moves the rating by so many points".

### 4. Marginal willingness to pay

```r
fs_mwtp(fit, price = "cost")
fs_mwtp(fit, price = "cost", method = "krinsky-robb", seed = 1)
```

MWTP is a ratio of coefficients, so its sampling distribution is skewed. Three
interval methods are offered -- the delta method, Krinsky-Robb simulation and
Fieller's theorem -- and it is worth comparing them before reporting a result.

## Scope

| Stage | Functions |
|---|---|
| Design | `fs_defficient()`, `fs_design_eval()`, `set_labels()` |
| Estimation | `fs_betareg()` |
| Post-estimation | `fs_mwtp()` |

## Roadmap

This is version 0.1.0. Planned for future releases:

- vignette text generation and export to survey platforms
- optimal blocking and control of presentation order
- ordered beta regression for ratings that pile up at the endpoints
- cluster-robust standard errors for respondents who rate several vignettes
- respondent-level heterogeneity: multilevel, random-parameter and
  finite-mixture models
- plotting methods for effects, willingness to pay and heterogeneity
- scenario prediction, simulation and welfare calculation

## Related packages

`FactorialSurvey` is about *rating-based* vignette experiments. For
choice-based designs and analyses see
[`idefix`](https://cran.r-project.org/package=idefix) and
[`cregg`](https://cran.r-project.org/package=cregg). The efficient design
search here is built on
[`AlgDesign`](https://cran.r-project.org/package=AlgDesign) and the estimation
on [`betareg`](https://cran.r-project.org/package=betareg).

## License

MIT
