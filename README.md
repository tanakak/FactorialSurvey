# FactorialSurvey

<!-- badges: start -->
[![R-CMD-check](https://github.com/tanakak/FactorialSurvey/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tanakak/FactorialSurvey/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

An integrated workflow for **factorial survey (vignette) experiments** in R.

A factorial survey shows respondents short descriptions whose attributes are
experimentally varied, and asks for a rating -- typically on a bounded scale
such as 0 to 10 -- or a yes/no answer. Because the attributes are randomised by design, their
effects on the rating carry a causal interpretation. `FactorialSurvey` covers
the stages of such a study in one place: building an efficient set of
vignettes, checking how many respondents are needed, estimating a model suited
to bounded ratings or yes/no answers with standard errors clustered by
respondent, and translating the estimates into quantities that can be
reported: marginal willingness to pay for each attribute and expected
willingness to pay for complete policy scenarios.

See `vignette("FactorialSurvey")` for a walk-through with real data.

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

`fs_design_eval()` reports the D- and A-efficiency (100 percent for an
orthogonal array), how evenly the levels appear and how strongly the
attributes are correlated. `fs_efficiency()` gives the efficiency of any
design, including one built elsewhere.

```r
fs_design_eval(design)
```

Before fielding, `fs_power()` simulates the survey under effects you expect
and reports how often each would be detected, and how precise willingness to
pay would be, for several sample sizes.

`set_labels()` attaches the wording the respondent will actually see:

```r
design <- set_labels(
  as.data.frame(design),
  list(damage = c("Minor", "Moderate", "Severe"),
       distance = c("Nearby", "Far away"))
)
```

### 3. Write the vignettes and schedule the respondents

`fs_vignette_text()` fills a template with each row's levels. Formatting
functions render numbers the way respondents should read them, without
altering the design columns.

```r
design <- fs_vignette_text(
  design,
  "A {{damage}} damaged building {{distance}}. Repairs cost {{cost}} a year.",
  format = list(cost = function(x) paste0("$", formatC(x, big.mark = ",")))
)
```

`fs_assign()` deals blocks out to respondents and orders the vignettes within
each block using a Williams design, so that no attribute is confounded with
position or with the vignette a respondent happened to see just before.

```r
schedule <- fs_assign(design, n_respondents = 200, seed = 1)
```

`fs_export()` writes the result out. CSV files carry a UTF-8 byte order mark
by default, which is what makes non-ASCII text open correctly in Excel on
Windows.

```r
fs_export(design, "vignettes.csv")
fs_export(design, "loop.csv", format = "qualtrics")   # one file per block
fs_export(schedule, "schedule.xlsx", format = "xlsx") # one sheet per block
```

### 4. Fit the model

`fs_betareg()` maps the rating scale onto the open unit interval and fits a
beta regression, which respects the bounds of the scale instead of assuming
an unbounded normal error.

```r
fit <- fs_betareg(rating ~ damage + distance + cost,
                  data    = survey_data,
                  scale   = c(0, 10),
                  cluster = ~ respondent_id)

summary(fit)
```

Because each respondent rates several vignettes, the ratings of one person
are not independent. `cluster` makes standard errors, marginal effects and
willingness-to-pay intervals robust to this; without it `fs_betareg()` warns.

Average marginal effects are reported both on the unit interval and rescaled
back onto the original 0--10 scale, so they can be read as "this attribute
moves the rating by so many points".

### 5. Willingness to pay

```r
w <- fs_mwtp(fit, price = "cost")
plot(w)
fs_mwtp(fit, price = "cost", method = "krinsky-robb", seed = 1)

# Expected willingness to pay for complete scenarios
scenarios <- data.frame(damage   = c("minor", "severe"),
                        distance = c("far", "near"),
                        row.names = c("mild", "serious"))
fs_ewtp(fit, scenarios, price = "cost")
```

MWTP is a ratio of coefficients, so its sampling distribution is skewed. Three
interval methods are offered -- the delta method, Krinsky-Robb simulation and
Fieller's theorem -- and it is worth comparing them before reporting a result.

## Scope

| Stage | Functions |
|---|---|
| Design | `fs_defficient()`, `fs_efficiency()`, `fs_design_eval()` |
| Planning | `fs_simulate_ratings()`, `fs_power()` |
| Fielding | `set_labels()`, `fs_vignette_text()`, `fs_williams()`, `fs_assign()`, `fs_export()` |
| Estimation | `fs_betareg()` (ratings), `fs_logit()` (yes/no) |
| Willingness to pay | `fs_mwtp()`, `fs_ewtp()` |
| Graphics | `plot()` methods, `theme_fs()` |
| Data | `bento` |

## Roadmap

Planned for future releases:

- ordered beta regression for ratings that pile up at the endpoints
- respondent-level heterogeneity: multilevel, random-parameter and
  finite-mixture models
- scenario prediction and welfare calculation

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
