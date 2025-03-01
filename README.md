# FactorialSurvey

**FactorialSurvey** is an R package designed for preprocessing and estimating factorial survey experiment (FSE) data.  
It provides functions for estimating logit models, random parameter logit models, and computing marginal willingness to pay (MWTP), with support for panel data structures.

## Installation

To install the development version from GitHub, run:

```r
# Install the remotes package if not already installed
install.packages("remotes")

# Install FactorialSurvey from GitHub
remotes::install_github("tanakak/FactorialSurvey")
```

## Features

fs_logit(): Standard logit model estimation with clustered standard errors.
fs_rplogit(): Random parameter logit model estimation (supports normal and log-normal distributions).
fs_mwtp(): Computes MWTP using estimated parameters.
Supports panel data with multiple responses per respondent.


## Usage
```r
library(FactorialSurvey)

# Example dataset
set.seed(123)
test_data <- data.frame(
  y = sample(0:1, 100, replace = TRUE),
  x1 = rnorm(100),
  x2 = rnorm(100),
  price = runif(100, 1, 10),
  respondent_id = rep(1:20, each = 5)  # Panel structure
)

# Estimate logit model
logit_model <- fs_logit(y ~ x1 + x2 + price, data = test_data, panel_id = "respondent_id")
print(logit_model)

# Compute MWTP for x1 relative to price
fs_mwtp(logit_model, attribute = "x1", price = "price")
```

## Development
This package is under active development. Contributions and feedback are welcome!


##  License
This package is released under the GPL-3 license.
