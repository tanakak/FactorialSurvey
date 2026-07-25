#' @title FactorialSurvey: Design and Analysis of Factorial Survey Experiments
#'
#' @description
#' `FactorialSurvey` supports the full workflow of a factorial survey
#' (vignette) experiment. A *factorial survey* presents respondents with short
#' descriptions ("vignettes") whose attributes are experimentally varied, and
#' asks for a rating -- typically on a bounded scale such as 0 to 10. Because
#' the attributes are randomised by design, their effects on the rating can be
#' given a causal interpretation.
#'
#' The package is organised around three stages.
#'
#' \strong{1. Design.} [fs_defficient()] selects a D-efficient subset of
#' vignettes from the full factorial and splits it into blocks;
#' [fs_design_eval()] reports level balance and attribute correlations;
#' [set_labels()] attaches readable level labels to the resulting data frame.
#'
#' \strong{2. Estimation.} [fs_betareg()] fits a beta regression to bounded
#' ratings, mapping the observed scale onto the open unit interval and
#' returning average marginal effects on both the transformed and the original
#' scale.
#'
#' \strong{3. Post-estimation.} [fs_mwtp()] converts a pair of coefficients
#' into a marginal willingness to pay, with a delta-method standard error and
#' confidence interval.
#'
#' @section A note on standard errors:
#' In a typical factorial survey each respondent rates several vignettes, so
#' observations are clustered within respondents. The models fitted by
#' [fs_betareg()] treat observations as independent, which understates
#' standard errors. Cluster-robust covariance and mixed-effects estimation are
#' planned for a future release; until then, treat the reported uncertainty of
#' clustered designs as optimistic.
#'
#' @references
#' Ferrari, S. L. P. and Cribari-Neto, F. (2004). Beta regression for
#' modelling rates and proportions. *Journal of Applied Statistics*, 31(7),
#' 799--815. \doi{10.1080/0266476042000214501}
#'
#' Smithson, M. and Verkuilen, J. (2006). A better lemon squeezer?
#' Maximum-likelihood regression with beta-distributed dependent variables.
#' *Psychological Methods*, 11(1), 54--71.
#' \doi{10.1037/1082-989X.11.1.54}
#'
#' Wallander, L. (2009). 25 years of factorial surveys in sociology: A review.
#' *Social Science Research*, 38(3), 505--520.
#' \doi{10.1016/j.ssresearch.2009.03.004}
#'
#' @keywords internal
"_PACKAGE"
