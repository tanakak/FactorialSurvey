#' Ratings of take-away lunch boxes from a classroom factorial survey
#'
#' Ratings of eight take-away lunch box (bento) vignettes by 48 university
#' students. Each vignette describes a lunch box by its main dish, whether it
#' comes with a side dish, whether nutrition information is shown, and its
#' price; respondents rated how likely they would be to buy it on a 0 to 10
#' scale. Every respondent rated all eight vignettes.
#'
#' The data are small and were collected for teaching, which makes them
#' convenient for illustrating the workflow: estimating a beta regression with
#' standard errors clustered by respondent, then marginal and expected
#' willingness to pay.
#'
#' @format A data frame with 384 rows (48 respondents by 8 vignettes) and 7
#'   variables:
#' \describe{
#'   \item{respondent}{Respondent identifier, 1 to 48.}
#'   \item{vignette}{Vignette identifier, 1 to 8.}
#'   \item{main}{Main dish, a factor with levels `"grilled fish"` (the
#'     reference), `"teriyaki chicken"` and `"tofu hamburger"`.}
#'   \item{side}{Side dish included: 1 yes, 0 no.}
#'   \item{info}{Nutrition information shown: 1 yes, 0 no.}
#'   \item{price}{Price in Japanese yen: 700, 900, 1100 or 1300.}
#'   \item{rating}{Purchase intention, from 0 ("would definitely not buy")
#'     to 10 ("would definitely buy").}
#' }
#'
#' @source Collected from students in the course "Environmental Data
#'   Analysis" at Shiga University in 2025, with their permission to publish
#'   the anonymous responses.
#'
#' @examples
#' head(bento)
#' fit <- fs_betareg(rating ~ main + side + info + price, data = bento,
#'                   cluster = ~ respondent, compute_me = FALSE)
#' fs_mwtp(fit, price = "price")
"bento"
