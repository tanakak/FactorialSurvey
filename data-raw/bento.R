## Build the `bento` dataset from the classroom survey files.
##
## bento.csv    : one row per respondent, ratings of vignettes 1-8 (fs_q1..fs_q8)
## vignette.csv : one row per vignette, attributes x1-x4 (id_fse = vignette)
##
## x1 main dish : 0 grilled fish, 1 teriyaki chicken, 2 tofu hamburger
## x2 side dish : 1 yes, 0 no
## x3 nutrition information shown : 1 yes, 0 no
## x4 price (yen)

ratings <- read.csv("data-raw/bento.csv")
vig <- read.csv("data-raw/vignette.csv")

n <- nrow(ratings)
k <- ncol(ratings)
bento <- data.frame(
  respondent = rep(seq_len(n), each = k),
  vignette   = rep(seq_len(k), times = n),
  rating     = as.integer(as.vector(t(as.matrix(ratings))))
)
vig <- vig[match(bento$vignette, vig$id_fse), ]
bento$main  <- factor(c("grilled fish", "teriyaki chicken",
                        "tofu hamburger")[vig$x1 + 1L],
                      levels = c("grilled fish", "teriyaki chicken",
                                 "tofu hamburger"))
bento$side  <- as.integer(vig$x2)
bento$info  <- as.integer(vig$x3)
bento$price <- as.integer(vig$x4)
bento <- bento[, c("respondent", "vignette", "main", "side", "info", "price",
                   "rating")]
rownames(bento) <- NULL

save(bento, file = "data/bento.rda", compress = "xz")
