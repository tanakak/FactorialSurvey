# Reference implementation of the Smithson-Verkuilen mapping used by
# fs_betareg(), written independently of the package so that the tests check
# the transformation rather than restate it.
to_unit <- function(y, scale = c(0, 10)) {
  n <- length(y)
  y01 <- (y - scale[1L]) / (scale[2L] - scale[1L])
  (y01 * (n - 1) + 0.5) / n
}
