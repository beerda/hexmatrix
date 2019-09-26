#' Compute convolution over the neighbours.
#'
#' @param m a hex-matrix
#' @param probs a vector of length 7 with probabilities given to the neighbours
#'     clock-wisely from top-left with "this" coordinate's probability at 7th
#'     position
#' @export
hexconvol <- function(m,
                      probs=1) {
  assert_that(is.matrix(m))
  assert_that(is.vector(probs) & is.numeric(probs))

  p <- rep(probs, length.out=7)
  n <- neighbours(m)
  n[[7]] <- m

  f <- function(...) {
    x <- c(...)
    sum(x * p, na.rm=TRUE)
  }
  elementwisely(n, f)
}
