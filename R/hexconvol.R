#' Compute convolution over neighbours of elements of a hexmatrix.
#'
#' The result of this function is a hexmatrix of dimensions equal to `m` with values
#' computed as a weighted sum of value at the given position and in the neighbourhood
#' of that position. The weights are given by the `probs` argument.
#'
#' @param m a numeric hexmatrix
#' @param probs a vector of length 7 with six weights corresponding to the neighbours
#'     clock-wisely from top-left. The 7th value corresponds to the weight of a value
#'     at the "current" position. Precisely, the weights are understood as follows:
#'     \enumerate{
#'        \item `probs[1]` for top-left,
#'        \item `probs[2]` for top-right,
#'        \item `probs[3]` for right,
#'        \item `probs[4]` for bottom-right,
#'        \item `probs[5]` for bottom-left,
#'        \item `probs[6]` for left neighbour's weight of the "current" position,
#'        \item `probs[7]` for the "current" value's weight.
#'     }
#' @return a hexmatrix of dimensions equal to `m` with values computed as a convolution
#'     over the neighbours of `m`'s values
#'
#' @export
hexconvol <- function(m,
                      probs=1) {
  assert_that(is.matrix(m))
  assert_that(is.numeric(m))
  assert_that(is.vector(probs) & is.numeric(probs))

  p <- rep(probs, length.out=7)
  n <- neighbours(m, TRUE)

  f <- function(...) {
    x <- c(...)
    sum(x * p, na.rm=TRUE)
  }
  apply(n, c(1, 2), f)
}
