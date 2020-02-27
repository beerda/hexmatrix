#' @export
neighbourIndices <- function(indices) {
  if (is.vector(indices)) {
    indices <- matrix(indices, byrow=TRUE, ncol=2)
  }
  assert_that(is.numeric(indices))
  assert_that(is.matrix(indices))

  xDiff <- c(-1, -1, 0, 1, 1, 0)
  yDiffEven <- c(-1, 0, 1, 0, -1, -1)
  yDiffOdd <- c(0, 1, 1, 1, 0, -1)

  x <- rep(indices[, 1], each=6)
  y <- rep(indices[, 2], each=6)
  even <- x %% 2 == 0

  res <- c(x + xDiff,
           y + ifelse(even, yDiffEven, yDiffOdd))
  res <- array(res, dim=c(6, nrow(indices), 2))
  aperm(res, c(2, 1, 3))
}
