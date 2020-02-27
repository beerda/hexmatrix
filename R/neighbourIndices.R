#' @export
neighbourIndices <- function(indices, m) {
  if (is.vector(indices)) {
    indices <- matrix(indices, byrow=TRUE, ncol=2)
  }
  assert_that(is.numeric(indices))
  assert_that(is.matrix(indices))
  assert_that(is.matrix(m))

  xDiff <- c(-1, -1, 0, 1, 1, 0)
  yDiffEven <- c(-1, 0, 1, 0, -1, -1)
  yDiffOdd <- c(0, 1, 1, 1, 0, -1)

  x <- rep(indices[, 1], each=6)
  y <- rep(indices[, 2], each=6)
  even <- x %% 2 == 0

  resX <- x + xDiff
  resX[resX <= 0 | resX > nrow(m)] <- NA
  resY <- y + ifelse(even, yDiffEven, yDiffOdd)
  resY[resY <= 0 | resY > ncol(m)] <- NA

  resY[is.na(resX)] <- NA
  resX[is.na(resY)] <- NA

  res <- c(resX, resY)
  res <- array(res, dim=c(6, nrow(indices), 2))
  res <- aperm(res, c(2, 1, 3))
  res
}
