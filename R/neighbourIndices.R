#' Compute coordinates of neighbours of hexmatrix cells
#'
#' For each given coordinate of a cell (row, col), this function
#' computes the coordinates of the neighbouring cells in all six directions
#' starting clock-wisely from upper-left.
#'
#' @param indices A vector or matrix of coordinates, whose neighbour coordinates
#'   have to be computed. This argument has to be either a two-column matrix or
#'   a vector. If vector is provided, it is internally transformed into the
#'   two-column matrix. The columns represent row and column coordinates,
#'   respectively.
#' @param m A hexmatrix or hexarray, for which the neighbour coordinates have
#'   to be obtained.
#' @return An array with:
#'   * 1st dimension corresponding to the input coordinates (i.e., to the rows
#'     of the `indices` matrix),
#'   * 2nd dimension corresponding to the direction (1 to 6) of the
#'     neighbourhood (where 1 corresponds to the top-left neighbour and the rest
#'     of neighbours are assigned clock-wisely so that 2 corresponds to
#'     top-right, 3 to right, 4 to bottom-right, 5 to bottom-left and 6 to
#'     left),
#'   * 3rd dimension is always of size 2 and represents rows (1) and columns (2)
#'     of the resulting coordinates.
#'
#'   For instance, if a single pair of values is put into `indices`, i.e. row
#'   and column of a cell, the resulting array would have dimensions `(1, 6, 2)`
#'   for single input coordinate, 6 directions, 2 coordinates of the results.
#'   The row & column number of the top-left neighbour is in `[1, 1, 1:2]`
#'   of the result. The row & column number of the bottom-left neighbour is in
#'   `[1, 5, 1:2]` etc.
#' @export
neighbourIndices <- function(indices, m) {
  if (is.vector(indices)) {
    indices <- matrix(indices, byrow=TRUE, ncol=2)
  }
  assert_that(is.numeric(indices))
  assert_that(is.matrix(indices))
  assert_that(ncol(indices) == 2)
  assert_that(is.hexmatrix(m) || is.hexarray(m))

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
