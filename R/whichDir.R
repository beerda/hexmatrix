#' Return the number of direction from the current to the other neighbouring
#' cell in a hexmatrix.
#'
#' @param current The index of the actual cell in hexmatrix
#' @param other The index of the neighbouring cell
#' @param m The underlying hexmatrix or hexarray
#' @return A number from 1 to 6 indicating the direction from `current` to
#'   `other` cell. `NA` is returned if `current` and `other` are not neighbours.
#' @export
whichDir <- function(current, other, m) {
  assert_that(is.number(current))
  assert_that(is.number(other))
  assert_that(is.hexmatrix(m) || is.hexarray(m))

  res <- .Call('_hexmatrix_whichDir', PACKAGE = 'hexmatrix', current - 1, other - 1, nrow(m), ncol(m))
  ifelse(res < 0, NA, res + 1)
}
