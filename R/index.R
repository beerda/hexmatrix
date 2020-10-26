#' Get index an element of a matrix or array given by the point coordinates,
#' i.e., row, column, (and layer, for arrays) number.
#'
#' This function is an inverse function to [point()].
#'
#' @param m A matrix or an array whose element index has to be computed
#' @param point The coordinates of the element. For matrix, it should be a
#'     vector of two values (row, column). For array, three values have to be
#'     provided (row, column, layer).
#' @return An integer value corresponding to the index of the element determined
#'     in `point`, if `m` was treated as a single-dimensional vector.
#' @seealso [point()]
#' @export
index <- function(m, point, ...) {
  UseMethod('index')
}


#' @rdname index
#' @export
index.matrix <- function(m, point, ...) {
  assert_that(is.hexmatrix(m))
  assert_that(length(point) == 2)
  assert_that(is.count(point[1]))
  assert_that(is.count(point[2]))
  assert_that(point[1] <= nrow(m))
  assert_that(point[2] <= ncol(m))

  point[1] + nrow(m) * (point[2] - 1)
}


#' @rdname index
#' @export
index.array <- function(m, point, ...) {
  assert_that(is.hexarray(m))
  assert_that(length(point) == 3)
  assert_that(is.count(point[1]))
  assert_that(is.count(point[2]))
  assert_that(is.count(point[3]))
  assert_that(point[1] <= nrow(m))
  assert_that(point[2] <= ncol(m))
  assert_that(point[3] <= nlayer(m))

  point[1] + nrow(m) * (point[2] - 1) + nrow(m) * ncol(m) * (point[3] - 1)
}
