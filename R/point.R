#' Get coordinates (i.e., row, column, (layer)) of an element of a matrix or
#' array given by the index.
#'
#' This function is an inverse function to [index()].
#'
#' @param m A matrix or an array whose element coordinates have to be computed
#' @param index The index of the element.
#' @return For matrix, it returns a vector of two values (row, column). For
#'     array, a vector of three values is returned (row, column, layer).
#' @seealso [index()]
#' @export
point <- function(m, index, ...) {
  UseMethod('point')
}


#' @rdname point
#' @export
point.matrix <- function(m, index, ...) {
  assert_that(is.hexmatrix(m))
  assert_that(is.index(m, index))

  c((index - 1) %% nrow(m) + 1,
    ((index - 1) %/% nrow(m)) + 1)
}


#' @rdname point
#' @export
point.array <- function(m, index, ...) {
  assert_that(is.hexarray(m))
  assert_that(is.index(m, index))

  rowcol <- nrow(m) * ncol(m)
  c((index - 1) %% nrow(m) + 1,
    ((index - 1) %% rowcol) %/% nrow(m) + 1,
    (index - 1) %/% rowcol + 1)
}


#' @rdname point
#' @export
is.point <- function(m, point) {
  UseMethod('is.point')
}


#' @rdname point
#' @export
is.point.matrix <- function(m, point) {
  is.vector(point) &&
    length(point) == 2 &&
    is.count(point[1]) &&
    is.count(point[2]) &&
    point[1] <= nrow(m) &&
    point[2] <= ncol(m)
}


#' @rdname point
#' @export
is.point.array <- function(m, point) {
  is.vector(point) &&
    length(point) == 3 &&
    is.count(point[1]) &&
    is.count(point[2]) &&
    is.count(point[3]) &&
    point[1] <= nrow(m) &&
    point[2] <= ncol(m) &&
    point[3] <= nlayer(m)
}
