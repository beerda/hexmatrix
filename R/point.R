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


#' @rdname index
#' @export
point.matrix <- function(m, index, ...) {
  assert_that(is.hexmatrix(m))
  assert_that(is.count(index))
  assert_that(index <= nrow(m) * ncol(m))

  c((index - 1) %% nrow(m) + 1,
    ((index - 1) %/% nrow(m)) + 1)
}


#' @rdname index
#' @export
point.array <- function(m, index, ...) {
  assert_that(is.hexarray(m))
  assert_that(is.count(index))
  assert_that(index <= nrow(m) * ncol(m) * nlayer(m))

  rowcol <- nrow(m) * ncol(m)
  c((index - 1) %% nrow(m) + 1,
    ((index - 1) %% rowcol) %/% nrow(m) + 1,
    (index - 1) %/% rowcol + 1)
}
