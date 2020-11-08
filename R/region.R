#' Find a region of values in a hexmatrix
#'
#' For a given hexmatrix and a given index of a cell, find all indices of all
#' neighbours that have the same value.
#'
#' @param m A hexmatrix of values
#' @param i A index of a cell to start from
#' @return A vector of indices of cells that are neighbours of `i` and have the
#'   same value in `m` as `m[i]`, or cells recursively neighbouring with such
#'   neighbours.
#' @export
region <- function(m, i) {
  assert_that(is.hexmatrix(m) || is.hexarray(m))
  assert_that(is.index(m, i))

  res <- .Call('_hexmatrix_region', PACKAGE = 'hexmatrix', m, i - 1);
  sort(res) + 1
}
