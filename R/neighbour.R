#' @export
neighbour <- function(m, i, dir) {
  assert_that(is.hexmatrix(m) || is.hexarray(m))
  assert_that(is.index(m, i))
  assert_that(is.count(dir))
  assert_that(dir <= 6)

  res <- .Call('_hexmatrix_neigh',
               PACKAGE = 'hexmatrix',
               dir - 1,
               i - 1,
               nrow(m),
               ncol(m))
  if (res < 0) NA_real_ else res + 1
}
