#' @export
neighbour <- function(m, i, dir) {
  res <- .Call('_hexmatrix_neigh',
               PACKAGE = 'hexmatrix',
               dir - 1,
               i - 1,
               nrow(m),
               ncol(m))
  if (res < 0) NA_real_ else res + 1
}
