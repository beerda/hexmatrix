#' @export
reachability <- function(m,
                         dist=1) {
  rows <- nrow(m)
  cols <- ncol(m)
  if (is.number(dist)) {
    dist <- matrix(dist, nrow=rows, ncol=cols)
  }
  if (is.matrix(dist)) {
    assert_that(ncol(dist) == cols)
    assert_that(nrow(dist) == rows)
    dist <- array(dist, dim=c(nrow(dist), ncol(dist), 6))
  }
  assert_that(is.array(dist))
  assert_that(all(dim(dist) == c(rows, cols, 6)))

  .Call('_hexmatrix_reachability', PACKAGE = 'hexmatrix', m, dist)
}
