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

  old <- NA
  while (!identical(old, m)) {
    old <- m
    n <- neighbours(m)
    n <- array(c(n, dist), dim=c(rows, cols, 12))
    s <- apply(n, c(1, 2), function(h) {
      suppressWarnings(min(h[1:6] + h[7:12], na.rm=TRUE))
    })
    m <- suppressWarnings(pmin(m, s, na.rm=TRUE))
  }
  m
}
