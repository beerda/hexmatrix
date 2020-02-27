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
    s <- pmin(n[, , 1] + dist[, , 1],
              n[, , 2] + dist[, , 2],
              n[, , 3] + dist[, , 3],
              n[, , 4] + dist[, , 4],
              n[, , 5] + dist[, , 5],
              n[, , 6] + dist[, , 6],
              Inf, na.rm=TRUE)
    m <- pmin(m, s, Inf, na.rm=TRUE)
  }
  m
}
