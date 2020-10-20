reachabilityInternal <- function(m, dist, target) {
  res <- .Call('_hexmatrix_reachability', PACKAGE = 'hexmatrix', m, dist, target - 1)
  res$paths <- res$paths + 1
  res$init <- res$init + 1

  res
}


#' @export
reachability <- function(m,
                         dist=1,
                         target=NULL) {
  assert_that(is.matrix(m))
  assert_that(is.numeric(m))
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
  if (!is.null(target)) {
    assert_that(is.numeric(target))
    assert_that(is.scalar(target))
    assert_that(target >= 1 && target <= length(m))
  } else {
    target <- -1   # search cheapest paths to all cells
  }

  reachabilityInternal(m, dist, target)
}
