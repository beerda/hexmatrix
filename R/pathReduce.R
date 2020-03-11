#' @export
pathReduce <- function(m, paths, origin, f) {
  f <- match.fun(f)

  if (is.matrix(m)) {
    assert_that(nrow(m) == nrow(paths))
    assert_that(ncol(m) == ncol(paths))
    m <- array(m, dim=c(nrow(m), ncol(m), 6))
  }

  assert_that(is.array(m))
  assert_that(is.matrix(paths))
  assert_that(is.numeric(paths))
  assert_that(all(dim(m) == c(nrow(paths), ncol(paths), 6)))
  assert_that(min(paths, na.rm=TRUE) >= 0)
  assert_that(max(paths, na.rm=TRUE) <= length(paths))
  assert_that(is.function(f))

  res <- .Call('_hexmatrix_pathReduce', PACKAGE = 'hexmatrix', as.list(m), paths, origin, f)

  len <- unique(lapply(res, length))
  if (length(len) == 1) {
    res <- unlist(res)
    if (len == 1) {
      return(matrix(res, nrow=nrow(paths)))
    } else {
      return(array(res, dim=c(dim(m), len)))
    }

  } else {
    return(res);
  }
}
