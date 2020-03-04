#' @export
pathReduce <- function(m, path, f) {
  f <- match.fun(f)

  assert_that(is.matrix(m))
  assert_that(is.matrix(path))
  assert_that(is.numeric(path))
  assert_that(nrow(m) == nrow(path))
  assert_that(ncol(m) == ncol(path))
  assert_that(min(path, na.rm=TRUE) == 0)
  assert_that(max(path, na.rm=TRUE) == 6)
  assert_that(is.function(f))

  res <- .Call('_hexmatrix_pathReduce', PACKAGE = 'hexmatrix', as.list(m), path, f)

  len <- unique(lapply(res, length))
  if (length(len) == 1) {
    res <- unlist(res)
    if (len == 1) {
      return(matrix(res, nrow=nrow(m)))
    } else {
      return(array(res, dim=c(dim(m), len)))
    }

  } else {
    return(res);
  }
}
