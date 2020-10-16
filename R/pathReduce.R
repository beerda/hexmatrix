#' @export
pathReduce <- function(m, paths, origin, f) {
  f <- match.fun(f)

  if (is.matrix(m)) {
    m <- array(m, dim=c(nrow(m), ncol(m), 1, 6))
  }
  assert_that(is.array(m))
  rows <- dim(m)[1]
  cols <- dim(m)[2]
  layers <- dim(m)[3]
  assert_that(all(dim(m) == c(rows, cols, layers, 6 + layers * (layers - 1))))

  assert_that(is.array(paths))
  assert_that(is.numeric(paths))
  assert_that(all(dim(paths) == c(rows, cols, layers)))
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
