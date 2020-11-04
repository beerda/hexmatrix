#' @export
pathReduce <- function(f, origin, path, dist=1, trans=0) {
  # --- f assertions
  f <- match.fun(f)
  assert_that(is.function(f))

  # --- origin assertions (none)

  # --- path assertions
  assert_that(is.numeric(path))
  assert_that(min(path, na.rm=TRUE) >= 0)
  assert_that(max(path, na.rm=TRUE) <= length(path))
  if (is.hexmatrix(path)) {
    dim(path) <- c(dim(path), 1)
  }
  assert_that(is.hexarray(path))

  rows <- nrow(path)
  cols <- ncol(path)
  layers <- nlayer(path)

  # --- dist assertions
  if (is.scalar(dist)) {
    dist <- matrix(dist, nrow=rows, ncol=cols)
  }
  if (is.matrix(dist)) {
    assert_that(nrow(dist) == rows)
    assert_that(ncol(dist) == cols)
    dist <- array(dist, dim=c(rows, cols, layers, 6))
  }
  assert_that(is.array(dist))
  assert_that(all(dim(dist) == c(rows, cols, layers, 6)))

  # --- trans assertions
  if (is.scalar(trans)) {
    trans <- matrix(trans, nrow=rows, ncol=cols)
  }
  assert_that(is.matrix(trans))
  if (is.matrix(trans)) {
    assert_that(nrow(trans) == rows)
    assert_that(ncol(trans) == cols)
    trans <- array(trans, dim=c(rows, cols, layers * (layers - 1)))
  }
  assert_that(is.array(trans))
  assert_that(all(dim(trans) == c(rows, cols, layers * (layers - 1))))

  res <- .Call('_hexmatrix_pathReduce',
               PACKAGE = 'hexmatrix',
               as.list(dist),
               as.list(trans),
               path, origin, f)

  len <- unique(lapply(res, length))
  if (length(len) == 1) {
    res <- unlist(res)
    if (len == 1) {
      return(matrix(res, nrow=rows))
    } else {
      return(array(res, dim=c(dim(dist), len)))
    }

  } else {
    return(res);
  }
}
