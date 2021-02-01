#' @export
reachability <- function(m,
                         dist=1,
                         trans=0,
                         target=NULL) {
  # --- m assertions
  assert_that(is.numeric(m))
  assert_that(is.hexmatrix(m) || is.hexarray(m))
  m <- as.hexarray(m)

  rows <- nrow(m)
  cols <- ncol(m)
  layers <- nlayer(m)

  # --- dist assertions
  if (is.number(dist)) {
    dist <- matrix(dist, nrow=rows, ncol=cols)
  }
  if (is.matrix(dist)) {
    assert_that(nrow(dist) == rows)
    assert_that(ncol(dist) == cols)
    dist <- array(dist, dim=c(rows, cols, layers, 6))
  }
  if (length(dim(dist)) == 3) {
    assert_that(all(dim(dist) == c(rows, cols, 6)))
    dist <- array(dist, dim=c(rows, cols, layers, 6))
  }
  assert_that(is.array(dist))
  assert_that(all(dim(dist) == c(rows, cols, layers, 6)))

  # --- trans assertions
  if (is.number(trans)) {
    trans <- matrix(trans, nrow=rows, ncol=cols)
  }
  if (is.matrix(trans)) {
    assert_that(nrow(trans) == rows)
    assert_that(ncol(trans) == cols)
    trans <- array(trans, dim=c(rows, cols, layers * (layers - 1)))
  }
  assert_that(is.array(trans))
  assert_that(all(dim(trans) == c(rows, cols, layers * (layers - 1))))

  # --- target assertions
  if (!is.null(target)) {
    assert_that(is.index(m, target))
  } else {
    target <- 0   # search cheapest paths to all cells
  }

  res <- .Call('_hexmatrix_reachability',
               PACKAGE = 'hexmatrix',
               m, dist, trans, target - 1)
  res$paths <- res$paths + 1
  res$init <- res$init + 1

  res
}
