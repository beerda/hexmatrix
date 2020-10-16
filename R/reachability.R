#' @export
reachability <- function(m,
                         dist=1,
                         target=NULL) {
  # matrix should be a 3D array with 1d, 2d corresponding to the rows/cols of the
  # map and the 3d corresponding to map layers (z-axis)
  assert_that(is.numeric(m))
  if (is.matrix(m)) {
    # make an array from the matrix
    dim(m) <- c(dim(m), 1)
  }
  assert_that(is.array(m))
  assert_that(length(dim(m)) == 3)

  # dist should be a 4D array with 1d, 2d corresponding to the rows/cols of the
  # map, 3d corresponding to map layers (z-axis) and 4d to the movement
  # directions (1-6 are the horizontal directions to hexagonal neighbourhoods,
  # 7,8,9... are the directions between layers: 1->2, 1->3, ... 1->l, 2->1,
  # 2->3, ..., 2->l, ..., l->1, l->2, ..., l->(l-1))
  rows <- dim(m)[1]
  cols <- dim(m)[2]
  layers <- dim(m)[3]
  if (is.number(dist)) {
    dist <- matrix(dist, nrow=rows, ncol=cols)
  }
  if (is.matrix(dist)) {
    assert_that(ncol(dist) == cols)
    assert_that(nrow(dist) == rows)
    dist <- array(dist, dim=c(nrow(dist), ncol(dist), 1, 6))
  }
  assert_that(is.array(dist))
  if (length(dim(dist)) == 3) {
    assert_that(all(dim(dist) == c(rows, cols, 6)))
    dim(dist) <- c(rows, cols, 1, 6)
  }
  assert_that(all(dim(dist) == c(rows, cols, layers, 6 + layers * (layers - 1))))

  if (!is.null(target)) {
    assert_that(is.numeric(target))
    assert_that(is.scalar(target))
    assert_that(target >= 1 && target <= length(m))
  } else {
    target <- 0   # search cheapest paths to all cells
  }

  res <- .Call('_hexmatrix_reachability', PACKAGE = 'hexmatrix', m, dist, target - 1)
  res$paths <- res$paths + 1
  res$init <- res$init + 1

  res
}
