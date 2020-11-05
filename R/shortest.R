#' @export
shortest <- function(source,
                     target,
                     dist,
                     trans=0) {
  if (is.matrix(dist)) {
    dist <- array(dist, dim=c(dim(dist), 1, 6))
  }
  assert_that(is.array(dist))
  assert_that(is.numeric(dist))
  if (length(dim(dist)) == 3) {
    assert_that(dim(dist)[3] == 6)
    dim(dist) <- c(dim(dist)[1:2], 1, 6)
  }
  assert_that(length(dim(dist)) == 4)
  assert_that(dim(dist)[4] == 6)

  rows <- nrow(dist)
  cols <- ncol(dist)
  layers <- nlayer(dist)

  if (is.number(trans)) {
    trans <- array(0, dim=c(rows, cols, layers * (layers - 1)))
  }
  assert_that(is.array(trans))
  assert_that(is.numeric(trans))
  assert_that(length(dim(trans)) == 3)
  assert_that(all(dim(trans) == c(rows, cols, layers * (layers - 1))))

  assert_that(is.count(source))
  assert_that(source <= rows * cols * layers)

  assert_that(is.count(target))
  assert_that(target <= rows * cols * layers)

  res <- .Call('_hexmatrix_shortest',
               PACKAGE = 'hexmatrix',
               source - 1,
               target - 1,
               dist,
               trans)
  if (length(res) == 0) {
    res <- NULL
  } else {
    res$path <- res$path + 1
  }

  res
}
