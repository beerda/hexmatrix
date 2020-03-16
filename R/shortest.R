#' @export
shortest <- function(source,
                     target,
                     dist) {
  if (is.matrix(dist)) {
    dist <- array(dist, dim=c(nrow(dist), ncol(dist), 6))
  }
  assert_that(is.array(dist))

  assert_that(is.scalar(source))
  assert_that(is.numeric(source))
  assert_that(source >= 1 && source <= length(dist))

  assert_that(is.scalar(target))
  assert_that(is.numeric(target))
  assert_that(target >= 1 && target <= length(dist))

  res <- .Call('_hexmatrix_shortest', PACKAGE = 'hexmatrix', source - 1, target - 1, dist)
  res$prices <- rev(res$prices)
  res$path <- rev(res$path) + 1

  res
}
