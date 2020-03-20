#' @export
altpaths <- function(source,
                     target,
                     dist,
                     regions,
                     n=1,
                     step=1,
                     priceThreshold=function(p) p + log(p)) {
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

  assert_that(is.scalar(n) && is.numeric(n))
  assert_that(is.scalar(step) && is.numeric(step))

  res <- .Call('_hexmatrix_altpaths', PACKAGE = 'hexmatrix',
               source - 1, target - 1,
               dist, regions, n, step, priceThreshold)
  res <- lapply(res, function(r) {
    r$path <- r$path + 1
    r
  })

  res
}
