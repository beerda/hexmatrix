#' @export
morph <- function(m, dilate=TRUE, na.rm=FALSE) {
  assert_that(is.matrix(m))
  assert_that(is.scalar(dilate) && is.logical(dilate))
  assert_that(is.scalar(na.rm) && is.logical(na.rm))

  f <- ifelse(dilate, pmax, pmin)
  n <- neighbours(m, self=TRUE)
  res <- f(n[, , 1],
           n[, , 2],
           n[, , 3],
           n[, , 4],
           n[, , 5],
           n[, , 6],
           n[, , 7],
           na.rm=TRUE)
  res <- array(as.logical(res), dim=dim(m))

  if (!na.rm) {
    naMask <- morph(is.na(m), dilate=TRUE, na.rm=TRUE)
    res[naMask] <- NA
  }

  res
}