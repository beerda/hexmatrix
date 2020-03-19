#' @export
region <- function(m, i) {
  assert_that(is.matrix(m))
  assert_that(is.scalar(i) && is.numeric(i))
  assert_that(i > 0 && i <= length(m))

  res <- .Call('_hexmatrix_region', PACKAGE = 'hexmatrix', m, i);
  sort(res)
}
