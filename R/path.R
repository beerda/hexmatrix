#' @export
path <- function(to, paths) {
  assert_that(is.matrix(paths))
  assert_that(is.numeric(paths))
  assert_that(min(paths, na.rm=TRUE) >= 0)
  assert_that(max(paths, na.rm=TRUE) <= length(paths))

  assert_that(is.vector(to))
  assert_that(is.numeric(to))
  assert_that(min(to, na.rm=TRUE) >= 0)
  assert_that(max(to, na.rm=TRUE) <= length(paths))

  lapply(to, function(i) {
    .Call('_hexmatrix_path', PACKAGE = 'hexmatrix', i, paths);
  })

}
