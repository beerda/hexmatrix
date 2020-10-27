#' @import assertthat
#' @export
shiftDown <- function(m) {
  assert_that(is.hexmatrix(m) || is.hexarray(m))
  .Call('_hexmatrix_shiftDown', PACKAGE = 'hexmatrix', m, nrow(m))
}


#' @export
shiftUp <- function(m) {
  assert_that(is.hexmatrix(m) || is.hexarray(m))
  .Call('_hexmatrix_shiftUp', PACKAGE = 'hexmatrix', m, nrow(m))
}


#' @export
shiftRight <- function(m,
                       odd=TRUE) {
  assert_that(is.hexmatrix(m) || is.hexarray(m))
  assert_that(is.flag(odd))
  .Call('_hexmatrix_shiftRight', PACKAGE = 'hexmatrix',
        m, nrow(m), ncol(m), as.integer(odd))
}


#' @export
shiftLeft <- function(m,
                      odd=TRUE) {
  assert_that(is.hexmatrix(m) || is.hexarray(m))
  assert_that(is.flag(odd))
  .Call('_hexmatrix_shiftLeft', PACKAGE = 'hexmatrix',
        m, nrow(m), ncol(m), as.integer(odd))
}
