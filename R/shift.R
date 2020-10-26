#' @import assertthat
#' @export
shiftDown <- function(m) {
  .Call('_hexmatrix_shiftDown', PACKAGE = 'hexmatrix', m, nrow(m))
}


#' @export
shiftUp <- function(m) {
  .Call('_hexmatrix_shiftUp', PACKAGE = 'hexmatrix', m, nrow(m))
}


#' @export
shiftRight <- function(m,
                       odd=TRUE) {
  assert_that(is.matrix(m))
  assert_that(is.flag(odd))

  rows <- seq_len(nrow(m)) %% 2 == (odd + 0)
  m[rows, ] <- cbind(NA, m[rows, -ncol(m), drop=FALSE])
  m
}


#' @export
shiftLeft <- function(m,
                      odd=TRUE) {
  assert_that(is.matrix(m))
  assert_that(is.flag(odd))

  rows <- seq_len(nrow(m)) %% 2 == (odd + 0)
  m[rows, ] <- cbind(m[rows, -1, drop=FALSE], NA)
  m
}
