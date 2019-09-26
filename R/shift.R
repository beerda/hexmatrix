#' @import assertthat
#' @export
shiftDown <- function(m) {
  assert_that(is.matrix(m))

  rbind(NA, m[-nrow(m), ])
}


#' @export
shiftUp <- function(m) {
  assert_that(is.matrix(m))

  rbind(m[-1, ], NA)
}


#' @export
shiftRight <- function(m,
                       odd=TRUE) {
  assert_that(is.matrix(m))
  assert_that(is.flag(odd))

  rows <- seq_len(nrow(m)) %% 2 == (odd + 0)
  m[rows, ] <- cbind(NA, m[rows, -ncol(m)])
  m
}


#' @export
shiftLeft <- function(m,
                      odd=TRUE) {
  assert_that(is.matrix(m))
  assert_that(is.flag(odd))

  rows <- seq_len(nrow(m)) %% 2 == (odd + 0)
  m[rows, ] <- cbind(m[rows, -1], NA)
  m
}
