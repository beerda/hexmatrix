#' Returns an array that represents values of neighbours
#' in the clock-wise order starting from top-left neighbour
#' @param m a matrix or array to compute the neighbours from
#' @param self whether to add the 7-th direction, which is a copy of `m`
#' @return If `m` is a matrix, the returned value is an array with third
#'     dimension representing the neighbour direction (1 to 6, or 7 if `self` is
#'     `TRUE`). If `m` is a 3D array, then then the result is a 4D array with
#'     the fourth dimension representing the direction.
#' @export
#' @import magrittr
neighbours <- function(m, self=FALSE) {
  assert_that(is.hexmatrix(m) || is.hexarray(m))
  assert_that(is.flag(self))

  array(c(m %>% shiftDown() %>% shiftRight(FALSE),       # top left neighbours
          m %>% shiftDown() %>% shiftLeft(TRUE),         # top right neighbours
          m %>% shiftLeft(TRUE) %>% shiftLeft(FALSE),    # right neighbours
          m %>% shiftUp() %>% shiftLeft(TRUE),           # bottom right neighbours
          m %>% shiftUp() %>% shiftRight(FALSE),         # bottom left neightbours
          m %>% shiftRight(TRUE) %>% shiftRight(FALSE),  # left neightbours
          if(self) m else NULL),
        dim=c(dim(m), 6 + self))
}
