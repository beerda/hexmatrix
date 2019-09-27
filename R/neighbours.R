#' Returns a list of matrices that represent values of neighbours
#' in the clock-wise order starting from top-left neighbour
#' @export
#' @import magrittr
neighbours <- function(m) {
  assert_that(is.matrix(m))

  array(c(m %>% shiftDown() %>% shiftRight(FALSE),       # top left neighbours
          m %>% shiftDown() %>% shiftLeft(TRUE),         # top right neighbours
          m %>% shiftLeft(TRUE) %>% shiftLeft(FALSE),    # right neighbours
          m %>% shiftUp() %>% shiftLeft(TRUE),           # bottom right neighbours
          m %>% shiftUp() %>% shiftRight(FALSE),         # bottom left neightbours
          m %>% shiftRight(TRUE) %>% shiftRight(FALSE)), # left neightbours
        dim=c(nrow(m), ncol(m), 6))
}
