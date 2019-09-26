#' Returns a list of matrices that represent values of neighbours
#' in the clock-wise order starting from top-left neighbour
#' @export
neighbours <- function(m) {
  assert_that(is.matrix(m))

  list(TL=m %>% shiftDown() %>% shiftRight(FALSE),       # top left neighbours
       TR=m %>% shiftDown() %>% shiftLeft(TRUE),         # top right neighbours
       R=m %>% shiftLeft(TRUE) %>% shiftLeft(FALSE),     # right neighbours
       BR=m %>% shiftUp() %>% shiftLeft(TRUE),           # bottom right neighbours
       BL=m %>% shiftUp() %>% shiftRight(FALSE),         # bottom left neightbours
       L=m %>% shiftRight(TRUE) %>% shiftRight(FALSE))   # left neightbours
}
