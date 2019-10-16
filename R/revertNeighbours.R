#' @export
#' @import magrittr
revertNeighbours <- function (n) {
  assert_that(is.array(n))
  assert_that(dim(n)[3] %in% c(6:7))

  array(c(n[, , 4] %>% shiftDown() %>% shiftRight(FALSE),       # top left neighbours
          n[, , 5] %>% shiftDown() %>% shiftLeft(TRUE),         # top right neighbours
          n[, , 6] %>% shiftLeft(TRUE) %>% shiftLeft(FALSE),    # right neighbours
          n[, , 1] %>% shiftUp() %>% shiftLeft(TRUE),           # bottom right neighbours
          n[, , 2] %>% shiftUp() %>% shiftRight(FALSE),         # bottom left neightbours
          n[, , 3] %>% shiftRight(TRUE) %>% shiftRight(FALSE),  # left neightbours
          if (dim(n)[3] == 7) n[, , 7] else NULL),              # self
        dim=dim(n))
}
