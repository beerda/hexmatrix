#' @export
#' @import magrittr
revertNeighbours <- function(n) {
  assert_that(is.array(n))
  assert_that(length(dim(n)) %in% c(3, 4))

  if (length(dim(n)) == 3) {
    # hexmatrix neighbours
    assert_that(dim(n)[3] %in% c(6:7))
    f <- function(n, dir) n[, , dir]

  } else if (length(dim(n)) == 4) {
    # hexarray neighbours
    assert_that(dim(n)[4] %in% c(6:7))
    f <- function(n, dir) n[, , , dir]
  }

  self <- NULL
  if (dim(n)[length(dim(n))] == 7) {
    self <- f(n, 7)
  }

  array(c(f(n, 4) %>% shiftDown() %>% shiftRight(FALSE),       # top left neighbours
          f(n, 5) %>% shiftDown() %>% shiftLeft(TRUE),         # top right neighbours
          f(n, 6) %>% shiftLeft(TRUE) %>% shiftLeft(FALSE),    # right neighbours
          f(n, 1) %>% shiftUp() %>% shiftLeft(TRUE),           # bottom right neighbours
          f(n, 2) %>% shiftUp() %>% shiftRight(FALSE),         # bottom left neightbours
          f(n, 3) %>% shiftRight(TRUE) %>% shiftRight(FALSE),  # left neightbours
          self),
        dim=dim(n))
}
