#' Compute opposite direction number
#'
#' In `hexmatrix` package, the directions of neighbours of a cell in a hexmatrix
#' are numbered from 1 to 7 in clock-wise direction starting from top-left.
#' That is, number 1 refers to the top-left neighbour, 2 is for top-right
#' neighbour, 3 for right, 4 for bottom-right, 5 for bottom-left and 6 for left
#' neighbour. Number 7 in some cases represents no-movement, i.e., the neighbour
#' of a cell in the 7th direction is the cell itself.
#'
#' This function provides reverse numbering for such directions, i.e., a
#' top-left neighbour (i.e., in direction 1) of a cell has this cell in
#' bottom-right direction 4. etc.
#'
#' @param dir A direction number (from 1 to 7)
#' @return The opposite direction number, i.e. 4 for 1, 5 for 2, 6 for 3, 1 for
#' 4, 2 for 5, 3 for 6 and 7 for 7.
#' @export
oppositeDir <- function(dir) {
  assert_that(is.count(dir))
  assert_that(dir <= 7)

  ifelse(dir == 7, 7, (dir + 2) %% 6 + 1)
}
