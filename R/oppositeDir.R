#' @export
oppositeDir <- function(dir) {
  (dir + 2) %% 6 + 1
}
