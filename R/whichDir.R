#' @export
whichDir <- function(current, other, m) {
  res <- .Call('_hexmatrix_whichDir', PACKAGE = 'hexmatrix', current - 1, other - 1, nrow(m), ncol(m))
  ifelse(res < 0, NA, res + 1)
}
