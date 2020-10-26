#' @export
is.hexmatrix <- function(x) {
  is.matrix(x)
}


#' @export
is.hexarray <- function(x) {
  is.array(x) && length(dim(x)) == 3
}


#' @export
nlayer <- function(x) {
  dim(x)[3L]
}
