#' @export
is.hexmatrix <- function(x) {
  is.matrix(x)
}


#' @export
as.hexmatrix <- function(x, ...) {
  if (is.hexmatrix(x)) {
    return(x)
  } else if (is.hexarray(x)) {
    if (nlayer(x) > 1) {
      warning(paste('Conversion of hexarray to hexmatrix:',
                     (nlayer(x) - 1), 'omitted.'))
    }
    return(x[, , 1])
  } else {
    stop('Conversion to hexmatrix failed.')
  }
}
