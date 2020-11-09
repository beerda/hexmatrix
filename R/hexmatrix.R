#' @export
is.hexmatrix <- function(x) {
  is.matrix(x)
}


#' @export
is.hexarray <- function(x) {
  is.array(x) && length(dim(x)) == 3
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


#' @export
as.hexarray <- function(x, ...) {
  if (is.hexmatrix(x)) {
    return(array(x, dim=c(dim(x), 1)))
  } else if (is.hexarray(x)) {
    return(x)
  } else {
    stop('Conversion to hexarray failed.')
  }
}


#' @export
nlayer <- function(x) {
  dim(x)[3L]
}


#' @export
layernames <- function(x) {
  assert_that(is.hexarray(x))
  dimnames(x)[[3L]]
}


#' @export
`layernames<-` <- function(x, value) {
  assert_that(is.hexarray(x))

  dn <- dimnames(x)
  if (is.null(dn)) {
    if (is.null(value)) {
      return(x)
    }
    dn <- vector('list', 3L)
  }
  if (is.null(value)) {
    dn[3L] <- list(NULL)
  } else {
    dn[[3L]] <- value
  }
  dimnames(x) <- dn
  x
}
