#' @export
is.hexarray <- function(x) {
  is.array(x) && length(dim(x)) == 3
}


#' @export
as.hexarray <- function(x, ...) {
  if (is.hexmatrix(x)) {
    return(array(x, dim=c(dim(x), 1L)))
  } else if (is.list(x)) {
    assert_that(length(x) > 0L)
    assert_that(all(sapply(x, is.hexmatrix)))
    rows <- nrow(x[[1]])
    cols <- ncol(x[[1]])
    assert_that(all(sapply(x, function(m) nrow(m) == rows && ncol(m) == cols)))
    return(array(unlist(x),
                 dim=c(dim(x[[1]]), length(x)),
                 dimnames=list(NULL, NULL, names(x))))
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
