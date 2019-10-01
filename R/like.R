#' @rdname like
#' @export
like <- function(...) {
  UseMethod('like')
}


.empty <- function() { }


.fixDimname <- function(x, len) {
  if (length(x) < len) {
    x <- c(x, rep(NA, length(x) - len))
  } else if (length(x) > len) {
    x <- x[seq_len(len)]
  }
  x
}


#' @rdname like
#' @export
like.matrix <- function(m, data=.empty, nrow=.empty, ncol=.empty, byrow=FALSE, dimnames=.empty) {
  if (identical(data, .empty)) {
    data <- as.vector(m)
  }
  if (identical(nrow, .empty)) {
    nrow <- base::nrow(m)
  }
  if (identical(ncol, .empty)) {
    ncol <- base::ncol(m)
  }
  if (identical(dimnames, .empty)) {
    dimnames <- base::dimnames(m)
    dimnames <- list(.fixDimname(dimnames[[1]], nrow), .fixDimname(dimnames[[2]], ncol))
  }
  matrix(data=data, nrow=nrow, ncol=ncol, byrow=byrow, dimnames=dimnames)
}
