#' @rdname like
#' @export
like <- function(...) {
  UseMethod('like')
}


.empty <- function() { }


.fixDimname <- function(x, len) {
  if (!is.null(x)) {
    if (length(x) < len) {
      x <- c(x, rep(NA, length(x) - len))
    } else if (length(x) > len) {
      x <- x[seq_len(len)]
    }
  }
  x
}


#' @rdname like
#' @export
like.matrix <- function(m, data=.empty, nrow=.empty, ncol=.empty, byrow=FALSE, dimnames=.empty) {
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
  if (all(sapply(dimnames, is.null))) {
    dimnames <- NULL
  }
  if (identical(data, .empty)) {
    data <- as.vector(m[seq_len(nrow), seq_len(ncol)])
  }
  matrix(data=data, nrow=nrow, ncol=ncol, byrow=byrow, dimnames=dimnames)
}


#' @rdname like
#' @export
like.array <- function(a, data=.empty, dim=.empty, dimnames=.empty) {
  if (identical(dim, .empty)) {
    dim <- base::dim(a)
  }
  if (identical(dimnames, .empty)) {
    dimnames <- base::dimnames(a)
    dimnames <- lapply(seq_along(dim), function(i) {
      .fixDimname(dimnames[[i]], dim[i])
    })
  }
  if (all(sapply(dimnames, is.null))) {
    dimnames <- NULL
  }
  if (identical(data, .empty)) {
    data <- do.call(`[`, c(list(a), lapply(dim, seq_len)))
  }
  array(data=data, dim=dim, dimnames=dimnames)
}
