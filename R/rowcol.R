#' @export
rowcol <- function(m, ...) {
  UseMethod('rowcol')
}

#' @rdname rowcol
#' @export
rowcol.matrix <- function(m) {
  n <- deparse(substitute(m))
  assert_that(is.hexmatrix(m))

  rows <- nrow(m)
  cols <- ncol(m)
  res <- data.frame(row=rep(seq_len(rows), times=cols),
                    col=rep(seq_len(cols), each=rows),
                    n=as.vector(m))
  colnames(res)[3] <- n
  res
}


#' @rdname rowcol
#' @export
rowcol.array <- function(m) {
  n <- deparse(substitute(m))
  assert_that(is.hexarray(m))

  rows <- nrow(m)
  cols <- ncol(m)
  row <- rep(seq_len(rows), times=cols)
  col <- rep(seq_len(cols), each=rows)
  res <- data.frame(row=row,
                    col=col,
                    layer=rep(seq_len(nlayer(m)), each=length(row)),
                    n=as.vector(m))
  colnames(res)[4] <- n
  res
}


#' @rdname rowcol
#' @export
`rowcol<-` <- function(...) {
  UseMethod('rowcol<-')
}


#' @rdname rowcol
#' @export
`rowcol<-.matrix` <- function(m, row, col, value) {
  assert_that(is.hexmatrix(m))

  maxlen <- max(length(row), length(col), length(value))
  row <- rep(row, length.out=maxlen)
  col <- rep(col, length.out=maxlen)
  value <- rep(value, length.out=maxlen)
  for (i in seq_along(row)) {
    m[row[i], col[i]] <- value[i]
  }
  m
}


#' @rdname rowcol
#' @export
`rowcol<-.array` <- function(m, row, col, layer, value) {
  assert_that(is.hexarray(m))

  maxlen <- max(length(row), length(col), length(layer), length(value))
  row <- rep(row, length.out=maxlen)
  col <- rep(col, length.out=maxlen)
  layer <- rep(layer, length.out=maxlen)
  value <- rep(value, length.out=maxlen)
  for (i in seq_along(row)) {
    m[row[i], col[i], layer[i]] <- value[i]
  }
  m
}
