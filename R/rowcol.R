#' @export
rowcol <- function(m) {
  n <- deparse(substitute(m))
  assert_that(is.matrix(m))

  rows <- nrow(m)
  cols <- ncol(m)
  res <- data.frame(row=rep(seq_len(rows), times=cols),
                    col=rep(seq_len(cols), each=rows),
                    n=as.vector(m))
  colnames(res)[3] <- n
  res
}


#' @export
`rowcol<-` <- function(m, row, col, value) {
  maxlen <- max(length(row), length(col), length(value))
  row <- rep(row, length.out=maxlen)
  col <- rep(col, length.out=maxlen)
  value <- rep(value, length.out=maxlen)
  for (i in seq_along(row)) {
    m[row[i], col[i]] <- value[i]
  }
  m
}
