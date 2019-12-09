#' @export
rowcol <- function(m) {
  n <- deparse(substitute(m))
  assert_that(is.matrix(m))
  res <- which(m==m, arr.ind=TRUE, useNames=FALSE)
  res <- as.data.frame(res)
  res <- cbind(res, as.vector(m))
  colnames(res) <- c('row', 'col', n)
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
