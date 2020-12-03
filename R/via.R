#' Return a logical hexarray of paths that lead through the given intermediate
#' node
#'
#' @export
via <- function(paths, intermed) {
  assert_that(is.hexmatrix(paths) || is.hexarray(paths))

  mat <- is.hexmatrix(paths)
  paths <- as.hexarray(paths)
  rows <- nrow(paths)
  cols <- ncol(paths)
  layers <- nlayer(paths)
  rowcol <- rows * cols

  assert_that(is.count(intermed))
  assert_that(intermed <= rows * cols * layers)

  dist <- array(FALSE, dim=c(rows, cols, layers, 6))
  dist[intermed + 0:5 * rowcol * layers] <- TRUE
  trans <- array(FALSE, dim=c(rows, cols, layers * (layers-1)))
  trans[(intermed %% rowcol) + rowcol * (seq_len(layers-1) + intermed %/% rowcol - 1)] <- TRUE

  res <- pathReduce(f=`|`, origin=FALSE, path=paths, dist=dist, trans=trans)
  res[intermed] <- TRUE
  if (mat) {
    return(res[, , 1L])
  }
  return(res)
}
