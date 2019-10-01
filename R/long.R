#' @export
long <- function(...) {
  params <- list(...)
  if (length(params) == 0) {
    return(data.frame())
  }
  paramNames <- as.character(as.list(match.call()))[-1]
  if (!is.null(names(params))) {
    paramNames <- ifelse(names(params) == '', paramNames, names(params))
  }

  m <- params[[1]]
  assert_that(is.matrix(m))
  res <- which(m==m, arr.ind=TRUE, useNames=FALSE)
  res <- as.data.frame(res)
  for (i in seq_along(params)) {
    assert_that(is.matrix(params[[i]]))
    assert_that(all(dim(params[[i]] == dim(m))))
    res <- cbind(res, as.vector(params[[i]]))
  }
  colnames(res) <- c('row', 'col', paramNames)
  res
}


f <- function(...) {
  dots <- list(...)
  if (length(dots) == 0) {
    return(data.frame())
  }
  paramNames <- as.character(as.list(match.call()))[-1]
  print(names(dots))
  print(paramNames)
}
