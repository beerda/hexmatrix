#' @export
long <- function(..., .forPlot=FALSE) {
  assert_that(is.flag(.forPlot))

  params <- list(...)
  if (length(params) == 0) {
    return(data.frame())
  }
  paramNames <- as.character(as.list(match.call()))
  paramNames <- paramNames[-1]         # remove function name
  length(paramNames) <- length(params) # remove .forPlot
  if (!is.null(names(params))) {
    paramNames <- ifelse(names(params) == '', paramNames, names(params))
  }
  paramNames <- setdiff(paramNames, '.forPlot')

  m <- params[[1]]
  assert_that(is.hexmatrix(m) || is.hexarray(m))
  res <- which(m==m | is.na(m), arr.ind=TRUE, useNames=FALSE)
  res <- as.data.frame(res)
  for (i in seq_along(params)) {
    assert_that(is.hexmatrix(params[[i]]) || is.hexarray(params[[i]]))
    assert_that(length(dim(params[[i]])) == length(dim(m)))
    assert_that(all(dim(params[[i]]) == dim(m)))
    res <- cbind(res, as.vector(params[[i]]))
  }

  layername <- NULL
  if (is.hexarray(m)) {
    layername <- 'layer'
  }

  if (.forPlot) {
    colnames(res) <- c('y', 'x', layername, paramNames)
    oddy <- res$y %% 2 == 1
    res[oddy, 'x'] <- res[oddy, 'x'] + 0.5
  } else {
    colnames(res) <- c('row', 'col', layername, paramNames)
  }
  res
}
