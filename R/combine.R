#' @export
combine <- function(a, b, f=`+`) {
  assert_that(is.list(a))
  assert_that(is.list(b))
  assert_that(is.function(f))

  m <- max(length(a), length(b))
  length(a) <- m
  length(b) <- m
  res <- list()
  for (i in seq_len(m)) {
    if (!is.null(a[[i]])) {
      if (!is.null(b[[i]])) {
        res[[i]] <- f(a[[i]], b[[i]])
      } else {
        res[[i]] <- a[[i]]
      }
    } else {
      if (!is.null(b[[i]])) {
        res[[i]] <- b[[i]]
      }
    }
  }
  res
}
