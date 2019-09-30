#' Evaluate the relation between neighbours
#'
#' @export
neighrel <- function(f, ...) {
  dots <- list(...)
  if (length(dots) <= 0) {
    return(NULL)
  }
  m1 <- dots[[1]]
  assert_that(is.matrix(m1))
  for (i in seq_along(dots)) {
    assert_that(all(dim(dots[[i]]) == dim(m1)))
  }
  assert_that(is.function(f))

  ns <- lapply(dots, neighbours)
  dims <- dim(ns[[1]])

  ff <- function(x, y, direction) {
    a <- lapply(seq_along(dots), function(i) {
      c(ns[[i]][x, y, direction], dots[[i]][x, y])
    })
    a <- as.list(unlist(a, use.names=FALSE))
    do.call(f, a)
  }
  outer2(ff, seq_len(dims[1]), seq_len(dims[2]), seq_len(dims[3]))
}
