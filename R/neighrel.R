#' Evaluate the relation between neighbours
#'
#' @export
neighrel <- function(f, ...) {
  dots <- list(...)
  if (length(dots) <= 0) {
    return(NULL)
  }
  m1 <- dots[[1]]
  for (i in seq_along(dots)) {
    d <- dots[[i]]
    assert_that(is.hexmatrix(d) || is.hexarray(d))
    assert_that(length(dim(d)) == length(dim(m1)))
    assert_that(all(dim(d) == dim(m1)))
  }
  assert_that(is.function(f))

  neigh <- lapply(dots, neighbours)
  dims <- dim(neigh[[1]])
  curr <- lapply(dots, function(m) array(m, dim=dims))
  indices <- rep(seq_along(dots), each=2) + c(0, length(dots))
  args <- c(neigh, curr)
  args <- args[indices]
  res <- do.call(mapply, c(list(f), args))
  array(res, dim=dims)
}
