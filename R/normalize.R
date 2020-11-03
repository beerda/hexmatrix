#' Normalize the values of the array by given margin
#'
#' By normalization we understand dividing each value of the cell by a sum
#' computed along the given `MARGIN`. The summation and division may be replaced
#' with custom functions `SUM` and `DIV`, respectively.
#'
#' @param x An array to be normalized
#' @param MARGIN A margin, over which to perform the normalization.
#' @param SUM The summationfunction
#' @param DIV The division function
#' @return An array of dimensions equal to `x` with normalized values
#' @examples
#' m <- matrix(1:12, nrow=4)
#' r <- normalize(m, 1)   # normalize over the rows
#' rowSums(r)             # returns a vector of 1s
#' s <- normalize(m, 2)   # normalize over the columns
#' colSums(s)             # returns a vector of 1s
#' @export
normalize <-function(x, MARGIN=seq_len(length(dim(x) - 1)), SUM=sum, DIV=`/`) {
  assert_that(is.array(x))
  assert_that(is.vector(MARGIN))
  assert_that(is.numeric(MARGIN))
  assert_that(all(MARGIN %in% seq_along(dim(x))))
  assert_that(is.function(SUM))
  assert_that(is.function(DIV))

  remain <- setdiff(seq_along(dim(x)), MARGIN)
  perm <- c(MARGIN, remain)

  revperm <- perm
  names(revperm) <- seq_along(perm)
  revperm <- sort(revperm)
  revperm <- as.numeric(names(revperm))

  xperm <- aperm(x, perm)
  res <- apply(x, MARGIN, SUM, na.rm=TRUE)
  res <- array(res, dim=c(dim(x)[perm]))
  res <- DIV(xperm, res)
  aperm(res, revperm)
}
