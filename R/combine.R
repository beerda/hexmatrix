#' Combine lists by elements
#'
#' Function returns a list combined by applying a function `f` pairwisely on \eqn{i}-th element
#' of a list `a` and `b`, for each \eqn{i}. If `a`'s (resp. `b`'s) \eqn{i}-th element is `NULL`,
#' the function `f` is not called and the result contains the `b`'s (resp. `a`'s) non-`NULL` element
#' at the \eqn{i}-th position. The resulting list has size of the larger list and the elements of the
#' shorter input lists are recycled to size of the larger list. The elements are combined by index.
#' Names of the elements are ignored.
#'
#' @param a the first list to be combined
#' @param b the second list to be combined
#' @param f the function to be called pairwisely on the elements of `a` and `b` to produce the combinded result
#' @return an unnamed list of elements
#'
#' @export
#'
#' @examples
#' combine(list(1, 2, 3), list(2, 4, 6), f=`+`)
#' # results in list(3, 6, 9)
#'
#' combine(list(1, 2, 3), list(2, 4), f=`+`)
#' # results in list(3, 6, 3)
#'
#' a <- list()
#' a[[3]] <- 10
#' b <- list()
#' a[[5]] <- 20
#' combine(a, b, f=`+`)
#' # results in list(NULL, NULL, 10, NULL, 20)
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
