#' Outer product of multiple arrays
#'
#' This function works similarly as `base::outer()` except that it allows the
#' multiplication of more arguments than just two.
#'
#' @param f The function to be used on the outer products
#' @param ... Vectors, matrices or arrays whose values will be used as arguments
#'   to the function `f`. The elements of the first argument will be used as
#'   the first argument to `f`, elements of the second argument will be used as
#'   the second argument to `f` etc.
#' @return An array that is an outer product of the input arguments.
#' @export
#' @examples
#' outer2(prod, 1:4)           # returns 1:4
#' outer2(prod, 1:4, 1:3)      # the same as 1:4 %o% 1:3
#' outer2(prod, 1:4, 1:3, 1:2) # the same as (1:4 %o% 1:3) %o% 1:2
#'
#' # to see how f is called with arguments:
#' outer2(paste, 1:4, 1:3, 1:2)
outer2 <- function(f, ...) {
  dots <- lapply(list(...), as.array)
  dims <- lapply(dots, dim)
  udims <- unlist(dims)
  pdims <- sapply(dims, prod)
  ddims <- c(1, cumprod(pdims))
  if (length(udims) <= 0) {
    return(NULL)
  }

  res <- sapply(seq_len(prod(udims)), function(i) {
    a <- list()
    for (p in seq_along(pdims)) {
      index <- floor((i-1) / ddims[p]) %% pdims[p] + 1
      a[[p]] <- dots[[p]][index]
    }
    do.call(f, a)
  })

  res <- array(res, dim=udims)
  if (length(dim(res)) == 1) {
    res <- as.vector(res)
  }

  res
}
