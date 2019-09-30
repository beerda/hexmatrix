#' @export
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
  array(res, dim=udims)
}
