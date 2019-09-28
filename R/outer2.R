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

  #ntimes <- c(rev(cumprod(rev(udims[-1]))), 1)
  #neach <- c(1, cumprod(udims[-length(udims)]))
  #indices <- NULL
  #for (d in seq_along(udims)) {
    #i <- seq_len(udims[d])
    #indices <- c(indices, rep(i, each=neach[d], times=ntimes[d]))
  #}
  #indices <- matrix(indices, ncol=length(udims))

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
