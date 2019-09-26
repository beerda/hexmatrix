.listManipulationGenerator <- function(what) {
  function(l, na.rm=FALSE) {
    assert_that(is.list(l))
    assert_that(is.flag(na.rm))

    f <- function(...) {
      what(c(...), na.rm=na.rm)
    }
    if (length(l) == 0) {
      return(list())
    } else {
      res <- l[[1]]
      args <- list(FUN=f, SIMPLIFY=TRUE)
      res[] <- do.call(mapply, c(l, args))
      return(res)
    }
  }
}

#' @export
listSum <- .listManipulationGenerator(sum)
