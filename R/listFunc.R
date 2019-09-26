#' @export
elementwisely <- function(what) {
  function(l, ...) {
    assert_that(is.list(l))
    dots <- list(...)

    f <- function(...) {
      do.call(what, c(list(c(...)), dots))
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

elementwisely <- function(l, FUN, ...) {
  assert_that(is.list(l))
  assert_that(is.function(FUN))
  dots <- list(...)

  f <- function(...) {
    do.call(FUN, c(list(c(...)), dots))
  }

  if (length(l) == 0) {
    return(list())
  } else {
    res <- l[[1]]
    params <- list(FUN=f, SIMPLIFY=TRUE)
    res[] <- do.call(mapply, c(l, params))
    return(res)
  }
}


#' @export
listSum <- function(l, na.rm=FALSE) {
  elementwisely(l, sum, na.rm=na.rm)
}

#' @export
listMean <- function(l, na.rm=FALSE) {
  elementwisely(l, mean, na.rm=na.rm)
}

#' @export
listMin <- function(l, na.rm=FALSE) {
  elementwisely(l, min, na.rm=na.rm)
}

#' @export
listMax <- function(l, na.rm=FALSE) {
  elementwisely(l, max, na.rm=na.rm)
}
