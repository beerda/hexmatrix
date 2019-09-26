.listSum <- function(a, b) {
  a[is.na(a)] <- 0
  b[is.na(b)] <- 0
  a + b
}

.listIsNa <- function(a, b) {
  is.na(a) & is.na(b)
}


#' @export
listSum <- function(l,
                    na.rm=FALSE) {
  assert_that(is.list(l))
  assert_that(is.flag(na.rm))

  if (na.rm) {
    res <- Reduce(.listSum, l)
    nas <- Reduce(.listIsNa, l)
    res[nas] <- NA
    return(res)
  } else {
    res <- Reduce(`+`, l)
    return(res)
  }
}
