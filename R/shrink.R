#' Rescale the given hexmatrix by factor of 0.5. The resulting hexmatrix would
#' have half of rows and half of columns of the original matrix. The values in
#' cells will be summed appropriately.
#'
#' @param m A hexmatrix to shrink.
#' @return A rescaled hexmatrix.
#' @export
shrink <- function(m) {
  assert_that(is.hexmatrix(m))

  orig <- dim(m)
  if (any(orig < 1)) {
    return(matrix(0, nrow=0, ncol=0))
  }

  m <- cbind(0, 0, m, 0, 0, 0)
  m <- rbind(0, 0, m, 0, 0, 0)
  n <- neighbours(m, self=TRUE)

  resRows <- floor(orig[1] / 2) + 1
  if (orig[1] <= 2) {
    resCols <- floor(orig[2] / 2) + 1
  } else {
    resCols <- ceiling(orig[2] / 2) + 1
  }

  evnRows <- floor(resRows / 2)
  oddRows <- resRows - evnRows

  odds <- n[seq(from=3, by=4, length.out=oddRows),
            seq(from=3, by=2, length.out=resCols),
            , drop=FALSE]
  oddSum <- odds[, , 7] + (odds[, , 1] +  odds[, , 2] + odds[, , 3] + odds[, , 4] + odds[, , 5] + odds[, , 6]) / 2

  evns <- n[seq(from=5, by=4, length.out=evnRows),
            seq(from=2, by=2, length.out=resCols),
            , drop=FALSE]
  evnSum <- evns[, , 7] + (evns[, , 1] +  evns[, , 2] + evns[, , 3] + evns[, , 4] + evns[, , 5] + evns[, , 6]) / 2

  res <- matrix(NA_real_, nrow=resRows, ncol=resCols)
  res[seq(from=1, to=resRows, by=2), ] <- oddSum
  if (resRows > 1) {
    res[seq(from=2, to=resRows, by=2), ] <- evnSum
  }
  res
}
