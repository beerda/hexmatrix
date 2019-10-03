#' @export
normalize <-function(x, MARGIN=seq_len(length(dim(x) - 1)), SUM=sum, DIV=`/`) {
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
