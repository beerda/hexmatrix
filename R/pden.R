#' @export
pden <- function(m, values, probs) {
  assert_that(is.matrix(m))
  assert_that(is.vector(values))
  assert_that(is.vector(probs))
  assert_that(is.numeric(probs))
  assert_that(length(values) == length(probs))
  assert_that(min(probs) >= 0)
  assert_that(max(probs) <= 1)

  if (min(probs) > 0) {
    probs <- c(0, probs)
    values <- c(0, values)
  }

  l <- rowcol(m)
  d <- data.frame(m=unique(l$m))
  d$cumProb <- approxfun(values, probs)(d$m)
  d <- d[order(d$m, d$cumProb), ]
  d$prob <- d$cumProb - c(NA, d$cumProb[-nrow(d)])
  nas <- is.na(d$prob)
  d$prob[nas] <- d$cumProb[nas]
  d$cumProb <- NULL

  l$count <- sapply(l$m, function(v) { sum(l$m == v, na.rm=TRUE) })
  l <- merge(l, d, by='m')
  l$pden <- l$prob / l$count
  l <- l[order(l$col, l$row), ]

  res <- like(m, data=l$pden)
  res
}
