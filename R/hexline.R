.ystep <- sqrt(3) / 2

.index2real <- function(m, index) {
  p <- point(m, index)
  c((p[1] - 1) * .ystep,
    (p[2] - 1) + 0.5 * (p[1] %% 2))
}

.real2index <- function(m, real) {
  p <- c(real[1] / .ystep + 1,
         floor(real[2]) + 1)
  index(m, as.integer(p))
}

#' @export
hexline <- function(m, start, end) {
  assert_that(is.hexmatrix(m) || is.hexarray(m))
  assert_that(is.count(start))
  assert_that(start <= length(m))
  assert_that(is.count(end))
  assert_that(end <= length(m))
  assert_that(start %/% (nrow(m) * ncol(m)) == end %/% (nrow(m) * ncol(m)))

  from <- min(start, end)
  to <- max(start, end)

  fromR <- .index2real(m, from)
  toR <- .index2real(m, to)
  rowDiff <- toR[1] - fromR[1]
  colDiff <- toR[2] - fromR[2]

  dirs <- NA
  if (rowDiff >= 0) {
    if (colDiff >= 0) {
      dirs <- c(3, 4, 5)
    } else {
      dirs <- c(4, 5, 6)
    }
  } else {
    if (colDiff >= 0) {
      dirs <- c(1, 2, 3)
    } else {
      dirs <- c(1, 2, 6)
    }
  }

  baseFrac <- toR - fromR
  baseFrac <- baseFrac[1] / baseFrac[2]
  if (!is.finite(baseFrac)) {
    return(seq(from, to))
  }

  cur <- from
  curR <- fromR
  res <- c(cur)
  while (cur != to) {
    minDist <- Inf
    bestI <- NA
    for (dir in dirs) {
      nextI <- neighbour(m, cur, dir)
      if (!is.na(nextI)) {
        tryR <- .index2real(m, nextI)
        dist <- abs(baseFrac * (tryR[2] - fromR[2]) - tryR[1] + fromR[1])
        if (dist < minDist) {
          minDist <- dist
          bestI <- nextI
        }
      }
    }

    cur <- bestI
    res <- c(res, cur)
  }

  res
}
