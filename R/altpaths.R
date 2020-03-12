#' @export
altpaths <- function(m,
                     target,
                     n=1,
                     dist=1) {
  assert_that(is.matrix(m))
  assert_that(is.numeric(m))
  rows <- nrow(m)
  cols <- ncol(m)
  if (is.number(dist)) {
    dist <- matrix(dist, nrow=rows, ncol=cols)
  }
  if (is.matrix(dist)) {
    assert_that(ncol(dist) == cols)
    assert_that(nrow(dist) == rows)
    dist <- array(dist, dim=c(nrow(dist), ncol(dist), 6))
  }
  assert_that(is.array(dist))
  assert_that(all(dim(dist) == c(rows, cols, 6)))
  assert_that(is.numeric(target))
  assert_that(is.scalar(target))
  assert_that(target >= 1 && target <= length(m))
  assert_that(is.numeric(n))
  assert_that(is.scalar(n))

  assert_that(length(which(!is.na(m))) == 1)

  rowcol <- rows * cols;
  mNA <- like(m, data=NA_real_)
  res <- list()
  bPaths <- list()
  bPrices <- c()
  cheapest <- reachability(m, dist, target)
  res[[1]] <- rev(path(target, cheapest$paths)[[1]])

  for (k in seq_len(n - 1)) {
    currentPath <- res[[k]]

    for (i in seq_len(length(currentPath) - 1)) {
      spurNode <- currentPath[i]
      rootIndices <- seq_len(i)
      rootPath <- currentPath[rootIndices]

      modDist <- dist
      for (j in seq_along(res)) {
        otherPath <- res[[j]]
        if ((length(otherPath) > i) && all(rootPath == otherPath[rootIndices])) {
          otherNode <- otherPath[i + 1]
          dir <- whichDir(otherNode, spurNode, m)
          modDist[otherNode + rowcol * (dir - 1)] <- NA
        }
      }
      for (j in seq_len(i - 1)) {
        modDist[rootPath[j] + rowcol * 0:5] <- NA
      }
      modM <- mNA
      modM[spurNode] <- 0
      cheapest <- reachability(m=modM, dist=modDist, target=target)
      spurPath <- rev(path(target, cheapest$paths)[[1]])
      if (all(!is.na(spurPath))) {
        totalPath <- c(rootPath, spurPath[-1])
        if ((length(bPaths) == 0) || (sum(sapply(bPaths, identical, totalPath)) == 0)) { # totalPath is not in bPaths
          bPaths <- c(bPaths, list(totalPath))
          bPrices <- c(bPrices, cheapest$prices[target])
        }
      }
    }

    if (length(bPaths) == 0) {
      break;
    }

    o <- order(bPrices, decreasing=TRUE)
    bestIndex <- o[length(o)]
    res[[k + 1]] <- bPaths[[bestIndex]]
    bPrices <- bPrices[o][-length(o)]
    bPaths <- bPaths[o][-length(o)]
  }

  res
}
