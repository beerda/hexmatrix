.forbidden <- function(dist, path, dilat) {
  d <- dim(dist)
  mask <- matrix(FALSE, nrow=d[1], ncol=d[2])

  if (length(path) <= 2 * dilat) {
    mask[path] <- TRUE
  } else {
    i <- seq_len(dilat)
    path <- path[c(-i, -1 * (length(path) - i + 1))]
    mask[path] <- TRUE
    for (i in seq_len(dilat)) {
      mask <- morph(mask, dilate=TRUE, na.rm=TRUE)
    }
  }

  mask <- rep(mask, 6)
  dist[mask] <- dist[mask] * 100
  dist
}


#' @export
altpaths <- function(m,
                     target,
                     n=1,
                     priceThresh=1.5,
                     dist=1,
                     step=1,
                     dilat=1) {
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
  resPath <- list()
  resPrices <- list()
  bPaths <- list()
  bPrices <- list()
  bPricesAll <- c()
  cheapest <- reachability(m, dist, target)
  resPath[[1]] <- rev(path(target, cheapest$paths)[[1]])
  resPrices[[1]] <- cheapest$prices[resPath[[1]]]
  priceThreshold <- priceThresh * cheapest$prices[target]

  for (k in seq_len(n - 1)) {
    currentPath <- resPath[[k]]
    currentPrices <- resPrices[[k]]

    to <- length(currentPath) - step - 1
    if (to >= 1) {
      for (i in seq(from=1, to=to, by=step)) {
        spurNode <- currentPath[i]
        rootIndices <- seq_len(i)
        rootPath <- currentPath[rootIndices]
        rootPrices <- currentPrices[rootIndices]

        modDist <- dist
        for (j in seq_along(resPath)) {
          otherPath <- resPath[[j]]
          if ((length(otherPath) > i) && all(rootPath == otherPath[rootIndices])) {
            forbid <- otherPath[-rootIndices]
            length(forbid) <- min(length(forbid) - 1, step)
            modDist <- .forbidden(modDist, forbid, dilat=dilat)
          }
        }
        modDist <- .forbidden(modDist, rootPath[-i], dilat=1)

        modM <- mNA
        modM[spurNode] <- 0

        cheapest <- reachability(m=modM, dist=modDist, target=target)
        spurPath <- rev(path(target, cheapest$paths)[[1]])
        if (all(!is.na(spurPath))) {
          totalPath <- c(rootPath, spurPath[-1])
          totalPrices <-  c(rootPrices, rootPrices[length(rootPrices)] + cheapest$prices[spurPath[-1]])
          finalPrice <- totalPrices[length(totalPrices)]
          if (finalPrice < priceThreshold) {
            if ((length(bPaths) == 0) || (sum(sapply(bPaths, identical, totalPath)) == 0)) { # totalPath is not in bPaths
              bPaths <- c(bPaths, list(totalPath))
              bPrices <- c(bPrices, list(totalPrices))
              bPricesAll <- c(bPricesAll, finalPrice)
            }
          }
        }
      }
    }

    if (length(bPaths) == 0) {
      break;
    }

    o <- order(bPricesAll, decreasing=TRUE)
    bestIndex <- o[length(o)]
    resPath[[k + 1]] <- bPaths[[bestIndex]]
    resPrices[[k + 1]] <- bPrices[[bestIndex]]
    bPrices <- bPrices[o][-length(o)]
    bPricesAll <- bPricesAll[o][-length(o)]
    bPaths <- bPaths[o][-length(o)]
  }

  resPath
}


#' @export
altpaths2 <- function(source,
                      target,
                      dist,
                      n=1,
                      step=1,
                      dilat=1) {
  if (is.matrix(dist)) {
    dist <- array(dist, dim=c(nrow(dist), ncol(dist), 6))
  }
  assert_that(is.array(dist))

  assert_that(is.scalar(source))
  assert_that(is.numeric(source))
  assert_that(source >= 1 && source <= length(dist))

  assert_that(is.scalar(target))
  assert_that(is.numeric(target))
  assert_that(target >= 1 && target <= length(dist))

  assert_that(is.scalar(n) && is.numeric(n))
  assert_that(is.scalar(step) && is.numeric(step))
  assert_that(is.scalar(dilat) && is.numeric(dilat))

  res <- .Call('_hexmatrix_altpaths', PACKAGE = 'hexmatrix',
               source - 1, target - 1, dist, n, step, dilat)
  res <- lapply(res, function(r) {
    r$path <- r$path + 1
    r
  })

  res
}
