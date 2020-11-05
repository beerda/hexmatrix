#' @export
reachability3d <- function(layers,
                           dists=1,
                           transitions=0,
                           target=NULL) {
  .Deprecated('reachability')

  # layers assertions
  if (is.matrix(layers)) {
    layers <- list(layers)
  }
  assert_that(is.list(layers))
  assert_that(all(sapply(layers, is.matrix)))
  m1 <- layers[[1]]
  assert_that(all(sapply(layers, function(m) {
    all(dim(m) == dim(m1))
  })))

  rows <- nrow(m1)
  cols <- ncol(m1)
  nlay <- length(layers)

  # dists assertions
  if (is.number(dists)) {
    dists <- matrix(dists, nrow=rows, ncol=cols)
  }
  if (is.matrix(dists)) {
    assert_that(ncol(dists) == cols)
    assert_that(nrow(dists) == rows)
    dists <- array(dists, dim=c(rows, cols, 6))
  }
  if (is.array(dists)) {
    assert_that(all(dim(dists) == c(rows, cols, 6)))
    dists <- list(dists)
    dists <- dists[rep(1, nlay)]
  }
  assert_that(is.list(dists))
  assert_that(length(dists) == nlay)
  assert_that(all(sapply(dists, is.array)))
  d1 <- dists[[1]]
  assert_that(all(sapply(dists, function(d) {
    all(dim(d) == dim(d1))
  })))

  # transitions assertions
  if (is.number(transitions)) {
    transitions <- matrix(transitions, nrow=rows, ncol=cols)
  }
  if (is.matrix(transitions)) {
    assert_that(ncol(transitions) == cols)
    assert_that(nrow(transitions) == rows)
    transitions <- array(transitions, dim=c(rows, cols, nlay))
  }
  assert_that(is.array(transitions))
  assert_that(all(dim(transitions) == c(rows, cols, nlay)))

  # target assertions
  if (!is.null(target)) {
    assert_that(is.numeric(target))
    assert_that(is.scalar(target))
    assert_that(target >= 1 && target <= length(m1))
  } else {
    target <- -1   # search cheapest paths to all cells
  }


  seql <- seq_along(layers)
  changed <- TRUE

  while (changed) {
    changed <- FALSE
    # process each layer
    for (l in seql) {
      m <- layers[[l]]
      d <- dists[[l]]

      res <- .Call('_hexmatrix_reachability',
                   PACKAGE = 'hexmatrix',
                   m,
                   d,
                   array(0, dim=c(rows, cols, 0)),
                   target - 1)
      res$paths <- res$paths + 1
      res$init <- res$init + 1

      layers[[l]] <- res$prices
      changed <- changed || res$changed
    }

    # process transitions between layers
    tt <- 0
    for (from in seql) {
      froml <- layers[[from]]
      for (to in seql) {
        if (from != to) {
          tt <- tt + 1
          tol <- layers[[to]]
          newl <- froml + transitions[, , tt]
          indices <- which(is.na(tol) | tol > newl)
          if (length(indices) > 0) {
            layers[[to]][indices] <- newl[indices]
            changed <- TRUE
          }
        }
      }
    }
  }

  do.call(pmin, c(layers, na.rm=TRUE))
}
