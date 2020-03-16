test_that("shortest", {
  d <- array(1, dim=c(4, 3, 6))
  d[3, 1, 2] <- 2
  d[4, 1, 2] <- 2
  d[4, 1, 3] <- 2

  expectedPrices <- matrix(c(1,1,2,4, 1, 0, 1, 2, 2, 1, 2, 2),
                           nrow=4, ncol=3, byrow=FALSE)
  expectedPaths <- matrix(c(6, 6, 6, 8, 6, 0, 6, 7, 10, 6, 10, 7),
                          nrow=4, ncol=3, byrow=FALSE)

  for (i in seq_len(12)) {
    p <- path(i, expectedPaths)[[1]]
    res <- shortest(source=6, target=i, dist=d)
    expect_equal(res$prices, rev(expectedPrices[p]))
    expect_equal(res$path, rev(p))
  }
})
