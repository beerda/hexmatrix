test_that("reachability (dist is number)", {
  m <- matrix(NA_real_, nrow=4, ncol=3)
  m[2, 2] <- 0

  res <- reachability(m)
  expect_equal(res$init, 6)
  expect_equal(res$prices,
               matrix(c(1,1,1,2, 1,0,1,2, 2,1,2,2), nrow=4, ncol=3, byrow=FALSE))
  expect_equal(res$paths,
               matrix(c(6, 6, 6, 3, 6, 0, 6, 3, 10, 6, 10, 7),
                      nrow=4, ncol=3, byrow=FALSE))

  res <- reachability(m, dist=2)
  expect_equal(res$init, 6)
  expect_equal(res$prices,
               matrix(2 * c(1,1,1,2, 1,0,1,2, 2,1,2,2), nrow=4, ncol=3, byrow=FALSE))
  expect_equal(res$paths,
               matrix(c(6, 6, 6, 3, 6, 0, 6, 3, 10, 6, 10, 7),
                      nrow=4, ncol=3, byrow=FALSE))
})


test_that("reachability (dist is matrix)", {
  m <- matrix(NA_real_, nrow=4, ncol=3)
  m[2, 2] <- 0
  d <- matrix(1, nrow=4, ncol=3)
  d[3, 1] <- 3

  res <- reachability(m, dist=d)
  expect_equal(res$init, 6)
  expect_equal(res$prices,
               matrix(c(1,1,3,3, 1,0,1,2, 2,1,2,2), nrow=4, ncol=3, byrow=FALSE))
  expect_equal(res$paths,
               matrix(c(6, 6, 6, 8, 6, 0, 6, 7, 10, 6, 10, 7),
                      nrow=4, ncol=3, byrow=FALSE))
})


test_that("reachability (dist is array)", {
  m <- matrix(NA_real_, nrow=4, ncol=3)
  m[2, 2] <- 0
  d <- array(1, dim=c(4, 3, 6))

  d[3, 1, 2] <- 2
  res <- reachability(m, dist=d)
  expect_equal(res$init, 6)
  expect_equal(res$prices,
               matrix(c(1,1,2,3, 1,0,1,2, 2,1,2,2), nrow=4, ncol=3, byrow=FALSE))
  expect_equal(res$paths,
               matrix(c(6, 6, 6, 3, 6, 0, 6, 7, 10, 6, 10, 7),
                      nrow=4, ncol=3, byrow=FALSE))

  d[4, 1, 2] <- 2
  res <- reachability(m, dist=d)
  expect_equal(res$init, 6)
  expect_equal(res$prices,
               matrix(c(1,1,2,3, 1,0,1,2, 2,1,2,2), nrow=4, ncol=3, byrow=FALSE))
  expect_equal(res$paths,
               matrix(c(6, 6, 6, 8, 6, 0, 6, 7, 10, 6, 10, 7),
                      nrow=4, ncol=3, byrow=FALSE))

  d[4, 1, 3] <- 2
  res <- reachability(m, dist=d)
  expect_equal(res$init, 6)
  expect_equal(res$prices,
               matrix(c(1,1,2,4, 1,0,1,2, 2,1,2,2), nrow=4, ncol=3, byrow=FALSE))
  expect_equal(res$paths,
               matrix(c(6, 6, 6, 3, 6, 0, 6, 7, 10, 6, 10, 7),
                      nrow=4, ncol=3, byrow=FALSE))
})


test_that("reachability (init values)", {
  m <- matrix(NA_real_, nrow=4, ncol=3)
  m[1, 2] <- 0
  m[3, 1] <- 0

  res <- reachability(m)
  expect_equal(res$init, c(3, 5))
})


test_that("reachability to a specific target", {
  m <- matrix(NA_real_, nrow=4, ncol=3)
  m[2, 2] <- 0

  d <- array(1, dim=c(4, 3, 6))
  d[3, 1, 2] <- 2
  d[4, 1, 2] <- 2
  d[4, 1, 3] <- 2

  expectedPrices <- matrix(c(1,1,2,4, 1, 0, 1, 2, 2, 1, 2, 2),
                           nrow=4, ncol=3, byrow=FALSE)
  expectedPaths <- matrix(c(6, 6, 6, 3, 6, 0, 6, 7, 10, 6, 10, 7),
                          nrow=4, ncol=3, byrow=FALSE)

  for (i in seq_len(length(m))) {
    p <- path(i, expectedPaths)[[1]]
    res <- reachability(m, dist=d, target=i)
    expect_equal(res$prices[p], expectedPrices[p])
    expect_equal(res$paths[p], expectedPaths[p])
  }
})
