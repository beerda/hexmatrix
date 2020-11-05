test_that("reachability (dist is number)", {
  m <- matrix(NA_real_, nrow=4, ncol=3)
  m[2, 2] <- 0

  res <- reachability(m)
  expect_equal(res$init, 6)
  expect_equal(res$prices,
               array(c(1,1,1,2, 1,0,1,2, 2,1,2,2),
                     dim=c(4, 3, 1)))
  expect_equal(res$paths,
               array(c(6, 6, 6, 3, 6, 0, 6, 7, 10, 6, 10, 7),
                     dim=c(4, 3, 1)))

  res <- reachability(m, dist=2)
  expect_equal(res$init, 6)
  expect_equal(res$prices,
               array(2 * c(1,1,1,2, 1,0,1,2, 2,1,2,2),
                     dim=c(4, 3, 1)))
  expect_equal(res$paths,
               array(c(6, 6, 6, 3, 6, 0, 6, 7, 10, 6, 10, 7),
                     dim=c(4, 3, 1)))
})


test_that("reachability (dist is matrix)", {
  m <- matrix(NA_real_, nrow=4, ncol=3)
  m[2, 2] <- 0
  d <- matrix(1, nrow=4, ncol=3)
  d[3, 1] <- 3

  res <- reachability(m, dist=d)
  expect_equal(res$init, 6)
  expect_equal(res$prices,
               array(c(1,1,1,3, 1,0,1,2, 2,1,2,2),
                     dim=c(4, 3, 1)))
  expect_equal(res$paths,
               array(c(6, 6, 6, 8, 6, 0, 6, 7, 10, 6, 10, 7),
                     dim=c(4, 3, 1)))
})


test_that("reachability (dist is array)", {
  m <- matrix(NA_real_, nrow=4, ncol=3)
  m[2, 2] <- 0
  d <- array(1, dim=c(4, 3, 1, 6))

  d[2, 2, 1, 5] <- 2
  res <- reachability(m, dist=d)
  expect_equal(res$init, 6)
  expect_equal(res$prices,
               array(c(1,1,2,3, 1,0,1,2, 2,1,2,2),
                     dim=c(4, 3, 1)))
  expect_equal(res$paths,
               array(c(6, 6, 6, 8, 6, 0, 6, 7, 10, 6, 10, 7),
                     dim=c(4, 3, 1)))

  d[3, 1, 1, 5] <- 2
  res <- reachability(m, dist=d)
  expect_equal(res$init, 6)
  expect_equal(res$prices,
               array(c(1,1,2,3, 1,0,1,2, 2,1,2,2),
                     dim=c(4, 3, 1)))
  expect_equal(res$paths,
               array(c(6, 6, 6, 8, 6, 0, 6, 7, 10, 6, 10, 7),
                     dim=c(4, 3, 1)))

  d[4, 2, 1, 6] <- 2
  res <- reachability(m, dist=d)
  expect_equal(res$init, 6)
  expect_equal(res$prices,
               array(c(1,1,2,4, 1,0,1,2, 2,1,2,2),
                     dim=c(4, 3, 1)))
  expect_equal(res$paths,
               array(c(6, 6, 6, 8, 6, 0, 6, 7, 10, 6, 10, 7),
                     dim=c(4, 3, 1)))
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

  d <- array(1, dim=c(4, 3, 1, 6))
  d[2, 2, 1, 5] <- 2
  d[3, 1, 1, 5] <- 2
  d[4, 2, 1, 6] <- 2

  expectedPrices <- array(c(1,1,2,4, 1, 0, 1, 2, 2, 1, 2, 2),
                          dim=c(4, 3, 1))
  expectedPaths <- array(c(6, 6, 6, 8, 6, 0, 6, 7, 10, 6, 10, 7),
                         dim=c(4, 3, 1))

  for (i in seq_len(length(m))) {
    p <- path(i, expectedPaths)[[1]]
    res <- reachability(m, dist=d, target=i)
    expect_equal(res$prices[p], expectedPrices[p])
    expect_equal(res$paths[p], expectedPaths[p])
  }
})

test_that("reachability bug?", {
  dist <- matrix(1, nrow=4, ncol=3)
  m <- array(NA_real_, dim=c(4, 3, 2))
  m[2, 2, 1] <- 0
  res <- reachability(m, dist, 0)

  expect_equal(res$init, 6)
  expect_equal(res$paths,
               array(c(6, 6, 6, 3,
                       6, 0, 6, 7,
                       10, 6, 10, 7,
                       18, 18, 18, 15,
                       18, 6, 18, 19,
                       22, 18, 22, 19), dim=c(4, 3, 2)))
})
