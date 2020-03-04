test_that("reachability (dist is number)", {
  m <- matrix(NA_real_, nrow=4, ncol=3)
  m[2, 2] <- 0

  res <- reachability(m)
  expect_equal(res$initRows, 2)
  expect_equal(res$initCols, 2)
  expect_equal(res$prices,
               matrix(c(1,1,1,2, 1,0,1,2, 2,1,2,2), nrow=4, ncol=3, byrow=FALSE))
  expect_equal(res$paths,
               matrix(c(4, 5,
                        5, # or 6
                        3, 0, 6, 2, 1,
                        1, # or 6
                        2,
                        1, # or 2
                        1), nrow=4, ncol=3, byrow=TRUE))

  res <- reachability(m, dist=2)
  expect_equal(res$initRows, 2)
  expect_equal(res$initCols, 2)
  expect_equal(res$prices,
               matrix(2 * c(1,1,1,2, 1,0,1,2, 2,1,2,2), nrow=4, ncol=3, byrow=FALSE))
  expect_equal(res$paths,
               matrix(c(4, 5,
                        5, # or 6
                        3, 0, 6, 2, 1,
                        1, # or 6
                        2,
                        1, # or 2
                        1), nrow=4, ncol=3, byrow=TRUE))
})


test_that("reachability (dist is matrix)", {
  m <- matrix(NA_real_, nrow=4, ncol=3)
  m[2, 2] <- 0
  d <- matrix(1, nrow=4, ncol=3)
  d[3, 1] <- 3

  res <- reachability(m, dist=d)
  expect_equal(res$initRows, 2)
  expect_equal(res$initCols, 2)
  expect_equal(res$prices,
               matrix(c(1,1,3,3, 1,0,1,2, 2,1,2,2), nrow=4, ncol=3, byrow=FALSE))
  expect_equal(res$paths,
               matrix(c(4, 5,
                        5, # or 6
                        3, 0, 6, 2, 1,
                        1, # or 6
                        3,
                        2,
                        1), nrow=4, ncol=3, byrow=TRUE))
})


test_that("reachability (dist is array)", {
  m <- matrix(NA_real_, nrow=4, ncol=3)
  m[2, 2] <- 0
  d <- array(1, dim=c(4, 3, 6))

  d[3, 1, 2] <- 2
  res <- reachability(m, dist=d)
  expect_equal(res$initRows, 2)
  expect_equal(res$initCols, 2)
  expect_equal(res$prices,
               matrix(c(1,1,2,3, 1,0,1,2, 2,1,2,2), nrow=4, ncol=3, byrow=FALSE))
  expect_equal(res$paths,
               matrix(c(4, 5,
                        5, # or 6
                        3, 0, 6, 2, 1,
                        1, # or 6
                        2,
                        2,
                        1), nrow=4, ncol=3, byrow=TRUE))

  d[4, 1, 2] <- 2
  res <- reachability(m, dist=d)
  expect_equal(res$initRows, 2)
  expect_equal(res$initCols, 2)
  expect_equal(res$prices,
               matrix(c(1,1,2,3, 1,0,1,2, 2,1,2,2), nrow=4, ncol=3, byrow=FALSE))
  expect_equal(res$paths,
               matrix(c(4, 5,
                        5, # or 6
                        3, 0, 6, 2, 1,
                        1, # or 6
                        3,
                        2,
                        1), nrow=4, ncol=3, byrow=TRUE))

  d[4, 1, 3] <- 2
  res <- reachability(m, dist=d)
  expect_equal(res$initRows, 2)
  expect_equal(res$initCols, 2)
  expect_equal(res$prices,
               matrix(c(1,1,2,4, 1,0,1,2, 2,1,2,2), nrow=4, ncol=3, byrow=FALSE))
  expect_equal(res$paths,
               matrix(c(4, 5,
                        5, # or 6
                        3, 0, 6, 2, 1,
                        1, # or 6
                        2,
                        2,
                        1), nrow=4, ncol=3, byrow=TRUE))
})


test_that("reachability (init values)", {
  m <- matrix(NA_real_, nrow=4, ncol=3)
  m[1, 2] <- 0
  m[3, 1] <- 0

  res <- reachability(m)
  expect_equal(res$initRows, c(3, 1))
  expect_equal(res$initCols, c(1, 2))
})
