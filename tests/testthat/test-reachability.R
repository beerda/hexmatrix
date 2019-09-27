test_that("reachability (dist is number)", {
  m <- matrix(NA_real_, nrow=4, ncol=3)
  m[2, 2] <- 0

  expect_equal(reachability(m),
               matrix(c(1,1,1,2, 1,0,1,2, 2,1,2,2), nrow=4, ncol=3, byrow=FALSE))
  expect_equal(reachability(m, dist=2),
               matrix(2 * c(1,1,1,2, 1,0,1,2, 2,1,2,2), nrow=4, ncol=3, byrow=FALSE))
})


test_that("reachability (dist is matrix)", {
  m <- matrix(NA_real_, nrow=4, ncol=3)
  m[2, 2] <- 0
  d <- matrix(1, nrow=4, ncol=3)
  d[3, 1] <- 2
  expect_equal(reachability(m, dist=d),
               matrix(c(1,1,2,3, 1,0,1,2, 2,1,2,2), nrow=4, ncol=3, byrow=FALSE))
})


test_that("reachability (dist is array)", {
  m <- matrix(NA_real_, nrow=4, ncol=3)
  m[2, 2] <- 0
  d <- array(1, dim=c(4, 3, 6))
  d[3, 1, 2] <- 2
  expect_equal(reachability(m, dist=d),
               matrix(c(1,1,2,3, 1,0,1,2, 2,1,2,2), nrow=4, ncol=3, byrow=FALSE))

  d[4, 1, 2] <- 2
  expect_equal(reachability(m, dist=d),
               matrix(c(1,1,2,3, 1,0,1,2, 2,1,2,2), nrow=4, ncol=3, byrow=FALSE))

  d[4, 1, 3] <- 2
  expect_equal(reachability(m, dist=d),
               matrix(c(1,1,2,4, 1,0,1,2, 2,1,2,2), nrow=4, ncol=3, byrow=FALSE))
})
