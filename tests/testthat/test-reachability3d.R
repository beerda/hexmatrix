test_that("reachability3d(single_layer) is equivalent to reachability()", {
  m <- matrix(NA_real_, nrow=4, ncol=3)
  m[2, 2] <- 0
  d <- array(1, dim=c(4, 3, 6))

  d[3, 1, 2] <- 2
  res <- reachability3d(list(m), dist=list(d), transitions=0)
  expect_equal(res,
               matrix(c(1,1,2,3, 1,0,1,2, 2,1,2,2), nrow=4, ncol=3, byrow=FALSE))

  d[4, 1, 2] <- 2
  res <- reachability3d(list(m), dist=list(d), transitions=0)
  expect_equal(res,
               matrix(c(1,1,2,3, 1,0,1,2, 2,1,2,2), nrow=4, ncol=3, byrow=FALSE))

  d[4, 1, 3] <- 2
  res <- reachability3d(list(m), dist=list(d), transitions=0)
  expect_equal(res,
               matrix(c(1,1,2,4, 1,0,1,2, 2,1,2,2), nrow=4, ncol=3, byrow=FALSE))
})


test_that("reachability3d simple", {
  m1 <- matrix(NA_real_, nrow=4, ncol=3)
  m2 <- matrix(NA_real_, nrow=4, ncol=3)
  m1[2, 2] <- 0

  d1 <- array(1, dim=c(4, 3, 6))
  d2 <- array(1, dim=c(4, 3, 6))
  tr <- array(1, dim=c(4, 3, 2))

  res <- reachability3d(list(m1, m2), dist=list(d1, d2), transitions=tr)
  expect_equal(res, matrix(c(1,1,1,2, 1,0,1,2, 2,1,2,2), nrow=4, byrow=FALSE))
})



test_that("reachability3d two origins", {
  m1 <- matrix(NA_real_, nrow=4, ncol=3)
  m2 <- matrix(NA_real_, nrow=4, ncol=3)
  m1[2, 2] <- 0
  m2[4, 3] <- 0

  d1 <- array(1, dim=c(4, 3, 6))
  d2 <- array(1, dim=c(4, 3, 6))
  tr <- array(1, dim=c(4, 3, 2))

  res <- reachability3d(list(m1, m2), dist=list(d1, d2), transitions=tr)
  expect_equal(res, matrix(c(1,1,1,2, 1,0,1,1, 2,1,1,0), nrow=4, byrow=FALSE))
})



test_that("reachability3d use layer2", {
  m1 <- matrix(NA_real_, nrow=4, ncol=3)
  m2 <- matrix(NA_real_, nrow=4, ncol=3)
  m1[2, 2] <- 0

  d1 <- array(3, dim=c(4, 3, 6))
  d2 <- array(1, dim=c(4, 3, 6))
  tr <- array(1, dim=c(4, 3, 2))

  res <- reachability3d(list(m1, m2), dist=list(d1, d2), transitions=tr)
  expect_equal(res, matrix(c(2,2,2,3, 2,0,2,3, 3,2,3,3), nrow=4, byrow=FALSE))
})

