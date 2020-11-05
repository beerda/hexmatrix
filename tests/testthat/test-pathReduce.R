test_that("pathReduce(single layer)", {
  dist <- matrix(1, nrow=4, ncol=3)
  m <- matrix(NA_real_, nrow=4, ncol=3)
  m[2, 2] <- 0
  reach <- reachability(m)

  res <- pathReduce(f=`+`, origin=0, path=reach$paths, dist=dist, trans=100)
  expect_equal(res,
               matrix(c(1, 1, 2, 1, 0, 1, 1, 1, 2, 2, 2, 2), byrow=TRUE, nrow=4))

  res <- pathReduce(f=`+`, origin=1, path=reach$paths, dist=dist, trans=100)
  expect_equal(res,
               matrix(c(2, 2, 3, 2, 1, 2, 2, 2, 3, 3, 3, 3), byrow=TRUE, nrow=4))
})


test_that("pathReduce(two layers)", {
  m <- array(NA_real_, dim=c(4, 3, 2))
  m[2, 2, 1] <- 0
  reach <- reachability(m)
  reach$paths[1, 2, 2] <- 5

  dist <- array(1, dim=c(4, 3, 2, 6))
  dist[3, 2, 1, 1:6] <- 10 * 1:6

  trans <- array(100, dim=c(4, 3, 2))
  trans[1, 2, 1] <- 250
  trans[1, 2, 2] <- 500

  res <- pathReduce(f=`+`, origin=0, path=reach$paths, dist=dist, trans=trans)
  expect_equal(res,
               array(c(1, 1, 1, 2,
                       1, 0, 1, 51,
                       2, 1, 2, 41,
                       101, 101, 101, 102,
                       251, 100, 101, 102,
                       102, 101, 102, 102), dim=c(4, 3, 2)))
})
