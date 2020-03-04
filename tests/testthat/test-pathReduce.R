test_that("pathReduce", {
  s <- matrix(1, nrow=4, ncol=3)
  m <- matrix(NA_real_, nrow=4, ncol=3)
  m[2, 2] <- 0
  reach <- reachability(m)

  res <- pathReduce(s, reach$paths, 0, `+`)
  expect_equal(res,
               matrix(c(1, 1, 2, 1, 0, 1, 1, 1, 2, 2, 2, 2), byrow=TRUE, nrow=4))

  res <- pathReduce(s, reach$paths, 1, `+`)
  expect_equal(res,
               matrix(c(2, 2, 3, 2, 1, 2, 2, 2, 3, 3, 3, 3), byrow=TRUE, nrow=4))
})
