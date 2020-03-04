test_that("pathReduce", {
  s <- matrix(1, nrow=4, ncol=3)
  m <- matrix(NA_real_, nrow=4, ncol=3)
  m[2, 2] <- 0
  reach <- reachability(m)

  res <- pathReduce(s, reach$paths, `+`)
  expect_equal(res,
               matrix(c(2, 2, 3, 2, 1, 2, 2, 2, 3, 3, 3, 3), byrow=TRUE, nrow=4))
})
