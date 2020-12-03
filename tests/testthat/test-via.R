test_that("via", {
  m <- matrix(NA_real_, nrow=4, ncol=3)
  m[2, 2] <- 0
  d <- array(1, dim=c(4, 3, 1, 6))
  r <- reachability(m, dist=d)

  ee <- like(m, data=TRUE)
  expect_equal(via(r$paths, 6), ee)

  ee <- like(m, data=FALSE)
  ee[9:11]  <- TRUE
  expect_equal(via(r$paths, 10), ee)
})
