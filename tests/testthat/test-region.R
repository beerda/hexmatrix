test_that("region", {
  m <- matrix(1, nrow=4, ncol=3)
  expect_equal(region(m, 3), 1:12)

  m[1, 1] <- 2
  expect_equal(region(m, 3), 2:12)

  m[3, 1] <- 3
  expect_equal(region(m, 3), 3)

  m[4, ] <- 3
  expect_equal(region(m, 3), c(3, 4, 8, 12))

  m[, 3] <- 3
  expect_equal(region(m, 3), c(3, 4, 8:12))
})
