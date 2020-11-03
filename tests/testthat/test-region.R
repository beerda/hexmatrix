test_that("region(matrix)", {
  m <- matrix(1, nrow=4, ncol=3)
  expect_error(region(m, 0))
  expect_error(region(m, 13))
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


test_that("region(array)", {
  m <- array(1, dim=c(4, 3, 2))
  expect_error(region(m, 0))
  expect_error(region(m, 25))
  expect_equal(region(m, 3), 1:12)
  expect_equal(region(m, 15), 13:24)

  m[1, 1, 1] <- 2
  expect_equal(region(m, 3), 2:12)
  expect_equal(region(m, 15), 13:24)

  m[3, 1, 1] <- 3
  expect_equal(region(m, 3), 3)
  expect_equal(region(m, 15), 13:24)

  m[4, , 1] <- 3
  expect_equal(region(m, 3), c(3, 4, 8, 12))
  expect_equal(region(m, 15), 13:24)

  m[, 3, 1] <- 3
  expect_equal(region(m, 3), c(3, 4, 8:12))
  expect_equal(region(m, 15), 13:24)
})
