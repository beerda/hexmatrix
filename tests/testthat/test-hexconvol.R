test_that("hexconvol(matrix)", {
  m <- matrix(1:12, nrow=4, byrow=TRUE)

  expect_equal(hexconvol(m, 1),
               matrix(c(12, 17, 45, 28, 17, 33, 58, 48, 11, 33, 35, 40), nrow=4))
})


test_that("hexconvol(array)", {
  m <- matrix(1:12, nrow=4, byrow=TRUE)
  a <- array(0, dim=c(4, 3, 2))
  a[, , 1] <- m
  a[, , 2] <- m

  expect_equal(hexconvol(a, 1)[, , 1],
               matrix(c(12, 17, 45, 28, 17, 33, 58, 48, 11, 33, 35, 40), nrow=4))
  expect_equal(hexconvol(a, 1)[, , 2],
               matrix(c(12, 17, 45, 28, 17, 33, 58, 48, 11, 33, 35, 40), nrow=4))

  a[, , 2] <- m[4:1, ]
  expect_equal(hexconvol(a, 1)[, , 1],
               matrix(c(12, 17, 45, 28, 17, 33, 58, 48, 11, 33, 35, 40), nrow=4))
  expect_equal(hexconvol(a, 1)[, , 2],
               matrix(c(36, 29, 27, 7, 50, 54, 37, 15, 32, 51, 23, 16), nrow=4))
})
