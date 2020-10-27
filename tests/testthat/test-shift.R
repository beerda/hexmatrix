test_that("shift down", {
  m <- matrix(1:12, nrow=4, ncol=3, byrow=TRUE)
  expect_equal(shiftDown(m),
               matrix(c(rep(NA, 3), 1:9), nrow=4, ncol=3, byrow=TRUE))

  m <- array(1:24, dim=c(4, 3, 2))
  expect_equal(shiftDown(m),
               array(c(NA, 1, 2, 3,
                       NA, 5, 6, 7,
                       NA, 9, 10, 11,
                       NA, 13, 14, 15,
                       NA, 17, 18, 19,
                       NA, 21, 22, 23), dim=c(4, 3, 2)))
})


test_that("shift up", {
  m <- matrix(1:12, nrow=4, ncol=3, byrow=TRUE)
  expect_equal(shiftUp(m),
               matrix(c(4:12, rep(NA, 3)), nrow=4, ncol=3, byrow=TRUE))

  m <- array(1:24, dim=c(4, 3, 2))
  expect_equal(shiftUp(m),
               array(c(2, 3, 4, NA,
                       6, 7, 8, NA,
                       10, 11, 12, NA,
                       14, 15, 16, NA,
                       18, 19, 20, NA,
                       22, 23, 24, NA), dim=c(4, 3, 2)))
})


test_that("shift left", {
  m <- matrix(1:12, nrow=4, ncol=3, byrow=TRUE)

  expect_equal(shiftLeft(m, TRUE),
               matrix(c(2:3, NA, 4:6, 8:9, NA, 10:12), nrow=4, ncol=3, byrow=TRUE))
  expect_equal(shiftLeft(m, FALSE),
               matrix(c(1:3, 5:6, NA, 7:9, 11:12, NA), nrow=4, ncol=3, byrow=TRUE))

  expect_equal(shiftLeft(m[-4, ], TRUE),
               matrix(c(2:3, NA, 4:6, 8:9, NA), nrow=3, ncol=3, byrow=TRUE))
  expect_equal(shiftLeft(m[-4, ], FALSE),
               matrix(c(1:3, 5:6, NA, 7:9), nrow=3, ncol=3, byrow=TRUE))

  a <- array(0, dim=c(4, 3, 2))
  a[, , 1] <- m
  a[, , 2] <- m
  expect_equal(shiftLeft(a, TRUE)[, , 1],
               matrix(c(2:3, NA, 4:6, 8:9, NA, 10:12), nrow=4, ncol=3, byrow=TRUE))
  expect_equal(shiftLeft(a, TRUE)[, , 2],
               matrix(c(2:3, NA, 4:6, 8:9, NA, 10:12), nrow=4, ncol=3, byrow=TRUE))
  expect_equal(shiftLeft(a, FALSE)[, , 1],
               matrix(c(1:3, 5:6, NA, 7:9, 11:12, NA), nrow=4, ncol=3, byrow=TRUE))
  expect_equal(shiftLeft(a, FALSE)[, , 2],
               matrix(c(1:3, 5:6, NA, 7:9, 11:12, NA), nrow=4, ncol=3, byrow=TRUE))
})


test_that("shift right", {
  m <- matrix(1:12, nrow=4, ncol=3, byrow=TRUE)

  expect_equal(shiftRight(m, TRUE),
               matrix(c(NA, 1:2, 4:6, NA, 7:8, 10:12), nrow=4, ncol=3, byrow=TRUE))
  expect_equal(shiftRight(m, FALSE),
               matrix(c(1:3, NA, 4:5, 7:9, NA, 10:11), nrow=4, ncol=3, byrow=TRUE))

  expect_equal(shiftRight(m[-4, ], TRUE),
               matrix(c(NA, 1:2, 4:6, NA, 7:8), nrow=3, ncol=3, byrow=TRUE))
  expect_equal(shiftRight(m[-4, ], FALSE),
               matrix(c(1:3, NA, 4:5, 7:9), nrow=3, ncol=3, byrow=TRUE))

  a <- array(0, dim=c(4, 3, 2))
  a[, , 1] <- m
  a[, , 2] <- m
  expect_equal(shiftRight(a, TRUE)[, , 1],
               matrix(c(NA, 1:2, 4:6, NA, 7:8, 10:12), nrow=4, ncol=3, byrow=TRUE))
  expect_equal(shiftRight(a, TRUE)[, , 2],
               matrix(c(NA, 1:2, 4:6, NA, 7:8, 10:12), nrow=4, ncol=3, byrow=TRUE))
  expect_equal(shiftRight(a, FALSE)[, , 1],
               matrix(c(1:3, NA, 4:5, 7:9, NA, 10:11), nrow=4, ncol=3, byrow=TRUE))
  expect_equal(shiftRight(a, FALSE)[, , 2],
               matrix(c(1:3, NA, 4:5, 7:9, NA, 10:11), nrow=4, ncol=3, byrow=TRUE))
})
