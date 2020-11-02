test_that("revertNeighbours(hexmatrix)", {
  m <- matrix(1:12, nrow=4, byrow=TRUE)
  n <- neighbours(m)

  res <- revertNeighbours(n)
  expect_equal(res[, , 1],
               matrix(c(NA, NA, NA,
                        NA, 5, 6,
                        7, 8, 9,
                        NA, 11, 12), nrow=4, byrow=TRUE))
  expect_equal(res[, , 2],
               matrix(c(NA, NA, NA,
                        4, 5, 6,
                        7, 8, NA,
                        10, 11, 12), nrow=4, byrow=TRUE))
  expect_equal(res[, , 3],
               matrix(c(1, 2, NA,
                        4, 5, NA,
                        7, 8, NA,
                        10, 11, NA), nrow=4, byrow=TRUE))
  expect_equal(res[, , 4],
               matrix(c(1, 2, NA,
                        4, 5, 6,
                        7, 8, NA,
                        NA, NA, NA), nrow=4, byrow=TRUE))
  expect_equal(res[, , 5],
               matrix(c(1, 2, 3,
                        NA, 5, 6,
                        7, 8, 9,
                        NA, NA, NA), nrow=4, byrow=TRUE))
  expect_equal(res[, , 6],
               matrix(c(NA, 2, 3,
                        NA, 5, 6,
                        NA, 8, 9,
                        NA, 11, 12), nrow=4, byrow=TRUE))
})


test_that("revertNeighbours(hexmatrix) with self", {
  m <- matrix(1:12, nrow=4, byrow=TRUE)
  n <- neighbours(m, TRUE)

  res <- revertNeighbours(n)
  expect_equal(res[, , 1],
               matrix(c(NA, NA, NA,
                        NA, 5, 6,
                        7, 8, 9,
                        NA, 11, 12), nrow=4, byrow=TRUE))
  expect_equal(res[, , 2],
               matrix(c(NA, NA, NA,
                        4, 5, 6,
                        7, 8, NA,
                        10, 11, 12), nrow=4, byrow=TRUE))
  expect_equal(res[, , 3],
               matrix(c(1, 2, NA,
                        4, 5, NA,
                        7, 8, NA,
                        10, 11, NA), nrow=4, byrow=TRUE))
  expect_equal(res[, , 4],
               matrix(c(1, 2, NA,
                        4, 5, 6,
                        7, 8, NA,
                        NA, NA, NA), nrow=4, byrow=TRUE))
  expect_equal(res[, , 5],
               matrix(c(1, 2, 3,
                        NA, 5, 6,
                        7, 8, 9,
                        NA, NA, NA), nrow=4, byrow=TRUE))
  expect_equal(res[, , 6],
               matrix(c(NA, 2, 3,
                        NA, 5, 6,
                        NA, 8, 9,
                        NA, 11, 12), nrow=4, byrow=TRUE))
  expect_equal(res[, , 7], m)
})


test_that("revertNeighbours(hexarray)", {
  m <- matrix(1:12, nrow=4, byrow=TRUE)
  a <- array(0, dim=c(4, 3, 2))
  a[, , 1] <- m
  a[, , 2] <- 12 + m
  n <- neighbours(a)

  res <- revertNeighbours(n)
  expect_equal(res[, , 1, 1],
               matrix(c(NA, NA, NA,
                        NA, 5, 6,
                        7, 8, 9,
                        NA, 11, 12), nrow=4, byrow=TRUE))
  expect_equal(res[, , 1, 2],
               matrix(c(NA, NA, NA,
                        4, 5, 6,
                        7, 8, NA,
                        10, 11, 12), nrow=4, byrow=TRUE))
  expect_equal(res[, , 1, 3],
               matrix(c(1, 2, NA,
                        4, 5, NA,
                        7, 8, NA,
                        10, 11, NA), nrow=4, byrow=TRUE))
  expect_equal(res[, , 1, 4],
               matrix(c(1, 2, NA,
                        4, 5, 6,
                        7, 8, NA,
                        NA, NA, NA), nrow=4, byrow=TRUE))
  expect_equal(res[, , 1, 5],
               matrix(c(1, 2, 3,
                        NA, 5, 6,
                        7, 8, 9,
                        NA, NA, NA), nrow=4, byrow=TRUE))
  expect_equal(res[, , 1, 6],
               matrix(c(NA, 2, 3,
                        NA, 5, 6,
                        NA, 8, 9,
                        NA, 11, 12), nrow=4, byrow=TRUE))

  expect_equal(res[, , 2, 1],
               matrix(12 + c(NA, NA, NA,
                        NA, 5, 6,
                        7, 8, 9,
                        NA, 11, 12), nrow=4, byrow=TRUE))
  expect_equal(res[, , 2, 2],
               matrix(12 + c(NA, NA, NA,
                        4, 5, 6,
                        7, 8, NA,
                        10, 11, 12), nrow=4, byrow=TRUE))
  expect_equal(res[, , 2, 3],
               matrix(12 + c(1, 2, NA,
                        4, 5, NA,
                        7, 8, NA,
                        10, 11, NA), nrow=4, byrow=TRUE))
  expect_equal(res[, , 2, 4],
               matrix(12 + c(1, 2, NA,
                        4, 5, 6,
                        7, 8, NA,
                        NA, NA, NA), nrow=4, byrow=TRUE))
  expect_equal(res[, , 2, 5],
               matrix(12 + c(1, 2, 3,
                        NA, 5, 6,
                        7, 8, 9,
                        NA, NA, NA), nrow=4, byrow=TRUE))
  expect_equal(res[, , 2, 6],
               matrix(12 + c(NA, 2, 3,
                        NA, 5, 6,
                        NA, 8, 9,
                        NA, 11, 12), nrow=4, byrow=TRUE))
})


test_that("revertNeighbours(hexarray) with self", {
  m <- matrix(1:12, nrow=4, byrow=TRUE)
  a <- array(0, dim=c(4, 3, 2))
  a[, , 1] <- m
  a[, , 2] <- 12 + m
  n <- neighbours(a, TRUE)

  res <- revertNeighbours(n)
  expect_equal(res[, , 1, 1],
               matrix(c(NA, NA, NA,
                        NA, 5, 6,
                        7, 8, 9,
                        NA, 11, 12), nrow=4, byrow=TRUE))
  expect_equal(res[, , 1, 2],
               matrix(c(NA, NA, NA,
                        4, 5, 6,
                        7, 8, NA,
                        10, 11, 12), nrow=4, byrow=TRUE))
  expect_equal(res[, , 1, 3],
               matrix(c(1, 2, NA,
                        4, 5, NA,
                        7, 8, NA,
                        10, 11, NA), nrow=4, byrow=TRUE))
  expect_equal(res[, , 1, 4],
               matrix(c(1, 2, NA,
                        4, 5, 6,
                        7, 8, NA,
                        NA, NA, NA), nrow=4, byrow=TRUE))
  expect_equal(res[, , 1, 5],
               matrix(c(1, 2, 3,
                        NA, 5, 6,
                        7, 8, 9,
                        NA, NA, NA), nrow=4, byrow=TRUE))
  expect_equal(res[, , 1, 6],
               matrix(c(NA, 2, 3,
                        NA, 5, 6,
                        NA, 8, 9,
                        NA, 11, 12), nrow=4, byrow=TRUE))
  expect_equal(res[, , 1, 7], m)

  res <- revertNeighbours(n)
  expect_equal(res[, , 2, 1],
               matrix(12 + c(NA, NA, NA,
                        NA, 5, 6,
                        7, 8, 9,
                        NA, 11, 12), nrow=4, byrow=TRUE))
  expect_equal(res[, , 2, 2],
               matrix(12 + c(NA, NA, NA,
                        4, 5, 6,
                        7, 8, NA,
                        10, 11, 12), nrow=4, byrow=TRUE))
  expect_equal(res[, , 2, 3],
               matrix(12 + c(1, 2, NA,
                        4, 5, NA,
                        7, 8, NA,
                        10, 11, NA), nrow=4, byrow=TRUE))
  expect_equal(res[, , 2, 4],
               matrix(12 + c(1, 2, NA,
                        4, 5, 6,
                        7, 8, NA,
                        NA, NA, NA), nrow=4, byrow=TRUE))
  expect_equal(res[, , 2, 5],
               matrix(12 + c(1, 2, 3,
                        NA, 5, 6,
                        7, 8, 9,
                        NA, NA, NA), nrow=4, byrow=TRUE))
  expect_equal(res[, , 2, 6],
               matrix(12 + c(NA, 2, 3,
                        NA, 5, 6,
                        NA, 8, 9,
                        NA, 11, 12), nrow=4, byrow=TRUE))
  expect_equal(res[, , 2, 7], 12 + m)
})
