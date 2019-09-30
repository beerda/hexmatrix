test_that("neighbours", {
  m <- matrix(1:12, nrow=4, ncol=3, byrow=TRUE)
  n <- neighbours(m)

  expect_true(is.array(n))
  expect_equal(dim(n), c(4, 3, 6))
  expect_equal(n[, , 1],
               matrix(c(NA, NA, NA, NA, 1, 2, 4, 5, 6, NA, 7, 8), nrow=4, ncol=3, byrow=TRUE))
  expect_equal(n[, , 2],
               matrix(c(NA, NA, NA, 1, 2, 3, 5, 6, NA, 7, 8, 9), nrow=4, ncol=3, byrow=TRUE))
  expect_equal(n[, , 3],
               matrix(c(2, 3, NA, 5, 6, NA, 8, 9, NA, 11, 12, NA), nrow=4, ncol=3, byrow=TRUE))
  expect_equal(n[, , 4],
               matrix(c(5, 6, NA, 7, 8, 9, 11, 12, NA, NA, NA, NA), nrow=4, ncol=3, byrow=TRUE))
  expect_equal(n[, , 5],
               matrix(c(4, 5, 6, NA, 7, 8, 10, 11, 12, NA, NA, NA), nrow=4, ncol=3, byrow=TRUE))
  expect_equal(n[, , 6],
               matrix(c(NA, 1, 2, NA, 4, 5, NA, 7, 8, NA, 10, 11), nrow=4, ncol=3, byrow=TRUE))
})

test_that("neighbours on matrix with odd rows", {
  m <- matrix(1:6, nrow=3, ncol=2, byrow=TRUE)
  n <- neighbours(m)

  expect_true(is.array(n))
  expect_equal(dim(n), c(3, 2, 6))
  expect_equal(n[, , 1],
               matrix(c(NA, NA, NA, 1, 3, 4), nrow=3, ncol=2, byrow=TRUE))
  expect_equal(n[, , 2],
               matrix(c(NA, NA, 1, 2, 4, NA), nrow=3, ncol=2, byrow=TRUE))
  expect_equal(n[, , 3],
               matrix(c(2, NA, 4, NA, 6, NA), nrow=3, ncol=2, byrow=TRUE))
  expect_equal(n[, , 4],
               matrix(c(4, NA, 5, 6, NA, NA), nrow=3, ncol=2, byrow=TRUE))
  expect_equal(n[, , 5],
               matrix(c(3, 4, NA, 5, NA, NA), nrow=3, ncol=2, byrow=TRUE))
  expect_equal(n[, , 6],
               matrix(c(NA, 1, NA, 3, NA, 5), nrow=3, ncol=2, byrow=TRUE))
})