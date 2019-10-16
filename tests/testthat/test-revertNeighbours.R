test_that("revertNeighbours", {
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


test_that("revertNeighbours with self", {
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
