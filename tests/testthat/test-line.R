test_that("line", {
  m <- matrix(1:100, nrow=10, ncol=10)

  expect_equal(line(m, 3, 3), 3)
  expect_equal(line(m, 1, 51), c(1, 11, 21, 31, 41, 51))
  expect_equal(line(m, 51, 1), c(1, 11, 21, 31, 41, 51))
  expect_equal(line(m, 1, 60), c(1, 12, 13, 24, 25, 36, 37, 48, 49, 60))
  expect_equal(line(m, 60, 1), c(1, 12, 13, 24, 25, 36, 37, 48, 49, 60))
  expect_equal(line(m, 1, 93), c(1, 11, 21, 32, 42, 52, 62, 72, 73, 83, 93))
  expect_equal(line(m, 93, 1), c(1, 11, 21, 32, 42, 52, 62, 72, 73, 83, 93))
  expect_equal(line(m, 10, 41), c(10, 9, 18, 17, 26, 25, 34, 33, 42, 41))
  expect_equal(line(m, 41, 10), c(10, 9, 18, 17, 26, 25, 34, 33, 42, 41))
  expect_equal(line(m, 1, 10), 1:10)
  expect_equal(line(m, 10, 1), 1:10)
  expect_equal(line(m, 1, 9), 1:9)
  expect_equal(line(m, 9, 1), 1:9)
  expect_equal(line(m, 2, 9), 2:9)
  expect_equal(line(m, 9, 2), 2:9)
  expect_equal(line(m, 2, 10), 2:10)
  expect_equal(line(m, 10, 2), 2:10)
})

test_that("line<-", {
  m <- matrix(1:100, nrow=10, ncol=10)
  m2 <- matrix(1:100, nrow=10, ncol=10)

  line(m, 1, 51) <- 0

  indices <- c(1, 11, 21, 31, 41, 51)
  expect_equal(m[indices], rep(0, length(indices)))
  expect_equal(m[-indices], m2[-indices])
})

test_that("line (point)", {
  m <- matrix(1:100, nrow=10, ncol=10)
  m2 <- matrix(1:100, nrow=10, ncol=10)

  expect_equal(line(m, c(1, 1), c(3, 10)),
               c(1, 11, 21, 32, 42, 52, 62, 72, 73, 83, 93))

  line(m, c(1, 1), c(1, 6)) <- 0
  indices <- c(1, 11, 21, 31, 41, 51)
  expect_equal(m[indices], rep(0, length(indices)))
  expect_equal(m[-indices], m2[-indices])
})
