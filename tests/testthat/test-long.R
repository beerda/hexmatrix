test_that("long(hexmatrix)", {
  m1 <- matrix(1:12, nrow=4, byrow=FALSE)
  m2 <- matrix(2 * 1:12, nrow=4, byrow=FALSE)

  res <- long(m1)
  expect_true(is.data.frame(res))
  expect_equal(colnames(res), c('row', 'col', 'm1'))
  expect_equal(res[, 1], rep(1:4, times=3))
  expect_equal(res[, 2], rep(1:3, each=4))
  expect_equal(res[, 3], 1:12)

  res <- long(m1, m2)
  expect_true(is.data.frame(res))
  expect_equal(colnames(res), c('row', 'col', 'm1', 'm2'))
  expect_equal(res[, 1], rep(1:4, times=3))
  expect_equal(res[, 2], rep(1:3, each=4))
  expect_equal(res[, 3], 1:12)
  expect_equal(res[, 4], 2 * 1:12)
})


test_that("long(hexmatrix) naming of columns", {
  m1 <- matrix(1:12, nrow=4, byrow=FALSE)
  m2 <- matrix(2 * 1:12, nrow=4, byrow=FALSE)

  res <- long(m1, m2)
  expect_equal(colnames(res), c('row', 'col', 'm1', 'm2'))

  res <- long(a=m1, b=m2)
  expect_equal(colnames(res), c('row', 'col', 'a', 'b'))

  res <- long(a=m1, m2)
  expect_equal(colnames(res), c('row', 'col', 'a', 'm2'))
})


test_that("long(hexmatrix) for plot", {
  m1 <- matrix(1:12, nrow=4, byrow=FALSE)
  m2 <- matrix(2 * 1:12, nrow=4, byrow=FALSE)

  res <- long(m1, .forPlot=TRUE)
  expect_true(is.data.frame(res))
  expect_equal(colnames(res), c('y', 'x', 'm1'))
  expect_equal(res[, 1], rep(1:4, times=3))
  expect_equal(res[, 2], c(1.5, 1, 1.5, 1, 2.5, 2, 2.5, 2, 3.5, 3, 3.5, 3))
  expect_equal(res[, 3], 1:12)

  res <- long(m1, m2, .forPlot=TRUE)
  expect_true(is.data.frame(res))
  expect_equal(colnames(res), c('y', 'x', 'm1', 'm2'))
  expect_equal(res[, 1], rep(1:4, times=3))
  expect_equal(res[, 2], c(1.5, 1, 1.5, 1, 2.5, 2, 2.5, 2, 3.5, 3, 3.5, 3))
  expect_equal(res[, 3], 1:12)
  expect_equal(res[, 4], 2 * 1:12)
})


test_that("long(hexmatrix) naming of columns for plot", {
  m1 <- matrix(1:12, nrow=4, byrow=FALSE)
  m2 <- matrix(2 * 1:12, nrow=4, byrow=FALSE)

  res <- long(m1, m2, .forPlot=TRUE)
  expect_equal(colnames(res), c('y', 'x', 'm1', 'm2'))

  res <- long(a=m1, b=m2, .forPlot=TRUE)
  expect_equal(colnames(res), c('y', 'x', 'a', 'b'))

  res <- long(a=m1, m2, .forPlot=TRUE)
  expect_equal(colnames(res), c('y', 'x', 'a', 'm2'))
})


test_that("NA in long(hexmatrix) call", {
  m1 <- matrix(1:12, nrow=4, byrow=FALSE)
  m1[2, 2] <- NA

  res <- long(m1)
  expect_true(is.data.frame(res))
  expect_equal(colnames(res), c('row', 'col', 'm1'))
  expect_equal(res[, 1], rep(1:4, times=3))
  expect_equal(res[, 2], rep(1:3, each=4))
  expect_equal(res[, 3], c(1:5, NA_real_, 7:12))
})


test_that("long(hexarray)", {
  m1 <- array(1:24, dim=c(4, 3, 2))
  m2 <- array(2 * 1:24, dim=c(4, 3, 2))

  res <- long(m1)
  expect_true(is.data.frame(res))
  expect_equal(colnames(res), c('row', 'col', 'layer', 'm1'))
  expect_equal(res[, 1], rep(rep(1:4, times=3), times=2))
  expect_equal(res[, 2], rep(rep(1:3, each=4), times=2))
  expect_equal(res[, 3], rep(1:2, each=12))
  expect_equal(res[, 4], 1:24)

  res <- long(m1, m2)
  expect_true(is.data.frame(res))
  expect_equal(colnames(res), c('row', 'col', 'layer', 'm1', 'm2'))
  expect_equal(res[, 1], rep(rep(1:4, times=3), times=2))
  expect_equal(res[, 2], rep(rep(1:3, each=4), times=2))
  expect_equal(res[, 3], rep(1:2, each=12))
  expect_equal(res[, 4], 1:24)
  expect_equal(res[, 5], 2 * 1:24)
})


test_that("long errors", {
  m1 <- matrix(1:12, nrow=4, byrow=FALSE)
  m2 <- matrix(2 * 1:12, nrow=3, byrow=FALSE)
  a <- array(1:12, dim=c(4,3,1))

  expect_error(long(m1, m2))
  expect_error(long(m1, a))
})
