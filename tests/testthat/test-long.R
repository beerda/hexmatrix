test_that("long", {
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


test_that("long naming of columns", {
  m1 <- matrix(1:12, nrow=4, byrow=FALSE)
  m2 <- matrix(2 * 1:12, nrow=4, byrow=FALSE)

  res <- long(m1, m2)
  expect_equal(colnames(res), c('row', 'col', 'm1', 'm2'))

  res <- long(a=m1, b=m2)
  expect_equal(colnames(res), c('row', 'col', 'a', 'b'))

  res <- long(a=m1, m2)
  expect_equal(colnames(res), c('row', 'col', 'a', 'm2'))
})


test_that("long for plot", {
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


test_that("long naming of columns for plot", {
  m1 <- matrix(1:12, nrow=4, byrow=FALSE)
  m2 <- matrix(2 * 1:12, nrow=4, byrow=FALSE)

  res <- long(m1, m2, .forPlot=TRUE)
  expect_equal(colnames(res), c('y', 'x', 'm1', 'm2'))

  res <- long(a=m1, b=m2, .forPlot=TRUE)
  expect_equal(colnames(res), c('y', 'x', 'a', 'b'))

  res <- long(a=m1, m2, .forPlot=TRUE)
  expect_equal(colnames(res), c('y', 'x', 'a', 'm2'))
})


test_that("NA in long call", {
  m1 <- matrix(1:12, nrow=4, byrow=FALSE)
  m1[2, 2] <- NA

  res <- long(m1)
  expect_true(is.data.frame(res))
  expect_equal(colnames(res), c('row', 'col', 'm1'))
  expect_equal(res[, 1], rep(1:4, times=3))
  expect_equal(res[, 2], rep(1:3, each=4))
  expect_equal(res[, 3], c(1:5, NA_real_, 7:12))
})
