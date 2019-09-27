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
