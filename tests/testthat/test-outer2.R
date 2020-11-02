test_that("outer2", {
  res <- outer2(prod, 1:4)
  expect_equal(res, 1:4)

  res <- outer2(prod, 1:4, 1:3)
  expect_equal(res, 1:4 %o% 1:3)

  res <- outer2(prod, 1:4, 1:3, 1:2)
  expect_equal(res, (1:4 %o% 1:3) %o% 1:2)

  m <- matrix(1:12, nrow=4)
  res <- outer2(prod, m)
  expect_equal(res, m)

  res <- outer2(prod, m, 1:3, 1:2)
  expect_equal(res, (m %o% 1:3) %o% 1:2)

  m <- matrix(1:12, nrow=4)
  res <- outer2(prod, m, m, m)
  expect_equal(res, (m %o% m) %o% m)

  a <- array(seq_len(2*3*4), dim=c(2,3,4))
  res <- outer2(prod, a)
  expect_equal(res, a)

  res <- outer2(prod, a, m, 1:3)
  expect_equal(res, (a %o% m) %o% 1:3)
})

