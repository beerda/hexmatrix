test_that("listSum", {
  m1 <- matrix(c(1:11, NA), nrow=3)
  m2 <- matrix(2:13, nrow=3)
  m3 <- matrix(c(NA, 4:14), nrow=3)

  expect_equal(listSum(list(m1, m2, m3)),
               m1 + m2 + m3)

  n1 <- matrix(c(1:11, 0), nrow=3)
  n2 <- matrix(2:13, nrow=3)
  n3 <- matrix(c(0, 4:14), nrow=3)

  expect_equal(listSum(list(m1, m2, m3), na.rm=TRUE),
               n1 + n2 + n3)

  expect_equal(listSum(list(NA_real_, 1), na.rm=TRUE), 1)
  expect_equal(listSum(list(NA_real_, 1), na.rm=FALSE), NA_real_)
  expect_equal(listSum(list(NA_real_, NA_real_), na.rm=TRUE), NA_real_)
  expect_equal(listSum(list(NA_real_, NA_real_), na.rm=FALSE), NA_real_)
})
