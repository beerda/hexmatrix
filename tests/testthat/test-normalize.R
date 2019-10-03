test_that("normalize", {
  a <- array(1:24, dim=c(2,3,4))

  summ <- a[, , 1]  + a[, , 2] + a[, , 3] + a[, , 4]
  expected <- a
  expected[, , 1] <- expected[, , 1] / summ
  expected[, , 2] <- expected[, , 2] / summ
  expected[, , 3] <- expected[, , 3] / summ
  expected[, , 4] <- expected[, , 4] / summ
  expect_equal(normalize(a, c(1,2)), expected)

  summ <- a[1, , ]  + a[2, , ]
  expected <- a
  expected[1, , ] <- expected[1, , ] / summ
  expected[2, , ] <- expected[2, , ] / summ
  expect_equal(normalize(a, c(2, 3)), expected)
})
