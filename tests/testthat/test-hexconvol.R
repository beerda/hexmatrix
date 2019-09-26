test_that("hexconvol", {
  m <- matrix(1:12, nrow=4, byrow=TRUE)

  expect_equal(hexconvol(m, 1),
               matrix(c(12, 17, 45, 28, 17, 33, 58, 48, 11, 33, 35, 40), nrow=4))
})
