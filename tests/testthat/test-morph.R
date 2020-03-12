test_that("morph (dilation)", {
  m <- matrix(FALSE, nrow=4, ncol=5)
  m[2, 2] <- TRUE
  m[4, 5] <- TRUE

  expected <- matrix(FALSE, nrow=4, ncol=5)
  expected[c(1, 2, 3, 5, 6, 7, 10, 15, 16, 19, 20)] <- TRUE
  expect_equal(morph(m, dilate=TRUE), expected)

  expected <- matrix(FALSE, nrow=4, ncol=5)
  expect_equal(morph(m, dilate=FALSE), expected)
})
