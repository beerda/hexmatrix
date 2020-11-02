test_that("morph (dilation)", {
  m <- matrix(FALSE, nrow=4, ncol=5)
  m[2, 2] <- TRUE
  m[4, 5] <- TRUE

  expected <- matrix(FALSE, nrow=4, ncol=5)
  expected[c(1, 2, 3, 5, 6, 7, 10, 15, 16, 19, 20)] <- TRUE
  expect_equal(morph(m, dilate=TRUE), expected)
})


test_that("morph (erosion)", {
  m <- matrix(FALSE, nrow=4, ncol=5)
  m[c(1, 2, 3, 5, 6, 7, 10, 15, 16, 19, 20)] <- TRUE
  expected <- matrix(FALSE, nrow=4, ncol=5)
  expected[c(1, 2, 6, 20)] <- TRUE
  expect_equal(morph(m, dilate=FALSE), expected)

  m <- matrix(FALSE, nrow=4, ncol=5)
  m[2, 2] <- TRUE
  expected <- matrix(FALSE, nrow=4, ncol=5)
  expect_equal(morph(m, dilate=FALSE), expected)
})


test_that("morph(hexarray)", {
  a <- array(FALSE, dim=c(4, 5, 2))
  a[2, 2, 1] <- TRUE
  a[4, 5, 1] <- TRUE

  expected <- matrix(FALSE, nrow=4, ncol=5)
  expected[c(1, 2, 3, 5, 6, 7, 10, 15, 16, 19, 20)] <- TRUE
  res <- morph(a, dilate=TRUE)
  expect_equal(res[, , 1], expected)
  expect_equal(res[, , 2], like(expected, data=FALSE))

  a <- array(FALSE, dim=c(4, 5, 2))
  a[2, 2, 2] <- TRUE
  a[4, 5, 2] <- TRUE
  res <- morph(a, dilate=TRUE)
  expect_equal(res[, , 2], expected)
  expect_equal(res[, , 1], like(expected, data=FALSE))
})
