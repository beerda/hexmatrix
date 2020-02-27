test_that("neighbourIndices", {
  m <- matrix(0, nrow=6, ncol=6)
  m2 <- matrix(0, nrow=2, ncol=6)

  res <- neighbourIndices(c(2, 2), m)
  expect_equal(dim(res), c(1, 6, 2))
  expect_equal(res[1, , ],
               matrix(c(1, 1, 1, 2, 2, 3, 3, 2, 3, 1, 2, 1), byrow=TRUE, ncol=2))

  res <- neighbourIndices(c(2, 2), m2)
  expect_equal(dim(res), c(1, 6, 2))
  expect_equal(res[1, , ],
               matrix(c(1, 1, 1, 2, 2, 3, NA, NA, NA, NA, 2, 1), byrow=TRUE, ncol=2))


  res <- neighbourIndices(matrix(c(2, 2,
                                   3, 2,
                                   2, 3,
                                   1, 1), byrow=TRUE, ncol=2),
                          m)
  expect_equal(dim(res), c(4, 6, 2))
  expect_equal(res[1, , ],
               matrix(c(1, 1, 1, 2, 2, 3, 3, 2, 3, 1, 2, 1), byrow=TRUE, ncol=2))
  expect_equal(res[2, , ],
               matrix(c(2, 2, 2, 3, 3, 3, 4, 3, 4, 2, 3, 1), byrow=TRUE, ncol=2))
  expect_equal(res[3, , ],
               matrix(c(1, 2, 1, 3, 2, 4, 3, 3, 3, 2, 2, 2), byrow=TRUE, ncol=2))
  expect_equal(res[4, , ],
               matrix(c(NA, NA, NA, NA, 1, 2, 2, 2, 2, 1, NA, NA), byrow=TRUE, ncol=2))

  res <- neighbourIndices(matrix(c(2, 2,
                                   3, 2,
                                   2, 3,
                                   1, 1), byrow=TRUE, ncol=2),
                          m2)
  expect_equal(dim(res), c(4, 6, 2))
  expect_equal(res[1, , ],
               matrix(c(1, 1, 1, 2, 2, 3, NA, NA, NA, NA, 2, 1), byrow=TRUE, ncol=2))
  expect_equal(res[2, , ],
               matrix(c(2, 2, 2, 3, NA, NA, NA, NA, NA, NA, NA, NA), byrow=TRUE, ncol=2))
  expect_equal(res[3, , ],
               matrix(c(1, 2, 1, 3, 2, 4, NA, NA, NA, NA, 2, 2), byrow=TRUE, ncol=2))
  expect_equal(res[4, , ],
               matrix(c(NA, NA, NA, NA, 1, 2, 2, 2, 2, 1, NA, NA), byrow=TRUE, ncol=2))
})
