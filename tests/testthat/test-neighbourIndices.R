test_that("neighbourIndices", {
  res <- neighbourIndices(c(2, 2))
  expect_equal(dim(res), c(1, 6, 2))
  expect_equal(res[1, , ],
               matrix(c(1, 1, 1, 2, 2, 3, 3, 2, 3, 1, 2, 1), byrow=TRUE, ncol=2))


  res <- neighbourIndices(matrix(c(2, 2,
                                   3, 2,
                                   2, 3), byrow=TRUE, ncol=2))
  expect_equal(dim(res), c(3, 6, 2))
  expect_equal(res[1, , ],
               matrix(c(1, 1, 1, 2, 2, 3, 3, 2, 3, 1, 2, 1), byrow=TRUE, ncol=2))
  expect_equal(res[2, , ],
               matrix(c(2, 2, 2, 3, 3, 3, 4, 3, 4, 2, 3, 1), byrow=TRUE, ncol=2))
  expect_equal(res[3, , ],
               matrix(c(1, 2, 1, 3, 2, 4, 3, 3, 3, 2, 2, 2), byrow=TRUE, ncol=2))
})
