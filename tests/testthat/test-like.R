test_that("like matrix", {
  m <- matrix(1:12, nrow=4, byrow=FALSE)
  colnames(m) <- letters[1:3]
  rownames(m) <- LETTERS[1:4]

  expect_equal(like(m, data=NA),
               matrix(NA, nrow=4, ncol=3, dimnames=list(LETTERS[1:4], letters[1:3])))

  expect_equal(like(m, data=NA, nrow=3),
               matrix(NA, nrow=3, ncol=3, dimnames=list(LETTERS[1:3], letters[1:3])))


  m <- matrix(1:12, nrow=4, byrow=FALSE)
  rownames(m) <- LETTERS[1:4]

  expect_equal(like(m, data=NA),
               matrix(NA, nrow=4, ncol=3, dimnames=list(LETTERS[1:4], NULL)))

  expect_equal(like(m, data=NA, nrow=3),
               matrix(NA, nrow=3, ncol=3, dimnames=list(LETTERS[1:3], NULL)))
})
