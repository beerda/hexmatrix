test_that("like matrix", {
  m <- matrix(1:12, nrow=4, byrow=FALSE)
  colnames(m) <- letters[1:3]
  rownames(m) <- LETTERS[1:4]

  expect_equal(like(m, data=NA),
               matrix(NA, nrow=4, ncol=3, dimnames=list(LETTERS[1:4], letters[1:3])))

  expect_equal(like(m, data=NA, nrow=3),
               matrix(NA, nrow=3, ncol=3, dimnames=list(LETTERS[1:3], letters[1:3])))

  expect_equal(like(m, nrow=3),
               m[1:3, ])

  expect_equal(like(m, ncol=2),
               m[, 1:2])


  m <- matrix(1:12, nrow=4, byrow=FALSE)
  rownames(m) <- LETTERS[1:4]

  expect_equal(like(m, data=NA),
               matrix(NA, nrow=4, ncol=3, dimnames=list(LETTERS[1:4], NULL)))

  expect_equal(like(m, data=NA, nrow=3),
               matrix(NA, nrow=3, ncol=3, dimnames=list(LETTERS[1:3], NULL)))
})


test_that("like array", {
  a <- array(1:24,
             dim=c(3,4,2),
             dimnames=list(letters[1:3], LETTERS[1:4], 1:2))

  expect_equal(like(a, data=NA),
               array(NA,
                     dim=c(3,4,2),
                     dimnames=list(letters[1:3], LETTERS[1:4], 1:2)))

  expect_equal(like(a, data=NA, dim=c(2,2,2)),
               array(NA,
                     dim=c(2,2,2),
                     dimnames=list(letters[1:2], LETTERS[1:2], 1:2)))

  expect_equal(like(a, dim=c(2,2,2)),
               a[1:2, 1:2, 1:2])
})
