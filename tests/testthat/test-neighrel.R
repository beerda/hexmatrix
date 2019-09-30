test_that("neighrel", {
  m <- matrix(letters[1:3], nrow=4, ncol=3, byrow=FALSE)
  transl <- matrix(1:9, nrow=3, dimnames=list(letters[1:3], letters[1:3]))

  f <- function(u1, u2) {
    ifelse(any(is.na(c(u1, u2))),
           NA,
           transl[u1, u2])
  }

  res <- neighrel(f, m)
  expect_equal(res[, , 1], matrix(c(NA, NA, NA,
                                    NA, 7, 2,
                                    8, 3, 4,
                                    NA, 6, 7), nrow=4, ncol=3, byrow=TRUE))
  expect_equal(res[, , 2], matrix(c(NA, NA, NA,
                                    4, 8, 3,
                                    9, 1, NA,
                                    3, 4, 8), nrow=4, ncol=3, byrow=TRUE))
  expect_equal(res[, , 3], matrix(c(2, 6, NA,
                                    6, 7, NA,
                                    7, 2, NA,
                                    2, 6, NA), nrow=4, ncol=3, byrow=TRUE))
  expect_equal(res[, , 4], matrix(c(3, 4, NA,
                                    6, 7, 2,
                                    8, 3, NA,
                                    NA, NA, NA), nrow=4, ncol=3, byrow=TRUE))
  expect_equal(res[, , 5], matrix(c(2, 6, 7,
                                    NA, 9, 1,
                                    7, 2, 6,
                                    NA, NA, NA), nrow=4, ncol=3, byrow=TRUE))
  expect_equal(res[, , 6], matrix(c(NA, 4, 8,
                                    NA, 8, 3,
                                    NA, 3, 4,
                                    NA, 4, 8), nrow=4, ncol=3, byrow=TRUE))


  f <- function(u1, u2, v1, v2) {
    ifelse(any(is.na(c(u1, u2, v1, v2))),
           NA,
           0 + ((u1 == v1) & (u2 == v2)))
  }

  res <- neighrel(f, m, m)
  expect_equal(res[, , 1], matrix(c(NA, NA, NA,
                                    NA, 1, 1,
                                    1, 1, 1,
                                    NA, 1, 1), nrow=4, ncol=3, byrow=TRUE))
  expect_equal(res[, , 2], matrix(c(NA, NA, NA,
                                    1, 1, 1,
                                    1, 1, NA,
                                    1, 1, 1), nrow=4, ncol=3, byrow=TRUE))
  expect_equal(res[, , 3], matrix(c(1, 1, NA,
                                    1, 1, NA,
                                    1, 1, NA,
                                    1, 1, NA), nrow=4, ncol=3, byrow=TRUE))
  expect_equal(res[, , 4], matrix(c(1, 1, NA,
                                    1, 1, 1,
                                    1, 1, NA,
                                    NA, NA, NA), nrow=4, ncol=3, byrow=TRUE))
  expect_equal(res[, , 5], matrix(c(1, 1, 1,
                                    NA, 1, 1,
                                    1, 1, 1,
                                    NA, NA, NA), nrow=4, ncol=3, byrow=TRUE))
  expect_equal(res[, , 6], matrix(c(NA, 1, 1,
                                    NA, 1, 1,
                                    NA, 1, 1,
                                    NA, 1, 1), nrow=4, ncol=3, byrow=TRUE))

})
