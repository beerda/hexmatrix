test_that("rowcol matrix", {
  m <- matrix(1:12, nrow=4, ncol=3, byrow=FALSE)
  expect_equal(rowcol(m), long(m))

  m <- matrix(c(1:11, NA), nrow=4, ncol=3, byrow=FALSE)
  expect_equal(rowcol(m), long(m))
})


test_that("rowcol<- matrix NA", {
  m <- matrix(1:12, nrow=4, ncol=3, byrow=FALSE)
  rowcol(m, c(1,1,3,4), c(1,3,2,2)) <- NA
  expect_equal(m,
               matrix(c(NA,2,3,4,5,6,NA,NA,NA,10,11,12), nrow=4, byrow=FALSE))
})


test_that("rowcol<- matrix", {
  m <- matrix(1:12, nrow=4, ncol=3, byrow=FALSE)
  rowcol(m, c(1,1,3,4), c(1,3,2,2)) <- c(100, 200)
  expect_equal(m,
               matrix(c(100,2,3,4,
                        5,6,100,200,
                        200,10,11,12), nrow=4, byrow=FALSE))
})


test_that("rowcol array", {
  m <- array(1:24, dim=c(4, 3, 2))
  expect_equal(rowcol(m), long(m))

  m <- matrix(c(1:23, NA), nrow=4, ncol=3, byrow=FALSE)
  expect_equal(rowcol(m), long(m))
})


test_that("rowcol<- array NA", {
  m <- array(1:24, dim=c(4, 3, 2))
  rowcol(m, c(1,1,3,4), c(1,3,2,2), 1) <- NA
  expect_equal(m,
               array(c(NA,2,3,4,5,6,NA,NA,NA,10,11,12,
                       13,14,15,16,17,18,19,20,21,22,23,24), dim=c(4,3,2)))
})


test_that("rowcol<- array", {
  m <- array(1:24, dim=c(4, 3, 2))
  rowcol(m, c(1,1,3,4), c(1,3,2,2), 1) <- c(100, 200)
  expect_equal(m,
               array(c(100,2,3,4, 5,6,100,200, 200,10,11,12,
                       13,14,15,16,17,18,19,20,21,22,23,24), dim=c(4,3,2)))
})
