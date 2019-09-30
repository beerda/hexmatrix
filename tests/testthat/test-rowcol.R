test_that("rowcol", {
  m <- matrix(1:12, nrow=4, ncol=3, byrow=FALSE)
  expect_equal(rowcol(m), long(m))
})


test_that("rowcol<-", {
  m <- matrix(1:12, nrow=4, ncol=3, byrow=FALSE)
  rowcol(m, c(1,1,3,4), c(1,3,2,2)) <- NA
  expect_equal(m,
               matrix(c(NA,2,3,4,5,6,NA,NA,NA,10,11,12), nrow=4, byrow=FALSE))
})
