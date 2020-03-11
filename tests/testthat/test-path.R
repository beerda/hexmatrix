test_that("path", {
  paths <- matrix(1:12 - 1, ncol=3, byrow=FALSE)
  expect_equal(path(3, paths), list(3:1))
})
