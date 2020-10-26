test_that("point(matrix)", {
  m <- matrix(1:12, nrow=4)
  for (i in seq_len(nrow(m))) {
    for (j in seq_len(ncol(m))) {
      expect_equal(point(m, m[i, j]),  c(i, j))
    }
  }
})


test_that("point(array)", {
  a <- array(1:24, dim=c(4, 3, 2))
  for (i in seq_len(nrow(a))) {
    for (j in seq_len(ncol(a))) {
      for (k in seq_len(nlayer(a))) {
        expect_equal(point(a, a[i, j, k]),  c(i, j, k))
      }
    }
  }
})
