test_that("index(matrix)", {
  m <- matrix(1:12, nrow=4)
  for (i in seq_len(nrow(m))) {
    for (j in seq_len(ncol(m))) {
      expect_equal(index(m, c(i, j)), m[i, j])
    }
  }
})


test_that("index(array)", {
  a <- array(1:24, dim=c(4, 3, 2))
  for (i in seq_len(nrow(a))) {
    for (j in seq_len(ncol(a))) {
      for (k in seq_len(nlayer(a))) {
        expect_equal(index(a, c(i, j, k)), a[i, j, k])
      }
    }
  }
})
