test_that("pointToIndex & indexToPoint", {
  m <- matrix(1:12, nrow=4)
  for (i in seq_len(nrow(m))) {
    for (j in seq_len(ncol(m))) {
      expect_equal(pointToIndex(c(i, j), m), m[i, j])
      expect_equal(indexToPoint(m[i, j], m), c(i, j))
    }
  }
})
