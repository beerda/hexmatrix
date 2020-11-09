test_that("layernames", {
  a <- array(1:24, dim=c(4, 3, 2))
  rownames(a) <- LETTERS[1:4]
  colnames(a) <- letters[1:3]
  expect_true(is.null(layernames(a)))
  layernames(a) <- c('layer1', 'layer2')
  expect_equal(layernames(a), c('layer1', 'layer2'))
  layernames(a) <- NULL
  expect_true(is.null(layernames(a)))
  expect_equal(rownames(a), LETTERS[1:4])
  expect_equal(colnames(a), letters[1:3])
})
