test_that("as.hexarray", {
  m1 <- matrix(1:12, nrow=4, ncol=3)
  m2 <- matrix(101:112, nrow=4, ncol=3)
  a <- as.hexarray(list(terrain=m1, bridges=m2))
  expect_true(is.hexarray(a))
  expect_equal(nrow(a), 4)
  expect_equal(ncol(a), 3)
  expect_equal(nlayer(a), 2)
  expect_equal(layernames(a), c('terrain', 'bridges'))
})


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
