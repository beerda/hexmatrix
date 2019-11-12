test_that("shrink 0x0", {
  m <- matrix(1, nrow=0, ncol=0, byrow=FALSE)
  expect_equal(shrink(m),
               matrix(0, nrow=0, ncol=0, byrow=FALSE))
})

test_that("shrink 1x1", {
  m <- matrix(1, nrow=1, byrow=FALSE)
  expect_equal(shrink(m),
               matrix(1, nrow=1, byrow=FALSE))
})

test_that("shrink 1x2", {
  m <- matrix(1:2, nrow=1, byrow=FALSE)
  expect_equal(shrink(m),
               matrix(c(2, 1), nrow=1, byrow=FALSE))
})

test_that("shrink 1x3", {
  m <- matrix(1:3, nrow=1, byrow=FALSE)
  expect_equal(shrink(m),
               matrix(c(2, 4), nrow=1, byrow=FALSE))
})

test_that("shrink 1x4", {
  m <- matrix(1:4, nrow=1, byrow=FALSE)
  expect_equal(shrink(m),
               matrix(c(2, 6, 2), nrow=1, byrow=FALSE))
})

test_that("shrink 1x5", {
  m <- matrix(1:5, nrow=1, byrow=FALSE)
  expect_equal(shrink(m),
               matrix(c(2, 6, 7), nrow=1, byrow=FALSE))
})


test_that("shrink 2x1", {
  m <- matrix(1:2, nrow=2, byrow=FALSE)
  expect_equal(shrink(m),
               matrix(c(2, 1), nrow=2, byrow=FALSE))
})

test_that("shrink 2x2", {
  m <- matrix(1:4, nrow=2, byrow=FALSE)
  expect_equal(shrink(m),
               matrix(c(5.5, 1, 1.5, 2), nrow=2, byrow=FALSE))
})

test_that("shrink 2x3", {
  m <- matrix(1:6, nrow=2, byrow=FALSE)
  expect_equal(shrink(m),
               matrix(c(5.5, 1, 9.5, 5), nrow=2, byrow=FALSE))
})

test_that("shrink 2x4", {
  m <- matrix(1:8, nrow=2, byrow=FALSE)
  expect_equal(shrink(m),
               matrix(c(5.5, 1, 17, 5, 3.5, 4), nrow=2, byrow=FALSE))
})

test_that("shrink 3x1", {
  m <- matrix(1:3, nrow=3, byrow=FALSE)
  expect_equal(shrink(m),
               matrix(c(2, 2.5, 0, 1.5), nrow=2, byrow=FALSE))
})

test_that("shrink 3x2", {
  m <- matrix(1:6, nrow=3, byrow=FALSE)
  expect_equal(shrink(m),
               matrix(c(6.5, 2.5, 2, 10), nrow=2, byrow=FALSE))
})

test_that("shrink 3x4", {
  m <- matrix(1:12, nrow=3, byrow=FALSE)
  expect_equal(shrink(m),
               matrix(c(6.5, 2.5, 23.5, 18.5, 5, 22), nrow=2, byrow=FALSE))
})

test_that("shrink 4x2", {
  m <- matrix(1:8, nrow=4, byrow=FALSE)
  expect_equal(shrink(m),
               matrix(c(7.5, 4.5, 6, 2.5, 15.5, 0), nrow=3, byrow=FALSE))
})

test_that("shrink 5x2", {
  m <- matrix(1:10, nrow=5, byrow=FALSE)
  expect_equal(shrink(m),
               matrix(c(8.5, 4.5, 16.5, 3, 17.5, 5), nrow=3, byrow=FALSE))
})

test_that("shrink large", {
  for (i in seq_len(20)) {
    for (j in seq(i, 20)) {
      m <- matrix(seq(from=1, to=i*j), nrow=i, ncol=j, byrow=FALSE)
      expect_equal(sum(shrink(m)), sum(m),
                   info=paste0('i=', i, ', j=', j))
      expect_equal(sum(shrink(t(m))), sum(m),
                   info=paste0('i=', i, ', j=', j, ' (transposed)'))
    }
  }
})
