test_that("combine lists", {
  expect_equal(combine(list(),
                       list()),
               list())

  expect_equal(combine(list(1, 2, 3),
                       list(2, 4, 6), f=`-`),
               list(-1, -2, -3))

  expect_equal(combine(list(1, 2, 3),
                       list(2, 4, 6)),
               list(3, 6, 9))

  expect_equal(combine(list(1, 2, 3),
                       list(2, 4)),
               list(3, 6, 3))

  expect_equal(combine(list(1, 2),
                       list(2, 4, 6)),
               list(3, 6, 6))

  expect_equal(combine(list(1, 2, 3),
                       list(NULL, NULL, NULL, 2, 4, 6)),
               list(1, 2, 3, 2, 4, 6))

  expect_equal(combine(list(NULL, NULL, 3),
                       list()),
               list(NULL, NULL, 3))

  a <- list()
  a[[3]] <- 10
  b <- list()
  a[[5]] <- 20
  expect_equal(combine(a, b),
               list(NULL, NULL, 10, NULL, 20))
})
