test_that("update(vector)", {
  expect_equal(update(NULL, NULL), NULL)

  expect_equal(update(NULL, 'a'), 'a')
  expect_equal(update('a', NULL), 'a')

  expect_equal(update(NULL, c('a', 'b')), c('a', 'b'))
  expect_equal(update(c('a', 'b'), NULL), c('a', 'b'))

  expect_equal(update(c('a', 'b', 'c'),
                      c('A', 'B')),
               c('A', 'B', 'c'))

  expect_equal(update(c('A', 'B'),
                      c('a', 'b', 'c')),
                      c('a', 'b', 'c'))
  expect_equal(update(c(one='a', two='b', three='c'),
                      c(two='A', one='B')),
               c(one='B', two='A', three='c'))
})


test_that("update(list)", {
  expect_equal(update(NULL, NULL), NULL)

  expect_equal(update(NULL, 'a'), 'a')
  expect_equal(update('a', NULL), 'a')

  expect_equal(update(NULL, list('a', 'b')), list('a', 'b'))
  expect_equal(update(list('a', 'b'), NULL), list('a', 'b'))

  expect_equal(update(list('a', 'b', 'c'),
                      list('A', 'B')),
               list('A', 'B', 'c'))

  expect_equal(update(list('A', 'B'),
                      list('a', 'b', 'c')),
                      list('a', 'b', 'c'))
  expect_equal(update(list(one='a', two='b', three='c'),
                      list(two='A', one='B')),
               list(one='B', two='A', three='c'))
})
