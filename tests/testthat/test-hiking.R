test_that("hiking", {
  expect_equal(round(hiking(0), 2), 5.04)
  expect_equal(round(hiking(-0.05), 2), 6)
  expect_equal(round(hiking(0.84), 2), 0.27)
})
