test_that("pden", {
  m <- matrix(c(1, 2, 3, 4,
                2, 2, 3, 4,
                3, 3, 3, 4,
                4, 4, 4, NA), nrow=4, ncol=4)
  p1 <- 0.05
  p2 <- (0.1 - 0.05) / 3
  p3 <- (0.2 - 0.1) / 5
  p4 <- (0.4 - 0.2) / 6

  res <- pden(m, values=1:4, probs=c(0.05, 0.1, 0.2, 0.4))
  expect_equal(res,
               matrix(c(p1, p2, p3, p4,
                        p2, p2, p3, p4,
                        p3, p3, p3, p4,
                        p4, p4, p4, NA), nrow=4, ncol=4))
})
