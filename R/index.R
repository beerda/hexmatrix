#' @export
pointToIndex <- function(point, m=NULL, rows=nrow(m), cols=ncol(m)) {
  assert_that(is.count(rows))
  assert_that(is.count(cols))
  assert_that(length(point) == 2)
  assert_that(is.count(point[1]))
  assert_that(is.count(point[2]))
  assert_that(point[1] <= rows)
  assert_that(point[2] <= cols)

  point[1] + rows * (point[2] - 1)
}


#' @export
indexToPoint <- function(i, m=NULL, rows=nrow(m), cols=ncol(m)) {
  assert_that(is.count(rows))
  assert_that(is.count(cols))
  assert_that(is.count(i))
  assert_that(i <= rows * cols)

  c((i - 1) %% rows + 1, ((i - 1) %/% rows) + 1)
}
