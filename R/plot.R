#' @export
#' @import ggplot2
plot.matrix <- function(m, ...) {
  if (is.hexarray(m)) {
    m <- m[, , 1]
  }
  assert_that(is.hexmatrix(m))

  g <- ggplot() +
    geom_hex(data=long(m, .forPlot=TRUE),
             aes(x=x, y=y, fill=m),
             stat='identity') +
    scale_y_reverse() +
    xlab('') + ylab('')
  plot(g)
}
