#' @export
#' @import ggplot2
plot.matrix <- function(m, ...) {
  assert_that(is.hexarray(m) || is.hexmatrix(m))
  m <- as.hexmatrix(m)

  g <- ggplot() +
    geom_hex(data=long(m, .forPlot=TRUE),
             aes(x=x, y=y, fill=m, group=1),
             stat='identity') +
    scale_y_reverse() +
    xlab('') + ylab('')
  plot(g)
}
