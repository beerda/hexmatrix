#' @export
#' @import ggplot2
plot.hexmatrix <- function(m, ...) {
  g <- ggplot() +
    geom_hex(data=long(m, .forPlot=TRUE),
             aes(x=x, y=y, fill=m),
             stat='identity') +
    scale_y_reverse() +
    xlab('') + ylab('')
  plot(g)
}
