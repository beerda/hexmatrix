#' @export
hiking <- function(gradient) {
  # gradient is dh/dx
  # see https://en.wikipedia.org/wiki/Tobler%27s_hiking_function
  6 * exp((-3.5) * abs(gradient + 0.05))
}
