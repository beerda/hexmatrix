#' Tobler's hiking function
#'
#' A function that determines the hiking speed of a typical healthy adult person while
#' taking into account the slope angle.
#'
#' @param gradient a fraction of elevation difference over the horizontal distance
#'     (also a tangens of the slope angle)
#' @return a walking velocity in km/h
#' @references Tobler, Waldo (February 1993). Three presentations on geographical
#'     analysis and modeling: Non-isotropic geographic modeling speculations on the
#'     geometry of geography global spatial analysis. Technical Report. National
#'     center for geographic information and analysis. 93 (1).
#'
#' @export
#'
#' @examples
#' hiking(-0.05)   # a maximum of 6 km/h corresponding to the downhill angle of 2.86 degrees
#' hiking(0)       # speed on a path with flat surface
hiking <- function(gradient) {
  # gradient is dh/dx
  # see https://en.wikipedia.org/wiki/Tobler%27s_hiking_function
  6 * exp((-3.5) * abs(gradient + 0.05))
}
