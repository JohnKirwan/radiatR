
#' Apply radial distortion to the given point
#'
#' @param xy point
#' @param k radial distortion coefficients
#' @return distorted point
radial_distort <- function(xy, k) {
  r_sq <- xy[1]^2 + xy[2]^2
  x_dist <- xy[1] * (1 + k[1] * r_sq + k[2] * (r_sq^2))
  y_dist <- xy[2] * (1 + k[1] * r_sq + k[2] * (r_sq^2))
  xy_dist <- cbind(x_dist, y_dist)
  return(xy_dist)
}
