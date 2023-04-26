#' Calibrate the camera on the given xy coordinate
#'
#' @param x x-coordinate
#' @param y y-coordinate
#' @param K intrinsic camera matrix
#' @param k radial distortion coefficients
#' @param F focal length in millimeters
#' @return calibrated point
#' @export
#' @examples
#' K <- matrix(c(784.948340421183, 0, 0,
#'               0, 782.554388639436, 0,
#'               939.047051578744, 528.896744808718, 1), ncol = 3, byrow = TRUE)
#' k <- c(-0.289927776375773, 0.0392224238600441)
#' F <- rep(680, 2)
#' cam_cal_pt(100, 100, K, k, F)
cam_cal_pt <- function(x, y, K, k, F) {
  if (!(length(x) == 1 && is.numeric(x) && length(y) == 1 && is.numeric(y))) {
    warning('xy not a pt!')
  }
  xy <- optically_center(x, y, K[3, 1:2])
  xy <- focalize(xy, c(K[1, 1], K[2, 2]))
  xy <- radial_distort(xy, k)
  xy <- scaled_xy2mm(xy, c(K[1, 1], K[2, 2]), F)
  return(xy)
}
