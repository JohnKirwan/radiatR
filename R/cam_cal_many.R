#' Calibrate the camera on the given set of points
#'
#' @param points_idx matrix of points
#' @param K intrinsic camera matrix
#' @param k radial distortion coefficients
#' @param F focal length in millimeters
#' @return matrix of calibrated points
#' @export
#' @examples
#' K <- matrix(c(784.948340421183, 0, 0,
#'               0, 782.554388639436, 0,
#'               939.047051578744, 528.896744808718, 1), ncol = 3, byrow = TRUE)
#' k <- c(-0.289927776375773, 0.0392224238600441)
#' F <- rep(680, 2)
#' points <- matrix(c(100, 100, 200, 200, 300, 300), ncol = 2, byrow = TRUE)
#' cam_cal_many(points, K, k, F)
cam_cal_many <- function(points_idx, K, k, F) {
  temp <- array(data = 0, dim = c(dim(points_idx)[1], 2))
  for (i in 1:dim(points_idx)[1]) {
    temp[i, 1:2] <- (radiatR::cam_cal_pt(points_idx[i, 1], points_idx[i, 2], K, k, F))
  }
  return(temp)
}
