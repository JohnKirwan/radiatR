# Calibration utilities for camera-based coordinate correction
#

#' Normalize the given point by the focal length
#'
#' @param xy point
#' @param f focal length
#' @keywords internal
#' @return normalized point
focalize <- function(xy, f) {
  xy / f
}

#' Optically center the given point
#'
#' @param x x-coordinate
#' @param y y-coordinate
#' @param c optical center
#' @keywords internal
#' @return normalized point
optically_center <- function(x, y, c) {
  c(x, y) - c
}

#' Apply radial distortion to the given point
#'
#' @param xy point
#' @param k radial distortion coefficients
#' @return distorted point
radial_distort <- function(xy, k) {
  r_sq <- xy[1]^2 + xy[2]^2
  scale <- (1 + k[1] * r_sq + k[2] * (r_sq^2))
  xy * scale
}

#' Convert the given point from pixels to millimeters
#'
#' @param xy point
#' @param f focal length in pixels
#' @param F focal length in millimeters
#' @keywords internal
#' @return point in millimeters
scaled_xy2mm <- function(xy, f, F) {
  xy * F / (F / f)
}

#' Switch axes of the given intrinsic camera matrix
#'
#' @param K intrinsic camera matrix
#' @keywords internal
#' @return modified intrinsic camera matrix
calibration_switch_axes <- function(K) {
  temp_matrix <- K
  temp_matrix[1, 1] <- K[2, 2]
  temp_matrix[2, 2] <- K[1, 1]
  temp_matrix[3, 1] <- K[3, 2]
  temp_matrix[3, 2] <- K[3, 1]
  temp_matrix
}

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
    warning("xy not a pt!")
  }
  xy <- optically_center(x, y, K[3, 1:2])
  xy <- focalize(xy, c(K[1, 1], K[2, 2]))
  xy <- radial_distort(xy, k)
  scaled_xy2mm(xy, c(K[1, 1], K[2, 2]), F)
}

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
  temp <- array(data = 0, dim = c(nrow(points_idx), 2))
  for (i in seq_len(nrow(points_idx))) {
    temp[i, 1:2] <- cam_cal_pt(points_idx[i, 1], points_idx[i, 2], K, k, F)
  }
  temp
}

#' @include TrajSet.R
NULL

#' Camera calibration model for trajectory correction
#'
#' @name CalModel-class
#' @rdname CalModel-class
#' @slot K 3x3 intrinsic calibration matrix
#' @slot k Numeric vector of radial distortion coefficients (k1, k2, ...)
#' @slot F Numeric scalar or length-2 vector giving focal length scaling (e.g., mm per pixel)
#' @seealso \code{\link{calibrate_positions}}
#' @family calibration
#' @exportClass CalModel
setClass(
  "CalModel",
  slots = c(
    K = "matrix",
    k = "numeric",
    F = "numeric"
  )
)

#' Calibrate TrajSet positions using a camera model
#'
#' Applies camera intrinsics + radial distortion to convert pixel coordinates to metric space.
#'
#' @param x TrajSet with `x`/`y` coordinates
#' @param model `CalModel` object containing calibration parameters
#' @return TrajSet with updated `x`/`y` and angles in metric space
#' @family calibration
#' @export
setGeneric("calibrate_positions", function(x, model) standardGeneric("calibrate_positions"))

#' @rdname calibrate_positions
#' @export
setMethod("calibrate_positions", signature(x = "TrajSet", model = "CalModel"), function(x, model) {
  if (is.null(x@cols$x) || is.null(x@cols$y))
    stop("TrajSet has no x/y columns; supply cartesian coords to use calibration.")
  xc <- x@cols$x; yc <- x@cols$y
  K <- model@K; k <- model@k; F <- model@F

  xy <- cbind(x@data[[xc]], x@data[[yc]])
  xy <- sweep(xy, 2, K[3, 1:2], FUN = "-")
  xy <- sweep(xy, 2, c(K[1, 1], K[2, 2]), FUN = "/")
  r2 <- rowSums(xy^2)
  scale <- (1 + k[0 + 1] * r2 + k[0 + 2] * (r2^2))
  xy <- xy * scale
  xy_mm <- xy * F / (F / c(K[1, 1], K[2, 2]))

  x@data[[xc]] <- xy_mm[, 1]
  x@data[[yc]] <- xy_mm[, 2]
  x@data[[x@cols$angle]] <- (atan2(xy_mm[, 2], xy_mm[, 1]) %% (2 * pi))
  x
})
