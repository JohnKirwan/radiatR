#' Convert the given point from pixels to millimeters
#'
#' @param xy point
#' @param f focal length in pixels
#' @param F focal length in millimeters
#' @keywords internal
#' @return point in millimeters
scaled_xy2mm <- function(xy, f, F) {
  xy_px <- xy * F
  xy_mm <- xy_px / (F / f)
  return(xy_mm)
}
