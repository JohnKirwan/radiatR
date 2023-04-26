#' Normalize the given point by the focal length
#'
#' @param xy point
#' @param f focal length
#' @keywords internal
#' @return normalized point
focalize <- function(xy, f) {
  xy_norm <- xy / f
  return(xy_norm)
}
