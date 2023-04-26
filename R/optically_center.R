#' Optically center the given point
#'
#' @param x x-coordinate
#' @param y y-coordinate
#' @param c optical center
#' @keywords internal
#' @return normalized point
optically_center <- function(x, y, c) {
  xy_norm <- c(x, y) - c
  return(xy_norm)
}
