# rad_shepherd
#
# This is a function named 'rad_shepherd'
#
#' Make ggplot object of tracks radiating from circle centre. Accepts a data frame with x and y coordinates of the points, and optional grouping variables.
#'
#' @param theta Angle angle in radians in unit circle format, going anticlockwise.
#' @return An angle in the same format, brought into range -pi,+pi
#' @examples
#' a <- seq(from=-5,to=5,length.out=6)
#' mapply(rad_shepherd,a)
#' @export
#
rad_shepherd <- function(theta){  if(theta < -pi){theta <- theta + 2*pi
} else{ if(theta > pi){theta <- theta - 2*pi} }
  return(theta)}

