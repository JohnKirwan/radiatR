# rad_shepherd_clock
#
# This is a function named 'rad_shepherd_clock'
#
#' Make ggplot object of tracks radiating from circle centre. Accepts a data frame with x and y coordinates of the points, and optional grouping variables.
#'
#' @param theta Angle angle in radians in 'clock' format, going clockwise
#' @return An angle in the same format, brought into range -pi,+pi
#' @examples
#' a <- seq(from=-2,to=8,length.out=6)
#' mapply(rad_shepherd_clock,a)
#' @export
#
rad_shepherd_clock <- function(theta){  if(theta < 0){theta <- theta + 2*pi
} else{ if(theta > 2*pi){theta <- theta - 2*pi} }
  return(theta)}


