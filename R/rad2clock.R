# rad2clock
#
# This is a function named 'rad2clock'
#
#' Make ggplot object of tracks radiating from circle centre. Accepts a data frame with x and y coordinates of the points, and optional grouping variables.
#'
#' @param theta Angle angle in radians in conventional unit circle format.
#' @return An angle in radians in a 'clockwise' format.
#' @examples
#' a <- seq(from=pi/2,to=-pi/2,length.out=6)
#' mapply(rad2clock,a)
#' @export
#
rad2clock   <- function(theta){theta<-(pi/2)-theta
  if(theta < 0){theta <- theta + 2*pi
  }else{ if(theta > 2*pi){theta <- theta - 2*pi} }
  return(theta)
}


