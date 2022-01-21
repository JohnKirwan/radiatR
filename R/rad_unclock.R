# rad_unclock
#
# This is a function named 'rad_unclock'
#
#' Make ggplot object of tracks radiating from circle centre. Accepts a data frame with x and y coordinates of the points, and optional grouping variables.
#'
#' @param theta Angle angle in radians in 'clockwise' format.
#' @return An angle in radians in a conventional unit circle format.
#' @examples
#' a <- seq(from=0,to=2*pi,length.out=6)
#' mapply(rad_unclock,a)
#' @export
#
rad_unclock   <- function(theta){ttheta<-(pi/2)-theta
if(theta < -pi){theta <- theta + 2*pi
}else{ if(theta > pi){theta <- theta - 2*pi} }
return(theta)
}


