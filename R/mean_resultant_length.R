#' Calculate the mean resultant length of a set of angles in radians
#'
#' @param angles A numeric vector of angles in radians
#'
#' @return The mean resultant length (R)
#' @export
#'
#' @examples
#' angles <- c(0, pi, pi/2, 3*pi/2)
#' mean_resultant_length(angles)
mean_resultant_length <- function(angles) {
  # Check if the input is a numeric vector
  if (!is.numeric(angles)) {
    stop("The input must be a numeric vector of angles in radians")
  }
  
  # Calculate the mean of the cosine and sine values of the angles
  mean_cos <- mean(cos(angles))
  mean_sin <- mean(sin(angles))
  
  # Calculate the mean resultant length (R)
  R <- sqrt(mean_cos^2 + mean_sin^2)
  
  return(R)
}