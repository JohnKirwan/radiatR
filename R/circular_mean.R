#' Calculate the circular mean of a set of angles in radians
#'
#' @param angles A numeric vector of angles in radians
#' @param normalize_range Logical, if TRUE (default), the output will be in the range (-pi, pi], otherwise in the range [0, 2 * pi)
#'
#' @return The circular mean angle in radians
#' @export
#'
#' @examples
#' angles <- c(0, pi, pi/2, 3*pi/2)
#' circular_mean(angles)
#' circular_mean(angles, normalize_range = FALSE)
circular_mean <- function(angles, normalize_range = TRUE) {
  # Check if the input is a numeric vector
  if (!is.numeric(angles)) {
    stop("The input must be a numeric vector of angles in radians")
  }
  
  # Calculate the mean of the cosine and sine values of the angles
  mean_cos <- mean(cos(angles))
  mean_sin <- mean(sin(angles))
  
  # Calculate the circular mean angle in radians
  circular_mean_angle <- atan2(mean_sin, mean_cos)
  
  if (normalize_range) {
    # Ensure the circular mean angle is within the range (-pi, pi]
    if (circular_mean_angle <= -pi) {
      circular_mean_angle <- circular_mean_angle + 2 * pi
    }
  } else {
    # Ensure the circular mean angle is within the range [0, 2 * pi)
    if (circular_mean_angle < 0) {
      circular_mean_angle <- circular_mean_angle + 2 * pi
    }
  }
  
  return(circular_mean_angle)
}
