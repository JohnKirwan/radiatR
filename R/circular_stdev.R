#' Calculate the circular standard deviation using the mean resultant length (R)
#'
#' @param R The mean resultant length (R)
#'
#' @return The circular standard deviation
#' @export
#'
#' @examples
#' R <- mean_resultant_length(c(0, pi, pi/2, 3*pi/2))
#' circular_sd_from_R(R)
circular_sd_from_R <- function(R) {
  # Check if the input is a numeric value
  if (!is.numeric(R) || length(R) != 1) {
    stop("The input must be a single numeric value")
  }
  
  # Calculate the circular standard deviation using the mean resultant length (R)
  circular_sd_value <- sqrt(-2 * log(R))
  
  return(circular_sd_value)
}
