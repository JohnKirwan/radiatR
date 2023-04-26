#' Switch axes of the given intrinsic camera matrix
#'
#' @param K intrinsic camera matrix
#' @keywords internal
#' @return modified intrinsic camera matrix
calibration_switch_axes <- function(K) {
  temp_matrix <- K
  temp_matrix[1, 1] <- K[2, 2]
  temp_matrix[2, 2] <- K[1, 1]
  temp_matrix[3, 1] <- K[3, 2]
  temp_matrix[3, 2] <- K[3, 1]
  return(temp_matrix)
}
