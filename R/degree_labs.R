#' Create annotations for degrees at diagonals
#'
#' This function creates a list of annotation layers for degree marks at the diagonals.
#' The function takes one argument, a boolean value to specify if degree marks should be created or not.
#'
#' @param degrees A boolean indicating if degree marks should be created (default is TRUE)
#' @importFrom ggplot2 ggplot annotate
#' @return A list of ggplot2 annotation layers for degree marks at the diagonals if `degrees` is TRUE
#'
#' @examples
#' # Create degree annotations
#' degree_labs <- degree_labs(TRUE)
#' 
#' # Create a ggplot object
#' library(ggplot2)
#' p <- ggplot() + coord_polar()
#'
#' # Add degree marks at diagonals
#' p_modified <- p + degree_labs
#'
degree_labs <- function() {
  annotations <- list()
  
  #if (degrees == TRUE) {
    annotations <- list(
      ggplot2::annotate("text", x =  .85, y =  .85, label = paste0('45', "\U00B0")),
      ggplot2::annotate("text", x =  .85, y = -.85, label = paste0('135',"\U00B0")),
      ggplot2::annotate("text", x = -.85, y = -.85, label = paste0('225',"\U00B0")),
      ggplot2::annotate("text", x = -.85, y =  .85, label = paste0('315',"\U00B0"))
    )
  #}
  
  return(annotations)
}
