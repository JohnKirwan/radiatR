#' Create annotations for degrees at diagonals
#'
#' This function creates a list of annotation layers for degree marks at the diagonals.
#' The function takes one argument, a boolean value to specify if degree marks should be created or not.
#'
#' @param radius radius between 0 and 1
#' @param circle_colour description
#' @param circle_alpha alpha transparency 
#' @param circle_size size
#' @importFrom ggplot2 scale_x_continuous coord_polar
#' @return A geom_segment object of ticks
#'
#' @examples
#' # Create degree annotations
#' ticks <- add_ticks(TRUE)
#' 
#' # Create a ggplot object
#' library(ggplot2)
#' p <- ggplot() + coord_polar()
#'
#' # Add degree marks at diagonals
#' p_modified <- p + ticks
#'
add_circ <- function(
    radius = 1, circle_color = "grey60", circle_alpha = 1, circle_size = 1) {
  
  # Create a list of layers
  g_unit <- list(
    annotate("path",
             x = radius * cos(seq(0, 2 * pi, length.out = 1000)),
             y = radius * sin(seq(0, 2 * pi, length.out = 1000)),
             color = circle_color, alpha = circle_alpha, size = circle_size)
  )
  
  return(g_unit)
}