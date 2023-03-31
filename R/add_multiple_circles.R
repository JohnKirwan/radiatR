#' Add multiple concentric circles to a ggplot object
#'
#' This function creates a list of layers for concentric circles with specified radii that can be added to a ggplot object.
#' The function takes optional arguments to customize the appearance of the circles.
#'
#' @param radii A vector of radii for the concentric circles (default is c(0.25, 0.5, 0.75, 1))
#' @param circle_color The color of the circles (default is "grey40")
#' @param circle_alpha The transparency of the circles (default is 1)
#' @param circle_size The size of the circle lines (default is 1)
#' @importFrom ggplot2 annotate
#' @return A list of layers for concentric circles
#'
#' @examples
#' # Create concentric circles with custom appearance
#' circles <- add_multiple_circles(radii = c(0.2, 0.4, 0.6, 0.8, 1),
#'                                 circle_color = "grey",
#'                                 circle_alpha = 0.8,
#'                                 circle_size = 1)
#'
#' # Create a ggplot object
#' library(ggplot2)
#' p <- ggplot() + coord_fixed()
#'
#' # Add the concentric circles
#' p_modified <- p + circles
#'
add_multiple_circles <- function(radii = c(0.25, 0.5, 0.75),
                                 circle_color = "grey20",
                                 circle_alpha = 1,
                                 circle_size = 0.5) {
  
  circles_list <- list()
  
  for (radius in radii) {
    circle <- add_circ(radius, circle_color, circle_alpha, circle_size)
    circles_list <- append(circles_list, circle)
  }
  
  return(circles_list)
}

