#' Create annotations for degrees at diagonals
#'
#' This function creates a list of annotation layers for degree marks at the diagonals.
#' The function takes one argument, a boolean value to specify if degree marks should be created or not.
#'
#' @param degrees A boolean indicating if degree marks should be created (default is TRUE)
#' @importFrom ggplot2 ggplot annotate
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
add_ticks <- function() {
    # Add ticks to periphery
    tick_df <- data.frame(
      x = c(0,.66,.95,.66,0,-.66,-.95,-.66),
      y = c(.95,.66,0,-.66,-.95,-.66,0,.66),
      xend = c(0,.74,1.05,.74,0,-.74,-1.05,-.74),
      yend = c(1.05,.74,0,-.74,-1.05,-.74,0,.74)
      )
    
    ticks <- ggplot2::geom_segment(
      data = tick_df,
      mapping = ggplot2::aes(
      x     = .data$x,
      y     = .data$y,
      xend  = .data$xend,
      yend  = .data$yend),
      inherit.aes = F
      )
  
  return(ticks)
}
