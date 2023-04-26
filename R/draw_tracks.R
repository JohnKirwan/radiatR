#' Create geom_point or geom_line object for Cartesian coordinates
#'
#' This function takes sets of Cartesian coordinates, and creates a ggplot2 object using
#' either geom_point or geom_line, based on the specified input parameter `type`.
#'
#' @param geom A character specifying the type of object to create, either "point" or "line" (default is "point")
#' @param data data
#' @param ... Additional arguments to be passed to the internal geom_point or geom_line functions
#' @importFrom ggplot2 geom_point geom_path aes
#' @importFrom rlang enexpr list2 expr
#'
#' @return A ggplot2 geom_point or geom_path object based on the input type
#'
#' @examples
#' data <- data.frame(lon = c(1, 2, 3, 2, 0), lat = c(3, 4, 5, 7, 8),
#' cohort = c("A", "A", "B", "B", "C"))
#' geom_obj <- draw_tracks(data, mapping = aes(color = cohort))
#' ggplot(data, aes(x = lon, y = lat)) + geom_obj
#'
#' @export
draw_tracks <- function(data, geom = NULL, ...) {

  if (is.null(geom)) {geom <- "geom_path"}
  geom_expression <- enexpr(geom)

  # Main part
  geom_obj <- list2(eval(expr((!!geom_expression)(...))))

    return(geom_obj)
}
