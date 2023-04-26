#' Create a custom ggplot theme
#'
#' This function creates a custom ggplot theme object with specific theme elements.
#' The resulting theme object can be added to an existing ggplot object.
#'
#' @importFrom ggplot2 theme element_blank element_rect %+replace%
#' @return A ggplot theme object with the specified customizations
#'
#' @examples
#' # Create a custom theme
#' custom_theme <- create_custom_theme()
#'
#' # Create a ggplot object
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(x = mpg, y = disp)) + geom_point()
#'
#' # Apply the custom theme
#' p_modified <- p + custom_theme
#' print(p_modified)
#'
spartan_theme <- function(...) {
  theme_grey(...) %+replace%
    theme(
      strip.background = ggplot2::element_blank(),
      strip.text       = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = 'transparent'),
      plot.background  = ggplot2::element_rect(fill = 'transparent', color = NA),
      #remove grid lines
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
      )
  }
