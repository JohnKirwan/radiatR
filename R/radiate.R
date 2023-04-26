# radiate
#
#' Make ggplot object of tracks radiating from circle centre. Accepts a data frame with x and y coordinates of the points, and optional grouping variables.
#'
#' @param data A data frame containing a row for each tracked point.
#' @param x_col A character specifying the name of the column representing the x coordinates
#' @param y_col A character specifying the name of the column representing the y coordinates
#' @param group2 Grouping variable within data used within panel.
#' @param ticks Logical indicating whether ticks are drawn at perimeter.
#' @param degrees Logical indicating whether to mark degrees at panel diagonals. Defaults to TRUE.
#' @param legend Logical indicating whether to show legend. Defaults to FALSE.
#' @param title Plot title
#' @param xlab Text to include as plot x label. Defaults to none.
#' @param ylab Text to include as plot y label. Defaults to none.
#' @param axes Logical indicating whether panel axes should be shown.
#' @return A ggplot2 graphical object.
#' @examples
#' utils::data(urchin_tracks)
#' radiate(urchin_tracks, ncols = 3, group1 = "arc", group2 = "id")
#' @importFrom ggplot2 ggplot
#' @importFrom rlang .data
#' @export
#
radiate <- function(
  data,
  xcol, ycol,
  geom = NULL,
  #color = NULL,
  group2 = NULL,
  circ1 = NULL, circ2 = NULL, ticks = NULL,
  degrees = NULL, legend = NULL, title = NULL,
  xlab = NULL, ylab = NULL, axes = NULL, ...){
  #if (is.null(circ1)) {circ1 = 0.1}
  #if (is.null(circ2)) {circ2 = 0.2}
  if (is.null(ticks)) {ticks = TRUE}
  if (is.null(degrees)) {degrees = TRUE}
  if (is.null(legend)) {legend = FALSE}
  if (is.null(axes)) {axes = FALSE}
  if (is.null(geom)) {geom = "geom_path"}

  ggplot2::ggplot(data = data,
                  mapping = aes(x = rel_x, y = rel_y)) + ggplot2::coord_fixed() -> g

  # remove grid lines et cetera
  default_theme <- function() {
    spartan_theme()
  }
  g <- g + default_theme()

  # add inner circles
  g <- g + add_multiple_circles()
  g <- g + add_multiple_circles(radii = c(0.1, 0.2), circle_color = "skyblue")

  # add the paths
  g <- g + draw_tracks(data = data, geom = geom,
                       #aes_args = list(), other_args = list(),
                       ...)

  # g <- g +
  #     ggplot2::geom_path(
  #     data = data,
  #     mapping = ggplot2::aes(
  #       x       = .data$rel_x,
  #       y       = .data$rel_y,
  #       group   = {{group2}},
  #       if (is.null(color) == F) {color = {{color}}}
  #       )
  #     )

  # add unit circle
  g <- g + add_circ()

  # Add degrees at diagonals
  if (degrees == TRUE)  g <- g + degree_labs()

  # Add ticks to periphery
  if (ticks == TRUE)    g <- g + add_ticks()

  # remove the legend
  if (legend == FALSE) {
    g <- g + ggplot2::theme(
      legend.position = "none",
      legend.background = ggplot2::element_rect(fill = 'transparent'),
      legend.box.background = ggplot2::element_rect(fill = 'transparent'))
  }

  # remove the Cartesian axes
  if (axes == FALSE) {
    g <- g + ggplot2::theme(
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank())
  }

  # remove x and y Cartesian axis labels
  if (is.null(xlab)) {
    g <- g + ggplot2::xlab('')
  }
  else {g <- g + ggplot2::xlab(as.character(xlab))
  }

  if (is.null(ylab)) {g <- g + ggplot2::ylab('')
  }
  else {g <- g + ggplot2::ylab(as.character(ylab))
  }

  if (is.null(title)) {g <- g + ggplot2::theme(plot.title = ggplot2::element_blank())
  }
  else {g <- g + ggplot2::ggtitle(as.character(title)) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 14,hjust = 0.5))
  }

  return(g)
}

# For Mean Resultant Length
#geom_spoke(data = headings_summ[
#  headings_summ$obstacle %in% obstruct & headings_summ$arc %in% arc_angs,],
#  aes(x = 0,y = 0,group = arc,angle = mu_r,radius = R),
#  size = 1.3,colour="black",arrow = arrow(length = unit(.2,"cm")))+

