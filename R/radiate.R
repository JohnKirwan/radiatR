# radiate
#
#' Make ggplot object of tracks radiating from circle centre. Accepts a data frame with x and y coordinates of the points, and optional grouping variables.
#'
#' @param data A data frame containing a row for each tracked point.
#' @param ncols The number of columns used by grid_wrap.
#' @param circ1 Inner circle drawn.
#' @param circ2 Outer circle drawn.
#' @param group1 Grouping variable within data used by grid facet.
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
#' @importFrom rlang .data
#' @export
#
radiate <- function(
  data, ncols = NULL,
  group1 = NULL, group2 = NULL,
  circ1 = NULL, circ2 = NULL, ticks = NULL,
  degrees = NULL, legend = NULL, title = NULL,
  xlab = NULL, ylab = NULL, axes = NULL, ...){
  if (is.null(ncols)) {ncols = 3}
  if (is.null(circ1)) {circ1 = 0.1}
  if (is.null(circ2)) {circ2 = 0.2}
  if (is.null(ticks)) {ticks = TRUE}
  if (is.null(degrees)) {degrees = TRUE}
  if (is.null(legend)) {legend = FALSE}
  if (is.null(axes)) {axes = FALSE}

  ggplot2::ggplot(data = data) + ggplot2::coord_fixed() +
  ggplot2::annotate("path",colour = "black",linetype = "dashed",
                  x = cos(seq(0,2*pi,0.06)),y = sin(seq(0,2*pi,0.06))) +
  ggplot2::annotate("path", colour = "blue", linetype = "dashed",
             x = circ2*cos(seq(0,2*pi,0.05)),y = circ2*sin(seq(0,2*pi,0.05))) +
  ggplot2::annotate("path", color = "black",linetype = "dashed",
             x = circ1*cos(seq(0,2*pi,0.05)),y = circ1*sin(seq(0,2*pi,0.05))) +
  ggplot2::theme(
    strip.background = ggplot2::element_blank(),
    strip.text       = ggplot2::element_blank()) +
  ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = 'transparent'), #panel bg
    plot.background  = ggplot2::element_rect(fill = 'transparent', color = NA),#plot bg
    panel.grid.major = ggplot2::element_blank(), #remove major gridlines
    panel.grid.minor = ggplot2::element_blank() #remove minor gridlines
  ) -> g
  # Add facets
  if (is.null(group1) == FALSE) {
     g <- g + ggplot2::facet_wrap(
       .~get({{group1}}), ncol = ncols,strip.position = "top") +
       ggplot2::theme(panel.spacing = ggplot2::unit(.5,"cm"))
  }
  # Add track paths
  # data |>
  #   group_by({{ group2 }}) |>
  g <- g +
      ggplot2::geom_path(
      data = data,
      mapping = ggplot2::aes(
        x       = .data$rel_x,
        y       = .data$rel_y,
        group   = {{group2}},
        colour  = {{group2}}
        )
      )

  # Add degrees at diagonals
  if (degrees == TRUE) {
    g <- g +
    ggplot2::annotate("text", x =  .85, y =  .85, label = paste0('45', "\U00B0")) +
    ggplot2::annotate("text", x =  .85, y = -.85, label = paste0('135',"\U00B0")) +
    ggplot2::annotate("text", x = -.85, y = -.85, label = paste0('225',"\U00B0")) +
    ggplot2::annotate("text", x = -.85, y =  .85, label = paste0('315',"\U00B0"))
  }
  # Add ticks to periphery
  if (ticks == TRUE) {tick_df <- data.frame(
    x = c(0,.66,.95,.66,0,-.66,-.95,-.66),y = c(.95,.66,0,-.66,-.95,-.66,0,.66),
    xend = c(0,.74,1.05,.74,0,-.74,-1.05,-.74),yend = c(1.05,.74,0,-.74,-1.05,-.74,0,.74))
    g <- g +
      ggplot2::geom_segment(
        data = tick_df,
        mapping = ggplot2::aes(
          x     = .data$x,
          y     = .data$y,
          xend  = .data$xend,
          yend  = .data$yend),
        inherit.aes = F)
    }
  if (legend == FALSE) {
    g <- g + ggplot2::theme(
      legend.position = "none",
      legend.background = ggplot2::element_rect(fill = 'transparent'),
      legend.box.background = ggplot2::element_rect(fill = 'transparent'))
    }
  if (axes == FALSE) {
    g <- g + ggplot2::theme(
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank())
    }
    if (is.null(xlab)) {g <- g + ggplot2::xlab('')
    } else{g <- g + ggplot2::xlab(as.character(xlab))
    }
  if (is.null(ylab)) {g <- g + ggplot2::ylab('')
  } else {g <- g + ggplot2::ylab(as.character(ylab))
  }
  if (is.null(title)) {g <- g + ggplot2::theme(plot.title = ggplot2::element_blank())
  } else {g <- g + ggplot2::ggtitle(as.character(title)) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 14,hjust = 0.5))
  }
  return(g)
}

# For Mean Resultant Length
#geom_spoke(data = headings_summ[
#  headings_summ$obstacle %in% obstruct & headings_summ$arc %in% arc_angs,],
#  aes(x = 0,y = 0,group = arc,angle = mu_r,radius = R),
#  size = 1.3,colour="black",arrow = arrow(length = unit(.2,"cm")))+
