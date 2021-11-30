# radiate
#
# This is a function named 'radiate'
# You can learn more about package authoring with RStudio at:
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
#
#' Make ggplot object of tracks radiating from circle centre.
#'
#' @param data A data frame containing a row for each tracked point.
#' @param ncols The number of columns used by grid_wrap.
#' @param circ1 Inner circle drawn.
#' @param circ2 Outer circle drawn.
#' @param group1 Grouping variable within data used by grid facet.
#' @param group2 Grouping variable within data used within panel.
#' @param ticks Logical indicating whether ticks are drawn at perimeter.
#' @return A ggplot2 graphical object.
#' @examples
#' utils::data(urchin_tracks)
#' radiate(urchin_tracks, ncols=3, group1 = "arc")
#' @importFrom rlang .data
#' @export
#
radiate <- function(
  data,ncols=NULL,circ1=NULL,circ2=NULL,group1=NULL,group2=NULL,ticks=NULL){
  if(is.null(ncols)){ncols=2}
  if(is.null(circ1)){circ1=0.1}
  if(is.null(circ2)){circ2=0.2}
  if(is.null(ticks)){ticks=TRUE}
  if(ticks==TRUE){tick_df <- data.frame(
    x=c(0,.66,.95,.66,0,-.66,-.95,-.66),
    y=c(.95,.66,0,-.66,-.95,-.66,0,.66),
    xend=c(0,.74,1.05,.74,0,-.74,-1.05,-.74),
    yend=c(1.05,.74,0,-.74,-1.05,-.74,0,.74))
  }

  ggplot2::ggplot(data=data) + ggplot2::coord_fixed() +
  ggplot2::annotate("path",colour="black",linetype="dashed",
                  x=cos(seq(0,2*pi,0.06)),y=sin(seq(0,2*pi,0.06)))+
  ggplot2::annotate("path",colour="blue", linetype="dashed",
             x=circ2*cos(seq(0,2*pi,0.05)),y=circ2*sin(seq(0,2*pi,0.05))) +
  ggplot2::annotate("path", color="black",linetype="dashed",
             x=circ1*cos(seq(0,2*pi,0.05)),y=circ1*sin(seq(0,2*pi,0.05))) +
  ggplot2::annotate("text", x= .8, y= .8, label=paste0('45', "\U00B0")) +
  ggplot2::annotate("text", x= .8, y=-.8, label=paste0('135',"\U00B0")) +
  ggplot2::annotate("text", x=-.8, y=-.8, label=paste0('225',"\U00B0")) +
  ggplot2::annotate("text", x=-.8, y= .8, label=paste0('315',"\U00B0")) +
  ggplot2::geom_segment(
      data=tick_df,
      mapping = ggplot2::aes(
        x=.data$x,
        y=.data$y,
        xend=.data$xend,
        yend=.data$yend),
      inherit.aes=F) +
  ggplot2::xlab('') +
  ggplot2::ylab('') +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.line =ggplot2::element_blank(),
                   axis.ticks=ggplot2::element_blank(),
                   axis.text =ggplot2::element_blank()) +
  ggplot2::theme(legend.position = "none") +
  ggplot2::theme(panel.spacing   = ggplot2::unit(.5,"cm")) +
  ggplot2::theme(plot.title      = ggplot2::element_text(size=14,hjust=0.5),
                   strip.background= ggplot2::element_blank(),
                   strip.text      = ggplot2::element_blank()) +
  ggplot2::theme(panel.background= ggplot2::element_rect(fill='transparent'), #transparent panel bg
          plot.background = ggplot2::element_rect(fill='transparent', color=NA), #transparent plot bg
          panel.grid.major = ggplot2::element_blank(), #remove major gridlines
          panel.grid.minor = ggplot2::element_blank(), #remove minor gridlines
          legend.background = ggplot2::element_rect(fill='transparent'), #transparent legend bg
          legend.box.background = ggplot2::element_rect(fill='transparent') #transparent legend panel
  ) -> g

  if(is.null(group1)==FALSE){
     g <- g + ggplot2::facet_wrap(.~get(group1),ncol=ncols,strip.position="top")
  }
  if(is.null(group2)==TRUE){
    g <- g + ggplot2::geom_path(data = data,
                                mapping = ggplot2::aes(
                                  x       = .data$rel_x,
                                  y       = .data$rel_y )
                                  )
  } else{
    g <- g + ggplot2::geom_path(data = data,
                       mapping = ggplot2::aes(
                         x       = .data$rel_x,
                         y       = .data$rel_y,
                         group  = eval(str2lang(paste0('.data$',group2))),
                         colour = eval(str2lang(paste0('.data$',group2)))
                       ))
  }

  return(g)
}
