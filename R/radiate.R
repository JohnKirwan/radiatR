# radiate
#
# This is a function named 'radiate'
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

radiate <- function(data,ncols=NULL,circ1=NULL,circ2=NULL,group1=NULL,
                    group2=NULL,ticks=NULL){
  require('ggplot2')
  #arg <- match.call()
  if(is.null(ncols)){ncols=2}
  if(is.null(circ1)){circ1=0.1}
  if(is.null(circ2)){circ2=0.2}
  if(is.null(ticks)){ticks=TRUE}

if(ticks==TRUE){tick_df <- data.frame(
  x=c(0,.66,.95,.66,0,-.66,-.95,-.66),y=c(.95,.66,0,-.66,-.95,-.66,0,.66),
  xend=c(0,.74,1.05,.74,0,-.74,-1.05,-.74),yend=c(1.05,.74,0,-.74,-1.05,-.74,0,.74))}

  ggplot(data=data) + coord_fixed() +
    geom_path(aes(x=rel_x, y=rel_y, color=id))+
    annotate("path",colour="black",x=cos(seq(0,2*pi,0.06)),y=sin(seq(0,2*pi,0.06)))+
    annotate("path", color="blue",linetype="dashed",
             x=circ2*cos(seq(0,2*pi,0.05)),y=circ2*sin(seq(0,2*pi,0.05))) +
    annotate("path", color="black",linetype="dashed",
             x=circ1*cos(seq(0,2*pi,0.05)),y=circ1*sin(seq(0,2*pi,0.05))) +
    annotate("text", x= .8, y= .8, label=paste0('45',"°"))  +
    annotate("text", x= .8, y=-.8, label=paste0('135',"°")) +
    annotate("text", x=-.8, y=-.8, label=paste0('225',"°")) +
    annotate("text", x=-.8, y= .8, label=paste0('315',"°")) +
    geom_segment(data=tick_df,aes(x=x,y=y,xend=xend,yend=yend),inherit.aes=F) +
    xlab('') + ylab('') + theme_classic() +
    theme(axis.line=element_blank(),axis.ticks=element_blank(),
          axis.text=element_blank()) + theme(legend.position = "none") +
    theme(panel.spacing = unit(.5,"cm")) +
    theme(plot.title=element_text(size=14,hjust=0.5),
          strip.background=element_blank(),strip.text=element_blank()) +
    theme(panel.background = element_rect(fill='transparent'), #transparent panel bg
          plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
          panel.grid.major = element_blank(), #remove major gridlines
          panel.grid.minor = element_blank(), #remove minor gridlines
          legend.background = element_rect(fill='transparent'), #transparent legend bg
          legend.box.background = element_rect(fill='transparent') #transparent legend panel
    ) -> ggplot_obj

   if(is.null(group1)==FALSE){
     ggplot_obj<-ggplot_obj + facet_wrap(.~get(group1),ncol=ncols,
                                         strip.position="top")
   }
  return(ggplot_obj)
}


