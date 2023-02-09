# directedness_arrow
#
#' Make mean resultant length arrow
#'
#' @param theme The directory in which to look for the landmark files. Defaults to the current working directory.
#' @return A theme object.
#' #examples
#' #import_tracks(dir=data)
#' @export
#' @importFrom ggplot2 theme theme_classic
#
sparse_theme <- function(theme=NULL){
  if(is.null(theme)){theme=ggplot2::theme_classic()}

  xlab('') + ylab('') +
  theme +
  labs(title='',color = "Width")+ #Plot tracks with obstacle relative to stimulus')
  theme(axis.line=element_blank(),
        axis.ticks=element_blank(),
        axis.text=element_blank(),
        panel.spacing = unit(.5, "cm"),
        plot.title=element_text(size=14,hjust=0.5),#vjust=-62),
        strip.background=element_blank(),strip.text=element_blank(),
        panel.background = element_rect(fill='transparent'), #transparent panel bg
        plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
        panel.grid.major = element_blank(), #remove major gridlines
        panel.grid.minor = element_blank(), #remove minor gridlines
        legend.background = element_rect(fill='transparent',color=NA),
        legend.box.background = element_rect(fill='transparent',color=NA)
        )

        return(thematic)
}

