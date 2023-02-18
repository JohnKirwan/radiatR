# directedness_arrow
#
#' Make mean resultant length arrow
#'
#' @param trials_df The directory in which to look for the landmark files. Defaults to the current working directory.
#' @param arrow_head_cm The suffix of files containing landmark coordinates.
#' @param color The suffix of files containing track coordinates.
#' @param size Size of the segment.
#' @return A data frame of file names.
#' #examples
#' #import_tracks(dir=data)
#' @export
#' @importFrom ggplot2 geom_segment
#' @importFrom circular mean.circular rho.circular
#
directedness_arrow <- function(dataframe, arrow_head_cm = NULL, color = NULL, size=NULL){
  if (is.null(arrow_head_cm)) {arrow_head_cm = 0.2}
  if (is.null(color)) {color = "gray"}
  if (is.null(size)) {size = 2}

  s <- geom_segment( dataframe,
    aes(x = 0,
        y = 0,
        xend = circular::rho.circular(abs_rads)*cos(circular::mean.circular(abs_rads)),
        yend = circular::rho.circular(abs_rads)*sin(circular::mean.circular(abs_rads))
        ),
    size = deparse(substitute(size)),
    colour = deparse(substitute(color)),
    arrow = arrow(length = unit(deparse(substitute(arrow_head_cm)),"cm"))
    )

  return(s)
}

