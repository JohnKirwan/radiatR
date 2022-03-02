# import_info
#
#' Import landmark coordinates from text files
#'
#' @param filename The directory in which to look for the landmark files. Defaults to the current working directory.
#' @return A dataframe of file names.
#' #examples
#' #import_tracks(dir=data)
#' @importFrom utils read.csv
#' @export
#
import_info <- function(filename,...){
  df <- read.csv(file=filename,header=TRUE)
  x <- list(...)
  if(!is.null(x)){
    df$cond <- do.call(paste,list(df[,unlist(x)]))
  }
  if(any(!file_tbl$basename %in% df$file)){warning("Missing file")}
  if(any(!df$file %in% file_tbl$basename)){warning("Missing file")}

  return(x)
}



