# import_tracks
#
#' Import landmark coordinates from text files
#'
#' @param dir The directory in which to look for the landmark files. Defaults to the current working directory.
#' @param landmark_suffix The suffix of files containing landmark coordinates.
#' @param track_suffix The suffix of files containing track coordinates.
#' @return A dataframe of file names.
#' @examples
#' import_tracks(dir=data)
#' @export
#
import_tracks <- function(dir,landmark_suffix=NULL,track_suffix=NULL){
  if(is.null(dir)){dir=getwd()}
  if(is.null(landmark_suffix)){landmark_suffix="_point01.txt"}
  if(is.null(track_suffix)){track_suffix="_point02.txt"}

  dir      <-  normalizePath(dir)
  filez    <- list.files(dir,recursive = TRUE)
  file_tbl <- tibble::tibble(basename=sub("*_point01.txt","",
                list.files(dir,recursive = TRUE)[
                  grep('*point01.txt$',list.files(dir,recursive=T))]))

  file_tbl |> transform(landmark=paste0(basename,"_point01.txt"),
                     track   =paste0(basename,"_point02.txt")) -> file_tbl
  return(file_tbl)
}



