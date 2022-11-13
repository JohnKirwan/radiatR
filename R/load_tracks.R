# radiate
#
#' Make ggplot object of tracks radiating from circle centre. Accepts a data frame with x and y coordinates of the points, and optional grouping variables.
#'
#' @param file_tbl .
#' @param df .
#' @param track_dir The folder with the tracks.
#' @examples
#' utils::data(urchin_tracks)
#' radiate(urchin_tracks, ncols=3, group1 = "arc", group2 = "id")
#' @importFrom rlang .data
#' @export
#
load_tracks <- function(file_tbl, df, track_dir){

if(any(!file_tbl$basename %in% df$file)){warning("Missing file")}
if(any(!df$file %in% file_tbl$basename)){warning("Missing file")}

file_tbl$arc  <- rep(0,dim(file_tbl)[1])
file_tbl$type <- as.character(rep("",dim(file_tbl)[1]))
file_tbl$obstacle <- as.character(rep("",dim(file_tbl)[1]))
file_tbl$id       <- as.character(rep("",dim(file_tbl)[1]))

i=1
while(i <= dim(df)[1]){
  file_tbl$arc [which(file_tbl$basename==df$file[i])] <- df$arc[i]
  file_tbl$type[which(file_tbl$basename==df$file[i])] <- df$type[i]
  file_tbl$obstacle[which(file_tbl$basename==df$file[i])] <- df$obstacle[i]
  file_tbl$id[which(file_tbl$basename==df$file[i])] <- df$id[i]
  i=i+1
}

if(any(!file_tbl$track %in% list.files(normalizePath(track_dir),recursive = TRUE))){
  stop(print(paste0("The following track file is missing:",
                    file_tbl$track[which(!file_tbl$track %in% list.files(
                      normalizePath(track_dir),recursive = TRUE))])))               }

if(any(!list.files(normalizePath(track_dir),
                   recursive = TRUE)[grep('*point02.txt$',
  list.files(normalizePath(track_dir),recursive = TRUE))] %in% file_tbl$track)){
  stop(print(paste0("The following track file lacks a counterpart:",
  list.files(normalizePath(track_dir),recursive = TRUE)[grep('*point02.txt$',
  list.files(normalizePath(track_dir),recursive = TRUE))])))                   }

return(file_tbl)
}
