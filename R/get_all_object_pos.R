# get_all_animal_pos
#
#' Make ggplot object of tracks radiating from circle centre. Accepts a data frame with x and y coordinates of the points, and optional grouping variables.
#'
#' @param landmarks .
#' @param animal_track .
#' @param file_tbl .
#' @examples
#' utils::data(urchin_tracks)
#' radiate(urchin_tracks, ncols=3, group1 = "arc", group2 = "id")
#' @export
#
get_all_object_pos <- function(landmarks,animal_track,file_tbl,track_dir){
  i <- 1 # for all files in file_tbl
  while(i <= dim(file_tbl)[1]){
    animal_track = read.delim(normalizePath(paste0(track_dir,'/',file_tbl$track[i])),
                              sep="\t", header=F)[,1:3]
    names(animal_track) <- c("frame","x","y")
    landmarks <- read.delim(normalizePath(
      paste0(track_dir,'/',file_tbl$landmark[i])),sep="\t", header=F)[,1:3]
    names(landmarks) <- c("frame","x","y")
    if(length(landmarks$x) %% 2 == 1){
      warning(paste("Odd number of landmarks:",file_tbl$basename[i])) }
    trial_limits <- get_trial_limits(landmarks,animal_track,file_tbl,i) #
    trial_track_list <- get_tracked_object_pos(trial_limits,animal_track) # run get_animal_pos
    if(i == 1){
      all_trackz       <- trial_track_list[[1]] # populate with first list
      trialz <- trial_track_list[[2]] # first instance from above
    } else {
      all_trackz       <- c(all_trackz,trial_track_list[[1]])
      trialz <- rbind(trialz,trial_track_list[[2]])
    }
    i <- i + 1
  }
  trackz_n_limits <- list(all_trackz,trialz)
  return(trackz_n_limits)
}
