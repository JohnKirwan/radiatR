# get_trial_limits
#
#' Make ggplot object of tracks radiating from circle centre. Accepts a data frame with x and y coordinates of the points, and optional grouping variables.
#'
#' @param landmarks A data frame containing a row for each tracked point.
#' @param animal_track The number of columns used by grid_wrap.
#' @param file_tbl Inner circle drawn.
#' @param vid_num Outer circle drawn.
#' @return A .
#' @examples
#' utils::data(urchin_tracks)
#' radiate(urchin_tracks, ncols=3, group1 = "arc", group2 = "id")
#' @importFrom rlang .data
#' @importFrom tibble as_tibble
#' @export
#
get_trial_limits <- function(
  landmarks,animal_track,file_tbl,vid_num,midpoint=NULL){
  if(is.null(midpoint)){midpoint="midpoint"}
  if(is.null(vid_num)){vid_num=1}

  num_trials = dim(landmarks)[1]/2 # count number of trials
  # pick out start frame and centre and stimulus coords
  tl <- tibble::as_tibble(array(0,dim=c(num_trials,18),
        dimnames = list(NULL, c("first_f","orig_x","orig_y","stim_x","stim_y",
        "last_f","stim_x_0","stim_y_0","stim_theta","r_px","quadrant","video",
         "order","vid_ord","x0","y0","x1","y1"))),  .name_repair="unique")
    tl$video <- as.character(tl$video)
    landmarks$y <- - landmarks$y # invert y axis to read from bottom to top

    i = 1
    while (i <= num_trials){ # for each trial
      tl$first_f[i]<- as.numeric(landmarks[(i*2)-1,1])# trial i start frame
      tl$orig_x[i] <- as.numeric(landmarks[(i*2)-1,2])# trial i centre pt x,y
      tl$orig_y[i] <- as.numeric(landmarks[(i*2)-1,3])# trial i centre pt x,y
      tl$stim_x[i] <- as.numeric(landmarks[i*2,2])# trial i stim centre x,y
      tl$stim_y[i] <- as.numeric(landmarks[i*2,3])# trial i stim centre x,y
      if (i < num_trials){
        tl$last_f[i] <- as.numeric(landmarks[((i+1)*2)-1,1]-1)# trial i last frame
      } else { tl$last_f[i]  <- max(animal_track[,1]) } # max along dimension 1
      i = i + 1
    }

    tl$video    <- file_tbl[[vid_num,1]]
    tl$arc      <- file_tbl$arc[file_tbl$basename==tl$video]
    tl$type     <- file_tbl$type[file_tbl$basename==tl$video]
    tl$obstacle <- file_tbl$obstacle[file_tbl$basename==tl$video]
    tl$id       <- file_tbl$id[file_tbl$basename==tl$video]

    tl$stim_x_0 <- tl$stim_x - tl$orig_x # stimulus cart coords - orig
    tl$stim_y_0 <- tl$stim_y - tl$orig_y
    tl$stim_theta <- atan2(tl$stim_y_0,tl$stim_x_0)
    tl$r_px     <- sqrt((tl$stim_x_0)^2+(tl$stim_y_0)^2) # pixel radius
    # if the stimulus centre is set to the midpoint, shift all stimulus positions accordingly
    if(midpoint=="midpoint" & tl$type[1]=="Herm"){    # shift .5 T clockwise
      # subtracting angles only works in +ve space
      stim_theta    <- mapply(rad2clock, tl$stim_theta) # change to clock
      stim_theta    <- stim_theta - tl$arc*(pi/360) # add T/2 to shift clockwise
      stim_theta    <- mapply(rad_shepherd_clock,stim_theta) #herd into 0 to 2*pi
      tl$stim_theta <- mapply(rad_unclock,stim_theta) # back to unit circle
    }
    ##### MAY NEED TO REMAKE THESE BELOW!
    #tl$stim_x_0 <- tl$stim_x_0 / tl$r_px # standardize by the radius
    #tl$stim_y_0 <- tl$stim_y_0 / tl$r_px # standardize by the radius
    tl$stim_x_0  <- cos(tl$stim_theta)
    tl$stim_y_0  <- sin(tl$stim_theta)

    i=1
    while (i <= num_trials){
      if (tl$stim_theta[i] > .25*pi & tl$stim_theta[i] <= .75*pi){
        tl$quadrant[i] = "top" #if stimulus in first quadrant
      }else if (tl$stim_theta[i]>= -.25*pi & tl$stim_theta[i] < .25*pi){
        tl$quadrant[i] = "left"
      }else if (tl$stim_theta[i] >= -.75*pi & tl$stim_theta[i] < -.25*pi ){
        tl$quadrant[i] = "bottom"
      }else {
        tl$quadrant[i] <- "right" }
      i = i + 1
    }

  return(tl)
  }
