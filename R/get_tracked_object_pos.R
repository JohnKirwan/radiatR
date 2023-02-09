# get_tracked_object_pos
#
#' Make ggplot object of tracks radiating from circle centre. Accepts a data frame with x and y coordinates of the points, and optional grouping variables.
#'
#' @param trial_limits A data frame containing a row for each tracked point.
#' @param animal_track The number of columns used by grid_wrap.
#' @examples
#' utils::data(urchin_tracks)
#' radiate(urchin_tracks, ncols=3, group1 = "arc", group2 = "id")
#' @importFrom purrr map
#' @importFrom tibble tibble add_column
#' @export
#
get_tracked_object_pos <- function(
    trial_limits,animal_track,circ0=NULL,circ1=NULL,radius_criterion=NULL){
    #if(is.null(midpoint)){midpoint="midpoint"}
    if(is.null(radius_criterion)){radius_criterion="first_past"}
    if(is.null(circ0)){circ0=.1}
    if(is.null(circ1)){circ0=.2}

    num_trials = dim(trial_limits)[1] # count number of trials
    animal_track$y <- -animal_track$y # invert y so that runs from bottom up
    # make nested tibble of the sets
    trackz <- purrr::map(vector(length=num_trials), tibble::tibble)

    i <- 1 # initialize i
    while (i <= dim(trial_limits)[1]){    # for each trial in a video

      animal_track$trial_num[ animal_track$frame >= trial_limits$first_f[i] &
                                animal_track$frame <= trial_limits$last_f[i]] <- i # get the trial no for each obs
      XY <- tibble::as_tibble(animal_track[animal_track$frame >= trial_limits$first_f[i] &
                                     animal_track$frame <= trial_limits$last_f[i],]) # make a tibble of that trial
      XY |> tibble::add_column(trans_x=0,trans_y=0,abs_theta=0,trans_rho=0,video=NA,
                         order=NA,vid_ord=NA,rel_theta=0) -> XY
      XY[,5:6] <- sweep(XY[,2:3],2,as.numeric(trial_limits[i,2:3]),"-") # subtract origin
      XY[,5:6] <- XY[,5:6] / trial_limits$r_px[i]
      # get theta from Cartesian coordinates
      abs_theta    <- mapply(atan2,XY$trans_y,XY$trans_x)
      # reorient abs theta to be clockwise and with a zero at the top
      XY$abs_theta <- mapply(rad2clock,abs_theta)

      # looks like this line below should apply to rel_theta
      XY$trans_rho <- unlist(sqrt(XY[,5]^2 + XY[,6]^2)) #/as.numeric(trial_limits$r_px[i]) #normalised rho
      XY$video <- trial_limits$video[i]
      XY$order <- as.character(i) # order of trials in video

      XY |> tidyr::unite("vid_ord", video:order,remove = FALSE, sep="_") -> XY
      stim_theta <- rad2clock(trial_limits$stim_theta[i])
      # reorient stim_theta and get relative position
      XY$rel_theta <- XY$abs_theta - stim_theta
      XY$rel_theta <- mapply(rad_shepherd_clock,XY$rel_theta) # herd back into clock
      XY$rel_theta <- mapply(rad_unclock,XY$rel_theta)# get back between -pi and pi
      # get absolute theta values back too
      XY$abs_theta <- mapply(rad_unclock,XY$abs_theta) # return to unit circle
      XY$rel_x     <- XY$trans_rho*cos(XY$rel_theta)
      XY$rel_y     <- XY$trans_rho*sin(XY$rel_theta)
      trackz[[i]]  <- XY
      # assign tracks less than 40% of arena to be empty
      if(min(XY$trans_rho>.4)){trackz[[i]] <- "Track starts too far from centre"
      warning(paste('Track starts too far from centre:'),XY$video[1])}
      if(any(XY$trans_rho> 1)){
        warning(paste('Track exceeds arena width: '),XY$video[1])}

      trial_limits$order[i]   <- as.character(i)
      trial_limits$vid_ord[i] <- paste0(trial_limits[i,12],"_",i)


      if(radius_criterion=="first_past"){ # if taking 1st point outside radii
        if(!any(XY$trans_rho > circ0)){warning('NO PTS BEYOND INNER CIRC!!')}
        trial_limits$x0[i] <- XY$trans_x[XY$trans_rho >= circ0][1] #get 1st pts
        trial_limits$y0[i] <- XY$trans_y[XY$trans_rho >= circ0][1] #beyond inner circ
        # if pts both sides of outer circ
        if(any(XY$trans_rho <= circ1) & any(XY$trans_rho > circ1)){
          trial_limits$x1[i] <- XY$trans_x[XY$trans_rho >= circ1][1]
          trial_limits$y1[i] <- XY$trans_y[XY$trans_rho >= circ1][1]
        }else if(!any(XY$trans_rho > circ1)){warning("NO POINT BEYOND OUTER RADIUS!!!")
          trial_limits$x1[i] <- XY$trans_x[which.max(XY$trans_rho)] # get most distant point
          trial_limits$y1[i] <- XY$trans_y[which.max(XY$trans_rho)] # get most distant point
        }else{warning("NO POINT WITHIN OUTER RADIUS!!!")
          trial_limits$x1[i] <- XY$trans_x[i+1] # take the 2nd points outside
          trial_limits$y1[i] <- XY$trans_y[i+1] # as x0,y0 will be 1st
        }
      }else{
        trial_limits$x0[i] <- XY$trans_x[which.min(abs(XY$trans_rho - circ0))]
        trial_limits$y0[i] <- XY$trans_y[which.min(abs(XY$trans_rho - circ0))]
        trial_limits$x1[i] <- XY$trans_x[which.min(abs(XY$trans_rho - circ1))] #.5
        trial_limits$y1[i] <- XY$trans_y[which.min(abs(XY$trans_rho - circ1))]
      }

      i = i + 1
    }
    return(list(trackz,trial_limits)) # norm to radius
  }


