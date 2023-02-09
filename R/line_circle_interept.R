# radiate
#
#' Make ggplot object of tracks radiating from circle centre. Accepts a data frame with x and y coordinates of the points, and optional grouping variables.
#'
#' @param x0 A data frame containing a row for each tracked point.
#' @param y0 The number of columns used by grid_wrap.
#' @param x1 Inner circle drawn.
#' @param y1 Outer circle drawn.
#' @return A ggplot2 graphical object.
#' @examples
#' utils::data(urchin_tracks)
#' radiate(urchin_tracks, ncols=3, group1 = "arc", group2 = "id")
#' @export
#
line_circle_intercept <- function(x0,y0,x1,y1){
  h = 0;k=0;radius=1 # circle origin x,y
  a = (x1 - x0)^2 + (y1 - y0)^2 # vector length
  b = (2*(x1 - x0)*(x0 - h)) + (2*(y1 - y0)*(y0 - k)) #
  c = (x0 - h)^2 + (y0 - k)^2 - radius^2
  D <- (b^2) - (4*a*c) # discriminant - must be positive
  if(a <= 0){warning("Vector length is not positive!")}
  if(D <= 0){warning(paste0(
    "Discriminant is not positive with x0=",x0," y0=",y0," x1=",x1," y1=",y1))}
  if(c >= 0){warning("Starting pt is outside radius!")}

  # the roots for t may be found in the usual quadratic form:
  t <- c(0,0)
  t[1] = (-b + sqrt(D)) / (2*a)
  t[2] = (-b - sqrt(D)) / (2*a)
  # 1st and 2nd rows of ints correspond to 1st and 2nd root respectively
  ints <- rbind(unlist(c( t[1]*(x1 - x0) + x0, t[1]*(y1 - y0) + y0)),
                unlist(c( t[2]*(x1 - x0) + x0, t[2]*(y1 - y0) + y0)))

  d <- c(sqrt(ints[1,1]^2 + ints[1,2]^2),sqrt(ints[2,1]^2 + ints[2,2]^2))
  if(all.equal(d[1],d[2])==FALSE){warning("Intersect is off unit circle!")}
  # 1st and 2nd rows of dist2track correspond to 1st and 2nd root respectively
  dist2track    <- c(0,0)
  dist2track[1] <- sqrt( (ints[1,1]-x1)^2 + (ints[1,2]-y1)^2 )
  dist2track[2] <- sqrt( (ints[2,1]-x1)^2 + (ints[2,2]-y1)^2 )
  xy <- ints[which.min(dist2track),]

  # commented because NAs crash this part
  #if(t[1] < 1 | t[2] > 0){ #which.min(dist2track)
  #  warning('Roots imply vector is weird') }
  return(tibble::tibble(x_int = xy[1],y_int = xy[2])) # return closest to outer pt
}
