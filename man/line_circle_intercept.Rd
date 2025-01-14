% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/line_circle_interept.R
\name{line_circle_intercept}
\alias{line_circle_intercept}
\title{Make ggplot object of tracks radiating from circle centre. Accepts a data frame with x and y coordinates of the points, and optional grouping variables.}
\usage{
line_circle_intercept(x0, y0, x1, y1)
}
\arguments{
\item{x0}{A data frame containing a row for each tracked point.}

\item{y0}{The number of columns used by grid_wrap.}

\item{x1}{Inner circle drawn.}

\item{y1}{Outer circle drawn.}
}
\value{
A ggplot2 graphical object.
}
\description{
Make ggplot object of tracks radiating from circle centre. Accepts a data frame with x and y coordinates of the points, and optional grouping variables.
}
\examples{
utils::data(urchin_tracks)
radiate(urchin_tracks, ncols=3, group1 = "arc", group2 = "id")
}
