% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_circ.R
\name{add_circ}
\alias{add_circ}
\title{Create annotations for degrees at diagonals}
\usage{
add_circ(
  radius = 1,
  circle_color = "grey60",
  circle_alpha = 1,
  circle_size = 1
)
}
\arguments{
\item{radius}{radius between 0 and 1}

\item{circle_alpha}{alpha transparency}

\item{circle_size}{size}

\item{circle_colour}{description}
}
\value{
A geom_segment object of ticks
}
\description{
This function creates a list of annotation layers for degree marks at the diagonals.
The function takes one argument, a boolean value to specify if degree marks should be created or not.
}
\examples{
# Create degree annotations
ticks <- add_ticks(TRUE)

# Create a ggplot object
library(ggplot2)
p <- ggplot() + coord_polar()

# Add degree marks at diagonals
p_modified <- p + ticks

}
