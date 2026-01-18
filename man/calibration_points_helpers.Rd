% Generated manually: calibration point helpers
\name{calibration_points_helpers}
\alias{calibration_points_tibble}
\alias{calibration_points_from_tibble}
\alias{write_calibration_points}
\alias{read_calibration_points}
\alias{calibration_from_points}
\title{Utilities for persisting calibration correspondences}
\usage{
calibration_points_tibble(image_points)

calibration_points_from_tibble(tbl)

write_calibration_points(image_points, path)

read_calibration_points(path)

calibration_from_points(
  board_dims,
  square_size,
  image_points,
  origin = c(0, 0),
  order = c("row_major", "col_major"),
  quiet = FALSE
)
}
\arguments{
\item{image_points}{Calibration correspondences provided as a list of 2-column
matrices, a data frame, or a CSV path (where supported).}

\item{tbl}{Data frame with columns \code{frame}, \code{x}, and \code{y} (and
optionally \code{corner}).}

\item{path}{File path for reading or writing calibration correspondence data
(CSV format).}

\item{board_dims}{Checkerboard dimensions supplied to
\code{checkerboard_points()}.}

\item{square_size}{Spacing between adjacent corners in world units.}

\item{origin}{World coordinate assigned to the corner at row 0, column 0.}

\item{order}{Traversal order of the corners.}

\item{quiet}{Suppress informational output.}
}
\value{
\code{calibration_points_tibble()} returns a data frame describing the image
points. \code{calibration_points_from_tibble()} and \code{read_calibration_points()}
return a list of matrices usable by \code{calibration_session()}.
\code{write_calibration_points()} invisibly returns \code{path}.
\code{calibration_from_points()} returns a structure matching the output of
\code{calibration_session()} but without the interactive capture step.
}
\description{
These helpers convert calibration correspondences between list and tabular
forms, persist them to disk, and compute camera calibration results using saved
points.
}
