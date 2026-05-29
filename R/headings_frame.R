#' Construct a headings frame from a data frame of angles
#'
#' Validates the angle column, optionally converts degrees to radians, and
#' marks the data frame with class \code{headings_frame} and attributes that
#' downstream functions (\code{stack_headings},
#' \code{add_stacked_headings}, \code{\link{radiate}}) will use as
#' defaults.
#'
#' @param data A data frame containing the angle column.
#' @param col Unquoted or quoted name of the angle column.
#' @param units Units of the angle column: \code{"radians"} or
#'   \code{"degrees"}. No default — must be specified. Values are converted
#'   to radians in place when \code{"degrees"}.
#' @param angle_convention \code{"unit_circle"} (0 = East, CCW, default) or
#'   \code{"clock"} (0 = North, CW).
#' @param coords \code{"absolute"} (default) or \code{"relative"}.
#'
#' @return A \code{data.frame} with additional class \code{"headings_frame"}
#'   and attributes \code{heading_col}, \code{angle_convention},
#'   \code{coords}.
#'
#' @seealso \code{stack_headings}, \code{add_stacked_headings},
#'   \code{\link{radiate}}
#' @importFrom rlang ensym as_string
#' @export
headings_frame <- function(data,
                           col,
                           units,
                           angle_convention = "unit_circle",
                           coords           = "absolute") {
  col_name <- rlang::as_string(rlang::ensym(col))
  if (!col_name %in% names(data))
    stop(sprintf("column '%s' not found in data.", col_name))
  if (missing(units))
    stop("'units' must be specified: use \"radians\" or \"degrees\".\n",
         "  Hint: most behavioral data is recorded in degrees.")
  units            <- match.arg(units, c("radians", "degrees"))
  angle_convention <- match.arg(angle_convention, c("unit_circle", "clock"))
  coords           <- match.arg(coords, c("absolute", "relative"))

  .check_angle_units(data[[col_name]], units, col_name)
  if (units == "degrees")
    data[[col_name]] <- data[[col_name]] * pi / 180

  attr(data, "angle_convention") <- angle_convention
  attr(data, "coords")           <- coords
  attr(data, "heading_col")      <- col_name
  class(data) <- c("headings_frame", "data.frame")
  data
}
