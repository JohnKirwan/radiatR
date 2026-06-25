# Periodic / temporal data -> unit-circle angle. The data->angle counterpart of
# the circumference_labs() / scale_clock() / scale_months() labelling layer.

# Unit-circle radians for a fraction `f` through the cycle, in the clock/calendar
# convention used by .scale_positions(): the cycle start sits at the top (pi/2)
# and time increases clockwise, so f and the matching label coincide.
.cycle_angle <- function(f) (pi / 2 - 2 * pi * f) %% (2 * pi)

#' Map periodic time, date, or numeric data onto circular angles
#'
#' Converts a vector of timestamps, dates, or a generic periodic numeric into
#' unit-circle angles in radians (`[0, 2*pi)`), so periodic data --- events by
#' time of day (chronobiology) or time of year (phenology) --- can be analysed
#' and drawn with the circular tools (`circ_summary()`, `vonmises_fit()`,
#' `radiate()`, the distribution overlays). It is the data-side counterpart of
#' the [circumference_labs()] / [scale_clock()] / [scale_months()] labels: the
#' cycle start is placed at the top and time increases clockwise, so a `06:00`
#' timestamp lands exactly on the `"6"` hours label and the 1st of each month on
#' its month label.
#'
#' @param x A `POSIXct`, `Date`, or `numeric` vector.
#' @param period The cycle. `"day"` (time of day; `x` must be `POSIXct`),
#'   `"year"` (time of year; `Date` or `POSIXct`, day-accurate and leap-safe), or
#'   a single positive number giving the cycle length in the units of a numeric
#'   `x`.
#' @return A `numeric` vector of angles in radians (`[0, 2*pi)`), the same length
#'   as `x`; `NA` entries propagate. Wrap it with [headings_frame()] (or use it as
#'   a `heading` column) to feed the circular tools.
#' @seealso [circumference_labs()], [scale_clock()], [scale_months()],
#'   [headings_frame()], [circ_summary()], [radiate()]
#' @export
#' @examples
#' # events through a day -> a clock-face circular distribution
#' set.seed(1)
#' times <- as.POSIXct("2020-06-01", tz = "UTC") +
#'   rnorm(200, mean = 9 * 3600, sd = 2 * 3600)        # clustered around 09:00
#' ang <- as_angle(times, "day")
#' circ_summarise(data.frame(heading = ang), "heading", units = "radians")
#'
#' # a generic numeric cycle of length 10
#' as_angle(c(0, 2.5, 5, 7.5), period = 10)
as_angle <- function(x, period) {
  # period
  bad_period <- function()
    stop("`period` must be \"day\", \"year\", or a single positive number.",
         call. = FALSE)
  if (is.character(period)) {
    if (length(period) != 1L || !period %in% c("day", "year")) bad_period()
  } else if (!(is.numeric(period) && length(period) == 1L &&
               is.finite(period) && period > 0)) {
    bad_period()
  }

  if (identical(period, "day")) {
    if (!inherits(x, "POSIXct"))
      stop("`period = \"day\"` needs POSIXct timestamps (a Date has no time of day).",
           call. = FALSE)
    lt <- as.POSIXlt(x)
    f  <- (lt$hour * 3600 + lt$min * 60 + lt$sec) / 86400
  } else if (identical(period, "year")) {
    if (!(inherits(x, "Date") || inherits(x, "POSIXct")))
      stop("`period = \"year\"` needs Date or POSIXct input.", call. = FALSE)
    yr   <- as.POSIXlt(x)$year + 1900L
    if (inherits(x, "Date")) {
      y0 <- as.numeric(as.Date(paste0(yr,     "-01-01")))
      y1 <- as.numeric(as.Date(paste0(yr + 1L, "-01-01")))
      xn <- as.numeric(x)
    } else {
      tz <- attr(x, "tzone"); if (is.null(tz) || !nzchar(tz)) tz <- "UTC"
      y0 <- as.numeric(as.POSIXct(paste0(yr,     "-01-01"), tz = tz))
      y1 <- as.numeric(as.POSIXct(paste0(yr + 1L, "-01-01"), tz = tz))
      xn <- as.numeric(x)
    }
    f <- (xn - y0) / (y1 - y0)
  } else {
    if (!is.numeric(x))
      stop("a numeric `period` needs a numeric `x`.", call. = FALSE)
    f <- (x %% period) / period
  }

  .cycle_angle(f)
}
