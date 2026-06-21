# Perimeter (circumference) labelling for radial plots.
#
# A "scale" is a plain list describing how the circle's circumference is
# labelled: list(n, at, labels, name). `at`/`labels` are equal-length and may be
# shorter than `n` (draw `n` ticks, label a subset). Positions in `at` are raw
# unit-circle radians (0 = East, counterclockwise); perimeter_labs() rotates them
# through a circ_display so labels track the plotted data.

# Validate a scale spec; stop() on any structural problem, else return invisibly.
.check_scale <- function(scale) {
  if (!is.list(scale) || !all(c("n", "at", "labels") %in% names(scale)))
    stop("`scale` must be a list with `n`, `at`, and `labels`.", call. = FALSE)
  if (length(scale$n) != 1L || !is.numeric(scale$n) ||
      scale$n < 1 || scale$n != round(scale$n))
    stop("`scale$n` must be a positive integer scalar.", call. = FALSE)
  if (!is.numeric(scale$at))
    stop("`scale$at` must be a numeric vector of radians.", call. = FALSE)
  if (!is.character(scale$labels))
    stop("`scale$labels` must be a character vector.", call. = FALSE)
  if (length(scale$at) != length(scale$labels))
    stop("`scale$at` and `scale$labels` must have the same length.",
         call. = FALSE)
  invisible(scale)
}

# Raw unit-circle radian positions for 0-based division indices `k` of an
# `n_div`-division cycle: division 0 at the top (pi/2), increasing clockwise.
.scale_positions <- function(n_div, k) {
  (pi / 2 - 2 * pi * k / n_div) %% (2 * pi)
}

#' Perimeter scale: cardinal compass directions
#'
#' @param points `4` (N/E/S/W, default) or `8` (adds the intercardinals
#'   NE/SE/SW/NW).
#' @return A perimeter-scale list for [perimeter_labs()].
#' @seealso [perimeter_labs()], [scale_clock()], [scale_months()],
#'   [scale_seconds()]
#' @examples
#' scale_cardinal(points = 8)
#' @export
scale_cardinal <- function(points = 4) {
  if (!points %in% c(4, 8))
    stop("`points` must be 4 or 8.", call. = FALSE)
  labels <- if (points == 4)
    c("N", "E", "S", "W")
  else
    c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
  k <- seq_len(points) - 1L
  list(n = as.integer(points), at = .scale_positions(points, k),
       labels = labels, name = "cardinal")
}

#' Perimeter scale: clock hours
#'
#' Draws a tick at every hour and labels a sparse subset.
#'
#' @param hours `24` (default) or `12`.
#' @param every Label every `every` hours. `NULL` (default) auto-picks `6` for a
#'   24-hour dial and `3` for a 12-hour dial. Must evenly divide `hours`.
#' @return A perimeter-scale list for [perimeter_labs()].
#' @seealso [perimeter_labs()], [scale_cardinal()], [scale_months()],
#'   [scale_seconds()]
#' @examples
#' scale_clock(hours = 12)
#' @export
scale_clock <- function(hours = 24, every = NULL) {
  if (!hours %in% c(12, 24))
    stop("`hours` must be 12 or 24.", call. = FALSE)
  if (is.null(every)) every <- if (hours == 24) 6L else 3L
  if (hours %% every != 0)
    stop("`every` must evenly divide `hours`.", call. = FALSE)
  k <- seq(0L, hours - 1L, by = every)
  list(n = as.integer(hours), at = .scale_positions(hours, k),
       labels = as.character(k), name = "clock")
}

#' Perimeter scale: months of the year
#'
#' @param format `"abbr"` (Jan...Dec, default), `"initial"` (J, F, M, ...), or
#'   `"number"` (1...12).
#' @return A perimeter-scale list for [perimeter_labs()].
#' @seealso [perimeter_labs()], [scale_cardinal()], [scale_clock()],
#'   [scale_seconds()]
#' @examples
#' scale_months("initial")
#' @export
scale_months <- function(format = c("abbr", "initial", "number")) {
  format <- match.arg(format)
  labels <- switch(format,
                   abbr    = month.abb,
                   initial = substr(month.abb, 1, 1),
                   number  = as.character(1:12))
  k <- 0:11
  list(n = 12L, at = .scale_positions(12L, k), labels = labels,
       name = "months")
}

#' Perimeter scale: seconds (or minutes)
#'
#' Draws 60 ticks and labels a sparse subset. Also serves a minutes dial.
#'
#' @param every Label every `every` divisions (default `15`). Must evenly divide
#'   60.
#' @return A perimeter-scale list for [perimeter_labs()].
#' @seealso [perimeter_labs()], [scale_cardinal()], [scale_clock()],
#'   [scale_months()]
#' @examples
#' scale_seconds(every = 10)
#' @export
scale_seconds <- function(every = 15) {
  if (60 %% every != 0)
    stop("`every` must evenly divide 60.", call. = FALSE)
  k <- seq(0L, 59L, by = every)
  list(n = 60L, at = .scale_positions(60L, k),
       labels = as.character(k), name = "seconds")
}
