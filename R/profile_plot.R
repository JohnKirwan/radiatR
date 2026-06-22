# Robust upper limit for a speed axis + count of off-scale values, so a few
# single-frame tracking artifacts do not crush the display. max_speed: NULL ->
# the 99.5% quantile; a finite positive number -> that hard cap; Inf -> the data
# max (no effective clip). Applied by callers via coord_cartesian (a view zoom
# that keeps all data rows).
.robust_speed_limit <- function(value, max_speed = NULL) {
  if (!is.null(max_speed)) {
    ok <- length(max_speed) == 1L && is.numeric(max_speed) &&
      !is.na(max_speed) && (is.infinite(max_speed) || max_speed > 0)
    if (!ok)
      stop("`max_speed` must be NULL, a positive number, or Inf.", call. = FALSE)
  }
  mx <- max(value, na.rm = TRUE)
  limit <- if (is.null(max_speed))
    as.numeric(stats::quantile(value, 0.995, na.rm = TRUE))
  else if (is.infinite(max_speed)) mx
  else max_speed
  list(limit = limit, n_off = sum(value > limit, na.rm = TRUE), max = mx)
}

# The speed y-label: physical units when a distance scale + unit are set,
# otherwise generic coordinate units per second. Shared by plot_profile and
# plot_speed_direction.
.speed_ylab <- function(ts) {
  u <- distance_unit(ts)
  if (!is.null(distance_scale(ts)) && !is.null(u)) paste0("speed (", u, "/s)")
  else "speed (units/s)"
}

#' Kinematics profile plot for a Tracks
#'
#' Draws a per-observation kinematics metric against elapsed time -- the
#' non-circular companion to [radiate()]. `metric = "speed"` plots
#' [instantaneous_speed()] (one line per track); `metric = "turning"` plots
#' [angular_velocity()] (one line per track); `metric = "direction"` plots
#' [velocity_angle()] (the movement direction) as points.
#'
#' Speed and turning rate are per-second, so a frame rate is required for
#' frame-indexed time ([set_frame_rate()]); POSIXct time is used directly. With a
#' distance calibration ([set_distance_scale()]) speed is in physical units.
#'
#' Direction is circular: it is drawn as points (not a line) because a line would
#' draw a false vertical bar across the 0/2*pi seam. A rotating track shows points
#' spread across the range, and values near 0/2*pi (0/360 degrees) appear at both
#' extremes -- the honest representation.
#'
#' @param ts A `Tracks`.
#' @param metric `"speed"` (default), `"turning"`, or `"direction"`.
#' @param units For `metric = "turning"` (per second) or `metric = "direction"`,
#'   `"radians"` (default) or `"degrees"`.
#' @param colour_by,panel_by Optional column names of `as.data.frame(ts)` to
#'   colour by / facet into panels. Default: one neutral series per track.
#' @return A `ggplot2` object.
#' @seealso [instantaneous_speed()], [angular_velocity()], [velocity_angle()],
#'   [elapsed_seconds()], [radiate()], [set_frame_rate()]
#' @export
#' @examples
#' ts <- set_frame_rate(cpunctatus, 30)
#' plot_profile(ts, metric = "speed")
#' plot_profile(ts, metric = "direction", units = "degrees")
plot_profile <- function(ts, metric = c("speed", "turning", "direction"),
                         units = c("radians", "degrees"),
                         colour_by = NULL, panel_by = NULL) {
  if (!methods::is(ts, "Tracks")) stop("'ts' must be a Tracks.")
  metric <- match.arg(metric)
  units  <- match.arg(units)
  d <- as.data.frame(ts)
  for (cc in c(colour_by, panel_by)) {
    if (!is.null(cc) && !cc %in% names(d))
      stop("column '", cc, "' not found in the Tracks.")
  }
  el    <- elapsed_seconds(ts)                       # validates frame rate / POSIXct
  value <- switch(metric,
    speed     = instantaneous_speed(ts),
    turning   = angular_velocity(ts, units = units),
    direction = velocity_angle(ts, units = units))
  df <- data.frame(.elapsed = el, .value = value,
                   .id = d[[ts@cols$id]], stringsAsFactors = FALSE)
  if (!is.null(colour_by)) df[[colour_by]] <- d[[colour_by]]
  if (!is.null(panel_by))  df[[panel_by]]  <- d[[panel_by]]

  ylab <- switch(metric,
    speed = {
      u <- distance_unit(ts)
      if (!is.null(distance_scale(ts)) && !is.null(u)) paste0("speed (", u, "/s)")
      else "speed (units/s)"
    },
    turning   = if (units == "degrees") "turning rate (deg/s)" else "turning rate (rad/s)",
    direction = if (units == "degrees") "direction (deg)" else "direction (rad)")

  pts <- metric == "direction"
  if (is.null(colour_by)) {
    g <- ggplot2::ggplot(
      df,
      ggplot2::aes(x = .data$.elapsed, y = .data$.value, group = .data$.id)
    )
    g <- g + if (pts)
      ggplot2::geom_point(colour = "grey20", size = 0.7, alpha = 0.6, na.rm = TRUE)
    else
      ggplot2::geom_line(colour = "grey20", alpha = 0.7, na.rm = TRUE)
  } else {
    g <- ggplot2::ggplot(
      df,
      ggplot2::aes(x = .data$.elapsed, y = .data$.value,
                   group = .data$.id, colour = .data[[colour_by]])
    )
    g <- g + if (pts)
      ggplot2::geom_point(size = 0.7, alpha = 0.6, na.rm = TRUE)
    else
      ggplot2::geom_line(alpha = 0.7, na.rm = TRUE)
  }

  g <- g +
    ggplot2::labs(x = "elapsed (s)", y = ylab,
                  colour = if (!is.null(colour_by)) colour_by else NULL) +
    ggplot2::theme_minimal()
  if (!is.null(panel_by))
    g <- g + ggplot2::facet_wrap(ggplot2::vars(.data[[panel_by]]))
  g
}
