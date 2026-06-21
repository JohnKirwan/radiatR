#' Kinematics profile plot for a Tracks
#'
#' Draws a per-observation kinematics metric against elapsed time, one line per
#' trajectory -- the non-circular companion to [radiate()]. `metric = "speed"`
#' plots [instantaneous_speed()]; `metric = "turning"` plots [angular_velocity()].
#'
#' Speed and turning rate are per-second, so a frame rate is required for
#' frame-indexed time ([set_frame_rate()]); POSIXct time is used directly. With a
#' distance calibration ([set_distance_scale()]) speed is in physical units.
#'
#' @param ts A `Tracks`.
#' @param metric `"speed"` (default) or `"turning"`.
#' @param units For `metric = "turning"`, `"radians"` (default) or `"degrees"`
#'   per second.
#' @param colour_by,panel_by Optional column names of `as.data.frame(ts)` to
#'   colour the lines by / facet into panels. Default: one neutral line per track.
#' @return A `ggplot2` object.
#' @seealso [instantaneous_speed()], [angular_velocity()], [elapsed_seconds()],
#'   [radiate()], [set_frame_rate()]
#' @export
#' @examples
#' ts <- set_frame_rate(cpunctatus, 30)
#' plot_profile(ts, metric = "speed")
plot_profile <- function(ts, metric = c("speed", "turning"),
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
    speed   = instantaneous_speed(ts),
    turning = angular_velocity(ts, units = units))
  df <- data.frame(.elapsed = el, .value = value,
                   .id = d[[ts@cols$id]], stringsAsFactors = FALSE)
  if (!is.null(colour_by)) df[[colour_by]] <- d[[colour_by]]
  if (!is.null(panel_by))  df[[panel_by]]  <- d[[panel_by]]

  ylab <- if (metric == "speed") {
    u <- distance_unit(ts)
    if (!is.null(distance_scale(ts)) && !is.null(u)) paste0("speed (", u, "/s)")
    else "speed (units/s)"
  } else {
    if (units == "degrees") "turning rate (deg/s)" else "turning rate (rad/s)"
  }

  if (is.null(colour_by)) {
    g <- ggplot2::ggplot(
      df,
      ggplot2::aes(x = .data$.elapsed, y = .data$.value, group = .data$.id)
    ) +
      ggplot2::geom_line(colour = "grey20", alpha = 0.7, na.rm = TRUE)
  } else {
    g <- ggplot2::ggplot(
      df,
      ggplot2::aes(x = .data$.elapsed, y = .data$.value,
                   group = .data$.id, colour = .data[[colour_by]])
    ) +
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
