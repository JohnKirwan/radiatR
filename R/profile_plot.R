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

# Centered partial-window moving average of `value`, computed within each track
# (`id`) in `order_key` (time) order and returned aligned to the input order. No
# external dependency; `na.rm` tolerates the NA first row of speed/turning. Even
# windows floor to the next-smaller odd span.
.smooth_profile <- function(value, id, order_key, window) {
  if (window <= 1L) return(value)
  k   <- (window - 1L) %/% 2L
  out <- value
  for (g in unique(id)) {
    rows <- which(id == g)
    ord  <- rows[order(order_key[rows])]
    v    <- value[ord]
    n    <- length(v)
    out[ord] <- vapply(seq_len(n), function(i)
      mean(v[max(1L, i - k):min(n, i + k)], na.rm = TRUE), numeric(1))
  }
  out
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
#' distance calibration ([set_distance_scale()]) speed is in physical units. The
#' speed axis is robustly clipped by default (see `max_speed`).
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
#' @param colour_by,facets Optional column names of `as.data.frame(ts)` to
#'   colour by / facet into panels. Default: one neutral series per track.
#' @param max_speed For `metric = "speed"`, the speed-axis cap: `NULL` (default,
#'   the 99.5% quantile -- so single-frame tracking artifacts do not crush the
#'   plot), a positive number (hard cap), or `Inf` (no clip). Off-scale points
#'   are reported in a caption. Ignored for `"turning"`/`"direction"`.
#' @param smooth Integer window, in observations, for a centered per-track moving
#'   average of the `"speed"`/`"turning"` series. `1` (default) leaves the raw
#'   series unchanged; larger values smooth out per-frame jitter. Ignored for
#'   `"direction"` (a circular metric).
#' @param show_raw Logical; when `TRUE` and `smooth > 1`, draw the raw series as a
#'   faint line beneath the smoothed line. Default `FALSE`.
#' @return A `ggplot2` object.
#' @seealso [instantaneous_speed()], [angular_velocity()], [velocity_angle()],
#'   [plot_speed_direction()], [elapsed_seconds()], [radiate()], [set_frame_rate()]
#' @export
#' @examples
#' ts <- set_frame_rate(cpunctatus, 30)
#' plot_profile(ts, metric = "speed")
#' plot_profile(ts, metric = "direction", units = "degrees")
plot_profile <- function(ts, metric = c("speed", "turning", "direction"),
                         units = c("radians", "degrees"),
                         colour_by = NULL, facets = NULL, max_speed = NULL,
                         smooth = 1L, show_raw = FALSE) {
  if (!methods::is(ts, "Tracks")) stop("'ts' must be a Tracks.")
  metric <- match.arg(metric)
  units  <- match.arg(units)
  if (!is.numeric(smooth) || length(smooth) != 1L || !is.finite(smooth) ||
      smooth < 1 || smooth != round(smooth))
    stop("`smooth` must be a single positive integer (window in points; 1 = no smoothing).")
  smooth <- as.integer(smooth)
  d <- as.data.frame(ts)
  for (cc in c(colour_by, facets)) {
    if (!is.null(cc) && !cc %in% names(d))
      stop("column '", cc, "' not found in the Tracks.")
  }
  el    <- elapsed_seconds(ts)                       # validates frame rate / POSIXct
  value <- switch(metric,
    speed     = instantaneous_speed(ts),
    turning   = angular_velocity(ts, units = units),
    direction = velocity_angle(ts, units = units))
  # Sliding-window smoothing (line metrics only; direction is circular). The
  # smoothed series feeds the plotted line and the robust speed clip, so the
  # off-scale caption reflects what is drawn; the raw series is kept for the
  # optional faint overlay.
  raw <- value
  if (smooth > 1L && metric != "direction")
    value <- .smooth_profile(value, d[[ts@cols$id]], el, window = smooth)
  df <- data.frame(.elapsed = el, .value = value, .raw = raw,
                   .id = d[[ts@cols$id]], stringsAsFactors = FALSE)
  if (!is.null(colour_by)) df[[colour_by]] <- d[[colour_by]]
  if (!is.null(facets))  df[[facets]]  <- d[[facets]]

  ylab <- switch(metric,
    speed     = .speed_ylab(ts),
    turning   = if (units == "degrees") "turning rate (deg/s)" else "turning rate (rad/s)",
    direction = if (units == "degrees") "direction (deg)" else "direction (rad)")

  pts <- metric == "direction"
  overlay_raw <- isTRUE(show_raw) && smooth > 1L && !pts
  base_aes <- if (is.null(colour_by))
    ggplot2::aes(x = .data$.elapsed, y = .data$.value, group = .data$.id)
  else
    ggplot2::aes(x = .data$.elapsed, y = .data$.value, group = .data$.id,
                 colour = .data[[colour_by]])
  g <- ggplot2::ggplot(df, base_aes)
  # Faint raw line under the bold smoothed line (added first so it sits beneath).
  if (overlay_raw)
    g <- g + ggplot2::geom_line(
      mapping = ggplot2::aes(x = .data$.elapsed, y = .data$.raw, group = .data$.id),
      colour = "grey70", alpha = 0.25, linewidth = 0.3, na.rm = TRUE,
      inherit.aes = FALSE)
  g <- g + if (pts) {
    if (is.null(colour_by))
      ggplot2::geom_point(colour = "grey20", size = 0.7, alpha = 0.6, na.rm = TRUE)
    else ggplot2::geom_point(size = 0.7, alpha = 0.6, na.rm = TRUE)
  } else {
    if (is.null(colour_by))
      ggplot2::geom_line(colour = "grey20", alpha = 0.7, na.rm = TRUE)
    else ggplot2::geom_line(alpha = 0.7, na.rm = TRUE)
  }

  cap <- NULL
  if (metric == "speed") {
    rl  <- .robust_speed_limit(value, max_speed)
    g   <- g + ggplot2::coord_cartesian(ylim = c(0, rl$limit))
    if (rl$n_off > 0)
      cap <- sprintf("%d point%s off-scale (max %.3g)",
                     rl$n_off, if (rl$n_off == 1L) "" else "s", rl$max)
  }

  g <- g +
    ggplot2::labs(x = "elapsed (s)", y = ylab, caption = cap,
                  colour = if (!is.null(colour_by)) colour_by else NULL) +
    ggplot2::theme_minimal()
  if (!is.null(facets))
    g <- g + ggplot2::facet_wrap(ggplot2::vars(.data[[facets]]))
  g
}

#' Speed-vs-direction scatter for a Tracks
#'
#' Plots each observation's instantaneous speed ([instantaneous_speed()]) against
#' its movement direction ([velocity_angle()]) -- the bivariate companion to the
#' time profiles of [plot_profile()]. Points, since direction is circular.
#'
#' The speed axis is robustly clipped by default (`max_speed = NULL` zooms to the
#' 99.5% quantile) so a few single-frame tracking artifacts do not crush the
#' display; a caption reports how many points are off-scale. The clip is a view
#' zoom (`coord_cartesian`), so no data are dropped. Set `max_speed` to a number
#' for a hard cap, or `Inf` for the full raw range.
#'
#' @param ts A `Tracks`. Needs a frame rate for frame-indexed time
#'   ([set_frame_rate()]); POSIXct time is used directly.
#' @param units Direction units: `"radians"` (default, `[0, 2*pi)`) or
#'   `"degrees"` (`[0, 360)`).
#' @param colour_by Optional column of `as.data.frame(ts)` to colour points by.
#' @param max_speed Speed-axis cap: `NULL` (default, 99.5% quantile), a positive
#'   number (hard cap), or `Inf` (no clip).
#' @return A `ggplot2` object.
#' @seealso [plot_profile()], [instantaneous_speed()], [velocity_angle()]
#' @export
#' @examples
#' ts <- set_frame_rate(cpunctatus, 30)
#' plot_speed_direction(ts)
plot_speed_direction <- function(ts, units = c("radians", "degrees"),
                                 colour_by = NULL, max_speed = NULL) {
  if (!methods::is(ts, "Tracks")) stop("'ts' must be a Tracks.")
  units <- match.arg(units)
  d <- as.data.frame(ts)
  if (!is.null(colour_by) && !colour_by %in% names(d))
    stop("column '", colour_by, "' not found in the Tracks.")

  spd <- instantaneous_speed(ts)
  dir <- velocity_angle(ts, units = units)
  df  <- data.frame(.speed = spd, .direction = dir,
                    .id = d[[ts@cols$id]], stringsAsFactors = FALSE)
  if (!is.null(colour_by)) df[[colour_by]] <- d[[colour_by]]

  rl   <- .robust_speed_limit(spd, max_speed)
  cap  <- if (rl$n_off > 0)
    sprintf("%d point%s off-scale (max %.3g)",
            rl$n_off, if (rl$n_off == 1L) "" else "s", rl$max) else NULL
  xlab <- if (units == "degrees") "direction (deg)" else "direction (rad)"

  if (is.null(colour_by)) {
    g <- ggplot2::ggplot(
      df, ggplot2::aes(x = .data$.direction, y = .data$.speed)) +
      ggplot2::geom_point(colour = "grey20", size = 0.7, alpha = 0.5, na.rm = TRUE)
  } else {
    g <- ggplot2::ggplot(
      df, ggplot2::aes(x = .data$.direction, y = .data$.speed,
                       colour = .data[[colour_by]])) +
      ggplot2::geom_point(size = 0.7, alpha = 0.5, na.rm = TRUE)
  }

  g +
    ggplot2::coord_cartesian(ylim = c(0, rl$limit)) +
    ggplot2::labs(x = xlab, y = .speed_ylab(ts), caption = cap,
                  colour = if (!is.null(colour_by)) colour_by else NULL) +
    ggplot2::theme_minimal()
}

#' Speed distribution histogram for a Tracks
#'
#' A pooled histogram of step speeds ([instantaneous_speed()]) across all tracks,
#' annotated with the pooled median and coefficient of variation (CV = sd / mean,
#' a movement-regularity descriptor). The companion distribution view to the
#' time/scatter plots of [plot_profile()] / [plot_speed_direction()].
#'
#' The speed axis is robustly clipped by default (`max_speed = NULL` zooms to the
#' 99.5% quantile) so a few single-frame tracking artifacts do not flatten the
#' bulk of the distribution; a caption reports how many steps are off-scale. The
#' clip is a view zoom (`coord_cartesian`), so no data are dropped. Set
#' `max_speed` to a number for a hard cap, or `Inf` for the full raw range.
#'
#' @param ts A `Tracks`. Needs a frame rate for frame-indexed time
#'   ([set_frame_rate()]); POSIXct time is used directly.
#' @param max_speed Speed-axis cap: `NULL` (default, 99.5% quantile), a positive
#'   number (hard cap), or `Inf` (no clip).
#' @param bins Number of bars across the (clipped) speed axis. Default `30`.
#' @return A `ggplot2` object.
#' @seealso [instantaneous_speed()], [plot_profile()], [plot_speed_direction()]
#' @export
#' @examples
#' ts <- set_frame_rate(cpunctatus, 30)
#' plot_speed_histogram(ts)
plot_speed_histogram <- function(ts, max_speed = NULL, bins = 30) {
  if (!methods::is(ts, "Tracks")) stop("'ts' must be a Tracks.")
  spd <- instantaneous_speed(ts)
  rl  <- .robust_speed_limit(spd, max_speed)
  fin <- spd[is.finite(spd)]
  sub <- sprintf("median %.3g | CV %.2f",
                 stats::median(fin), stats::sd(fin) / mean(fin))
  cap <- if (rl$n_off > 0)
    sprintf("%d point%s off-scale (max %.3g)",
            rl$n_off, if (rl$n_off == 1L) "" else "s", rl$max) else NULL

  ggplot2::ggplot(data.frame(.speed = spd),
                  ggplot2::aes(x = .data$.speed)) +
    ggplot2::geom_histogram(binwidth = rl$limit / bins, boundary = 0,
                            fill = "grey40", colour = "white", na.rm = TRUE) +
    ggplot2::coord_cartesian(xlim = c(0, rl$limit)) +
    ggplot2::labs(x = .speed_ylab(ts), y = "count",
                  subtitle = sub, caption = cap) +
    ggplot2::theme_minimal()
}
