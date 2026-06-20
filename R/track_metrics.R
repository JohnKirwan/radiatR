# Spatial track metrics derived from trajectory geometry (as opposed to the
# circular heading statistics in circular_statistics.R): the straightness index
# and its reciprocal, the tortuosity ratio.

#' Path straightness index for a single trajectory
#'
#' The straightness index is the net (start-to-end) displacement divided by the
#' total path length travelled. It ranges from 0 (a maximally convoluted path
#' that returns to its starting point) to 1 (a perfectly straight path), and is
#' the reciprocal of the tortuosity ratio ([path_tortuosity()]). Unlike that
#' ratio it is bounded and does not blow up when the net displacement is small.
#'
#' The index is scale-invariant: multiplying all coordinates by a constant
#' leaves it unchanged, so absolute or relative coordinates give the same value
#' (provided the transformation is a similarity, i.e. uniform in x and y).
#'
#' @param x,y Numeric vectors of ordered (in time) coordinates for one
#'   trajectory.
#' @return A single straightness value in `[0, 1]`, or `NA_real_` when fewer
#'   than two finite points are available or the path has zero length.
#' @seealso [straightness_index()] for a whole `Tracks`; [path_tortuosity()].
#' @export
#' @examples
#' path_straightness(x = c(0, 1, 2), y = c(0, 0, 0))   # straight -> 1
#' path_straightness(x = c(0, 1, 0), y = c(0, 1, 0))   # returns to start -> 0
path_straightness <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]
  y <- y[ok]
  n <- length(x)
  if (n < 2L) return(NA_real_)
  total <- sum(sqrt(diff(x)^2 + diff(y)^2))
  if (!is.finite(total) || total == 0) return(NA_real_)
  net <- sqrt((x[n] - x[1L])^2 + (y[n] - y[1L])^2)
  net / total
}

#' Tortuosity ratio for a single trajectory
#'
#' The (classic) tortuosity ratio is the total path length travelled divided by
#' the net (start-to-end) displacement -- the reciprocal of the straightness
#' index ([path_straightness()]). It ranges from 1 (a perfectly straight path)
#' upward; larger values indicate a more convoluted path.
#'
#' The ratio is unbounded: when a trajectory returns to its starting point the
#' net displacement is zero and the ratio is `Inf`. For a bounded alternative,
#' or when start and end points may coincide, use [path_straightness()].
#'
#' Like the straightness index it is scale-invariant.
#'
#' @param x,y Numeric vectors of ordered (in time) coordinates for one
#'   trajectory.
#' @return A single tortuosity value `>= 1`, `Inf` when the net displacement is
#'   zero, or `NA_real_` when fewer than two finite points are available or the
#'   path has zero length.
#' @seealso [tortuosity_ratio()] for a whole `Tracks`; [path_straightness()].
#' @export
#' @examples
#' path_tortuosity(x = c(0, 1, 2), y = c(0, 0, 0))     # straight -> 1
#' path_tortuosity(x = c(0, 0, 1), y = c(0, 1, 1))     # L-shaped -> sqrt(2)
path_tortuosity <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]
  y <- y[ok]
  n <- length(x)
  if (n < 2L) return(NA_real_)
  total <- sum(sqrt(diff(x)^2 + diff(y)^2))
  if (!is.finite(total) || total == 0) return(NA_real_)
  net <- sqrt((x[n] - x[1L])^2 + (y[n] - y[1L])^2)
  if (net == 0) return(Inf)
  total / net
}

# Total path length (sum of step distances) over the finite points; 0 for < 2 points.
.path_length <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]; y <- y[ok]
  if (length(x) < 2L) return(0)
  sum(sqrt(diff(x)^2 + diff(y)^2))
}

#' Per-trajectory path length for a Tracks
#'
#' Total distance travelled along each trajectory. With a distance calibration
#' set ([set_distance_scale()]) the result is in physical units; otherwise in the
#' units of the recorded `x`/`y` coordinates.
#'
#' @param ts A `Tracks`.
#' @param x_col,y_col Names of the coordinate columns. Default to the `Tracks`'s
#'   recorded x/y columns.
#' @return A `data.frame` with one row per trajectory: the id column and a
#'   numeric `length` column.
#' @seealso [set_distance_scale()], [track_speed()], [straightness_index()]
#' @export
track_length <- function(ts, x_col = ts@cols$x, y_col = ts@cols$y) {
  out <- .trajectory_metric(ts, x_col, y_col, "length", .path_length)
  out$length <- out$length * (distance_scale(ts) %||% 1)
  out
}

# Apply a per-trajectory scalar metric `fun(x, y)` to each trajectory in `ts`,
# ordering each trajectory's points by the time column when one is recorded.
# Returns a data.frame with the id column and a `value_name` column.
.trajectory_metric <- function(ts, x_col, y_col, value_name, fun) {
  if (!methods::is(ts, "Tracks"))
    stop("'ts' must be a Tracks.")
  d   <- ts@data
  idc <- ts@cols$id
  tc  <- ts@cols$time
  for (cc in c(idc, x_col, y_col)) {
    if (is.null(cc) || !cc %in% names(d))
      stop("column '", cc, "' not found in the Tracks.")
  }
  ids <- unique(d[[idc]])
  vals <- vapply(ids, function(i) {
    sub <- d[d[[idc]] == i, , drop = FALSE]
    if (!is.null(tc) && tc %in% names(sub))
      sub <- sub[order(sub[[tc]]), , drop = FALSE]
    fun(sub[[x_col]], sub[[y_col]])
  }, numeric(1L))
  out <- data.frame(ids, vals, stringsAsFactors = FALSE)
  names(out) <- c(idc, value_name)
  out
}

#' Per-trajectory straightness index for a Tracks
#'
#' Computes [path_straightness()] for each trajectory in a `Tracks`, ordering
#' each trajectory's points by its time column when one is recorded.
#'
#' @param ts A `Tracks`.
#' @param x_col,y_col Names of the coordinate columns to use. Default to the
#'   `Tracks`'s recorded x/y columns (the real recorded positions), so the metric
#'   reflects the physical path rather than any display transform.
#' @return A `data.frame` with one row per trajectory: the `Tracks`'s id column
#'   and a numeric `straightness` column.
#' @seealso [path_straightness()], [tortuosity_ratio()]
#' @export
straightness_index <- function(ts, x_col = ts@cols$x, y_col = ts@cols$y) {
  .trajectory_metric(ts, x_col, y_col, "straightness", path_straightness)
}

#' Per-trajectory tortuosity ratio for a Tracks
#'
#' Computes [path_tortuosity()] for each trajectory in a `Tracks`, ordering
#' each trajectory's points by its time column when one is recorded.
#'
#' @param ts A `Tracks`.
#' @param x_col,y_col Names of the coordinate columns to use. Default to the
#'   `Tracks`'s recorded x/y columns (the real recorded positions), so the metric
#'   reflects the physical path rather than any display transform.
#' @return A `data.frame` with one row per trajectory: the `Tracks`'s id column
#'   and a numeric `tortuosity` column (`>= 1`, possibly `Inf`).
#' @seealso [path_tortuosity()], [straightness_index()]
#' @export
tortuosity_ratio <- function(ts, x_col = ts@cols$x, y_col = ts@cols$y) {
  .trajectory_metric(ts, x_col, y_col, "tortuosity", path_tortuosity)
}

#' Per-step speed along a trajectory
#'
#' Speed of each step as straight-line step distance divided by the elapsed time
#' of that step: `sqrt(diff(x)^2 + diff(y)^2) / diff(seconds)`. The unit is the
#' distance unit of `x`/`y` per second; for radiatR's unit-arena coordinates that
#' is arena-units (radii) per second.
#'
#' @param x,y Numeric vectors of ordered (in time) coordinates for one trajectory.
#' @param seconds Numeric vector, the elapsed time of each point in seconds (same
#'   length as `x`/`y`).
#' @return A numeric vector of per-step speeds, length `length(x) - 1`. A step is
#'   `NA` when either endpoint is non-finite or its time increment is `<= 0`;
#'   `numeric(0)` when fewer than two points are given.
#' @seealso [track_speed()] for a whole `Tracks`; [elapsed_seconds()].
#' @export
#' @examples
#' step_speed(x = 0:3, y = rep(0, 4), seconds = (0:3) / 30)   # 30 units/s
step_speed <- function(x, y, seconds) {
  n <- length(x)
  if (n < 2L) return(numeric(0))
  dist <- sqrt(diff(x)^2 + diff(y)^2)
  dt   <- diff(seconds)
  spd  <- dist / dt
  bad  <- !is.finite(dt) | dt <= 0 |
          !is.finite(x[-n]) | !is.finite(x[-1L]) |
          !is.finite(y[-n]) | !is.finite(y[-1L])
  spd[bad] <- NA_real_
  spd
}

#' Per-trajectory speed for a Tracks, in real units
#'
#' Summarises each trajectory's speed (distance per second). Step speeds come
#' from [step_speed()] using the track's elapsed time from [elapsed_seconds()];
#' the per-track summary is the chosen `stat` of those step speeds.
#'
#' Numeric (frame) time requires a frame rate ([set_frame_rate()]); POSIXct time
#' is used directly. With the default coordinate columns the unit is arena-units
#' (radii) per second, because radiatR normalises trajectories to a unit arena.
#'
#' @param ts A `Tracks`.
#' @param stat Per-track reduction of the step speeds: `"mean"` (default),
#'   `"max"`, or `"median"`.
#' @param x_col,y_col Names of the coordinate columns. Default to the `Tracks`'s
#'   recorded x/y columns.
#' @return A `data.frame` with one row per trajectory: the id column and a
#'   numeric `speed` column (`NA` for tracks with fewer than two usable points).
#' @seealso [step_speed()], [elapsed_seconds()], [track_duration()],
#'   [straightness_index()]
#' @export
track_speed <- function(ts, stat = c("mean", "max", "median"),
                        x_col = ts@cols$x, y_col = ts@cols$y) {
  if (!methods::is(ts, "Tracks")) stop("'ts' must be a Tracks.")
  stat <- match.arg(stat)
  reduce <- switch(stat, mean = mean, max = max, median = stats::median)
  d   <- ts@data
  idc <- ts@cols$id
  tc  <- ts@cols$time
  for (cc in c(idc, x_col, y_col, tc)) {
    if (is.null(cc) || !cc %in% names(d))
      stop("column '", cc, "' not found in the Tracks.")
  }
  el  <- elapsed_seconds(ts)                 # validates frame rate / handles POSIXct
  scl <- distance_scale(ts) %||% 1
  ids <- unique(d[[idc]])
  vals <- vapply(ids, function(i) {
    sel <- d[[idc]] == i
    sub <- d[sel, , drop = FALSE]
    ord <- order(sub[[tc]])
    s   <- step_speed(sub[[x_col]][ord], sub[[y_col]][ord], el[sel][ord])
    s   <- s[is.finite(s)]
    if (!length(s)) NA_real_ else reduce(s) * scl
  }, numeric(1L))
  out <- data.frame(ids, vals, stringsAsFactors = FALSE)
  names(out) <- c(idc, "speed")
  out
}

#' Per-observation instantaneous speed for a Tracks
#'
#' Instantaneous speed at each point, aligned to the `Tracks`'s rows (the
#' per-observation sibling of [elapsed_seconds()]). Each point carries the speed
#' of the step that ends at it; the first point of every trajectory is `NA`.
#' Speeds come from [step_speed()] using the track's elapsed time from
#' [elapsed_seconds()].
#'
#' Numeric (frame) time requires a frame rate ([set_frame_rate()]); POSIXct time
#' is used directly. With the default coordinate columns the unit is arena-units
#' (radii) per second.
#'
#' @param ts A `Tracks`.
#' @param x_col,y_col Names of the coordinate columns. Default to the `Tracks`'s
#'   recorded x/y columns.
#' @return A numeric vector, one value per observation in `ts@data` order, `NA`
#'   at each trajectory's first point.
#' @seealso [step_speed()], [track_speed()], [elapsed_seconds()]
#' @export
instantaneous_speed <- function(ts, x_col = ts@cols$x, y_col = ts@cols$y) {
  if (!methods::is(ts, "Tracks")) stop("'ts' must be a Tracks.")
  d   <- ts@data
  idc <- ts@cols$id
  tc  <- ts@cols$time
  for (cc in c(idc, x_col, y_col, tc)) {
    if (is.null(cc) || !cc %in% names(d))
      stop("column '", cc, "' not found in the Tracks.")
  }
  el  <- elapsed_seconds(ts)                  # validates frame rate / handles POSIXct
  out <- rep(NA_real_, nrow(d))
  for (i in unique(d[[idc]])) {
    sel <- which(d[[idc]] == i)               # rows are time-ordered within id (validity)
    if (length(sel) >= 2L)
      out[sel[-1L]] <- step_speed(d[[x_col]][sel], d[[y_col]][sel], el[sel])
  }
  out * (distance_scale(ts) %||% 1)
}
