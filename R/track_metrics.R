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
#' @seealso [straightness_index()] for a whole `TrajSet`; [path_tortuosity()].
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
#' @seealso [tortuosity_ratio()] for a whole `TrajSet`; [path_straightness()].
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

# Apply a per-trajectory scalar metric `fun(x, y)` to each trajectory in `ts`,
# ordering each trajectory's points by the time column when one is recorded.
# Returns a data.frame with the id column and a `value_name` column.
.trajectory_metric <- function(ts, x_col, y_col, value_name, fun) {
  if (!methods::is(ts, "TrajSet"))
    stop("'ts' must be a TrajSet.")
  d   <- ts@data
  idc <- ts@cols$id
  tc  <- ts@cols$time
  for (cc in c(idc, x_col, y_col)) {
    if (is.null(cc) || !cc %in% names(d))
      stop("column '", cc, "' not found in the TrajSet.")
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

#' Per-trajectory straightness index for a TrajSet
#'
#' Computes [path_straightness()] for each trajectory in a `TrajSet`, ordering
#' each trajectory's points by its time column when one is recorded.
#'
#' @param ts A `TrajSet`.
#' @param x_col,y_col Names of the coordinate columns to use. Default to the
#'   `TrajSet`'s recorded x/y columns (the real arena positions), so the metric
#'   reflects the physical path rather than any display transform.
#' @return A `data.frame` with one row per trajectory: the `TrajSet`'s id column
#'   and a numeric `straightness` column.
#' @seealso [path_straightness()], [tortuosity_ratio()]
#' @export
straightness_index <- function(ts, x_col = ts@cols$x, y_col = ts@cols$y) {
  .trajectory_metric(ts, x_col, y_col, "straightness", path_straightness)
}

#' Per-trajectory tortuosity ratio for a TrajSet
#'
#' Computes [path_tortuosity()] for each trajectory in a `TrajSet`, ordering
#' each trajectory's points by its time column when one is recorded.
#'
#' @param ts A `TrajSet`.
#' @param x_col,y_col Names of the coordinate columns to use. Default to the
#'   `TrajSet`'s recorded x/y columns (the real arena positions), so the metric
#'   reflects the physical path rather than any display transform.
#' @return A `data.frame` with one row per trajectory: the `TrajSet`'s id column
#'   and a numeric `tortuosity` column (`>= 1`, possibly `Inf`).
#' @seealso [path_tortuosity()], [straightness_index()]
#' @export
tortuosity_ratio <- function(ts, x_col = ts@cols$x, y_col = ts@cols$y) {
  .trajectory_metric(ts, x_col, y_col, "tortuosity", path_tortuosity)
}
