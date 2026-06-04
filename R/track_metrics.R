# Spatial track metrics derived from trajectory geometry (as opposed to the
# circular heading statistics in circular_statistics.R). The straightness index
# is the first such metric; the tortuosity ratio (path length / net
# displacement) is a natural future addition, ideally measured over the segment
# up to a ring/perimeter crossing so the net displacement stays bounded.

#' Path straightness index for a single trajectory
#'
#' The straightness index is the net (start-to-end) displacement divided by the
#' total path length travelled. It ranges from 0 (a maximally convoluted path
#' that returns to its starting point) to 1 (a perfectly straight path), and is
#' the reciprocal of the tortuosity ratio (path length / net displacement).
#' Unlike that ratio it is bounded and does not blow up when the net
#' displacement is small.
#'
#' The index is scale-invariant: multiplying all coordinates by a constant
#' leaves it unchanged, so absolute or relative coordinates give the same value
#' (provided the transformation is a similarity, i.e. uniform in x and y).
#'
#' @param x,y Numeric vectors of ordered (in time) coordinates for one
#'   trajectory.
#' @return A single straightness value in `[0, 1]`, or `NA_real_` when fewer
#'   than two finite points are available or the path has zero length.
#' @seealso [straightness_index()] for a whole `TrajSet`.
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
#' @seealso [path_straightness()]
#' @export
straightness_index <- function(ts, x_col = ts@cols$x, y_col = ts@cols$y) {
  if (!methods::is(ts, "TrajSet"))
    stop("straightness_index: 'ts' must be a TrajSet.")
  d   <- ts@data
  idc <- ts@cols$id
  tc  <- ts@cols$time
  for (cc in c(idc, x_col, y_col)) {
    if (is.null(cc) || !cc %in% names(d))
      stop("straightness_index: column '", cc, "' not found in the TrajSet.")
  }
  ids <- unique(d[[idc]])
  vals <- vapply(ids, function(i) {
    sub <- d[d[[idc]] == i, , drop = FALSE]
    if (!is.null(tc) && tc %in% names(sub))
      sub <- sub[order(sub[[tc]]), , drop = FALSE]
    path_straightness(sub[[x_col]], sub[[y_col]])
  }, numeric(1L))
  out <- data.frame(ids, vals, stringsAsFactors = FALSE)
  names(out) <- c(idc, "straightness")
  out
}
