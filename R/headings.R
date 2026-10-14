# Heading extraction rules and circular summaries for Tracks
#
# Provides:
#   - derive_headings(Tracks, rule=..., ...): per-trajectory (and optionally multi-event) headings
#   - circ_summary_headings(Tracks, rule=..., group_by=...): circular summaries (mean direction, resultant length, etc.)
#   - gg helpers to overlay mean direction arrows with length equal to resultant R on polar plots
#   - rule registry & user-defined rules
#
# Dependencies: circular, ggplot2 (for plotting helpers)

#' @importFrom methods setGeneric setMethod
#' @importFrom circular circular mean.circular rho.circular
#' @importFrom ggplot2 ggplot aes geom_segment geom_point

# ---- utilities ---------------------------------------------------------------
`%||%` <- function(a, b) if (!is.null(a)) a else b

# carry helper: add columns from nearest time per id. Each carried column is
# assigned as a whole vector (`data[[col]][j]`), so its class is preserved --
# factors keep their labels, POSIXct/Date stay dates -- rather than being coerced
# to storage codes by row-by-row `[i, col] <-` assignment into an NA-initialised
# column.
.carry_nearest <- function(res, data, idc, tc, cols) {
  if (is.null(cols) || !length(cols)) return(res)
  cols <- intersect(cols, names(data))
  if (!length(cols)) return(res)
  # A carried column must never replace one the rule produced.
  clash <- intersect(cols, names(res))
  if (length(clash))
    rlang::abort(
      sprintf("`carry` cannot name %s: the headings already have %s, which the carried values would overwrite. Rename the column in the tracks first.",
              paste0("'", clash, "'", collapse = ", "),
              if (length(clash) > 1L) "columns of those names" else "a column of that name"),
      class = "radiatR_invalid_argument", arg = "carry", columns = clash)
  idx_by_id <- split(seq_len(nrow(data)), data[[idc]])
  # nearest source row for each result row (NA when the id is absent)
  j <- vapply(seq_len(nrow(res)), function(i) {
    idx <- idx_by_id[[as.character(res$id[i])]]
    if (is.null(idx) || !length(idx)) return(NA_integer_)
    idx[which.min(abs(data[[tc]][idx] - res$time[i]))]
  }, integer(1))
  out <- res
  for (col in cols) out[[col]] <- data[[col]][j]   # class-preserving subset
  out
}

# linear interpolation of crossing point along segment P0->P1 for a given radius r*
.segment_cross <- function(x0, y0, x1, y1, rstar) {
  r0 <- sqrt(x0^2 + y0^2); r1 <- sqrt(x1^2 + y1^2)
  if (!is.finite(r0) || !is.finite(r1) || r0 == r1) return(c(NA_real_, NA_real_, NA_real_))
  t <- (rstar - r0) / (r1 - r0)
  if (t < 0 || t > 1) return(c(NA_real_, NA_real_, NA_real_))
  xc <- x0 + t * (x1 - x0)
  yc <- y0 + t * (y1 - y0)
  tc <- t
  c(xc, yc, tc)
}

# ---- generic -----------------------------------------------------------------
#' Derive heading angle(s) from trajectories using specified rule
#'
#' @param x Tracks
#' @param rule Required. A single rule name, or a function. Built-in rules:
#'   `"crossing"`, `"distal"`, `"straight"`, `"origin_mean"`, `"net"`,
#'   `"velocity_mean"`, `"velocity_axis"`, `"window_net"`, `"goal_bias"`,
#'   `"pca_axis"`, `"ransac_straight"`, `"maxspeed_window"`, `"vm_fit"`,
#'   `"exit"`, `"entry"`, `"ring_tangent"`; a name registered with
#'   [register_heading_rule()] (see [list_heading_rules()]); or a function
#'   called once per trajectory as `f(df, cols, ...)` that returns columns
#'   `id`, `time` and `heading`.
#' @param ... rule-specific options (see Details), including `return_coords`.
#'   A built-in rule stops with an error naming any option it does not read;
#'   registered and function rules receive `...` unchecked.
#' @param coords Character. Which Cartesian columns to use: `"absolute"`
#'   (default, uses whatever `x`/`y` the `Tracks` currently holds in
#'   `Tracks@@cols` -- calibrated only if the caller supplied or normalized
#'   them that way) or `"relative"` (uses `rel_x`/`rel_y`, the
#'   landmark-rotated frame; errors if not registered).
#' @param on_missing One of `"warn"` (default), `"error"`, or `"quiet"`,
#'   controlling what happens when a rule produces no heading (`NA`) for one or
#'   more trials. The `NA` rows are always retained; the returned object carries
#'   `n_total` and `n_missing` (headings, i.e. rows), `n_trials` (distinct
#'   trials) and `missing_ids` (the trials with at least one missing heading)
#'   attributes. `"warn"` emits a
#'   warning, `"error"` stops, `"quiet"` is silent. Rule-based failures are often
#'   non-random (e.g. tracks that never reach the circumference) and can bias
#'   circular statistics, so they are surfaced by default.
#' @details
#' Passing `return_coords = TRUE` (via `...`, default `FALSE`) attaches the
#' construction coordinates each rule used to derive the heading, in the chosen
#' `coords` frame: `crossing` adds `x_inner`/`y_inner`; `distal` adds
#' `x_distal`/`y_distal`; `net` adds `x_start`/`y_start`/`x_end`/`y_end`;
#' `straight` adds `x_seg0`/`y_seg0`/`x_seg1`/`y_seg1` (the run endpoints);
#' `pca_axis` adds `x_centroid`/`y_centroid`/`axis_x`/`axis_y` (a unit axis
#' vector). Other rules ignore it.
#' The `distal` rule accepts `max_radius` (default `Inf`): the furthest-point
#' search is restricted to positions with radius `<= max_radius`, so
#' `max_radius = 1` ignores beyond-circumference (`rho > 1`) tracking outliers.
#' The `straight` rule takes `tol` (default `pi/18`, the largest turn between
#' successive steps) and `min_len` (default `5`, the fewest steps in a run). A
#' step of zero length (a pause) has no direction: turns are measured between
#' successive moving steps, so a pause neither breaks a run nor counts toward
#' `min_len`. The heading is the direction of the run's first moving step.
#' `pca_axis` takes `source = "position"` (default; the principal axis of the
#' positions about their centroid) or `"velocity"` (the principal axis of the
#' steps about zero, which weights a step by its squared length; compare
#' `velocity_axis`, which weights by length).
#'
#' The rules that average directions take a weighting through \code{...}:
#' \itemize{
#'   \item \code{velocity_mean} and \code{vm_fit} average the directions of
#'     the steps of a track. \code{weight_by = "uniform"} (default) counts
#'     every step once; \code{"step_length"} weights a step by its length,
#'     which gives the direction from the first position to the last, as
#'     \code{net} does. The two rules return the same heading: the
#'     maximum-likelihood mean direction of a von Mises distribution is the
#'     mean direction.
#'   \item \code{velocity_axis} takes the same \code{weight_by}, with
#'     \code{"step_length"} as its default.
#'   \item \code{origin_mean} averages the bearings of the positions from
#'     the origin, weighted by radius to the power \code{r_power} (default
#'     \code{0}, every position counted once; \code{1} gives the bearing of
#'     the centroid).
#' }
#' A step of zero length and a position at the origin have no direction and
#' are left out of these averages, whatever the weighting. A track with no
#' direction left, or whose directions cancel, has heading \code{NA}.
#'
#' The other rules likewise give \code{NA} where a displacement has zero
#' length: a track that never moves (\code{net}, \code{straight},
#' \code{goal_bias}, \code{maxspeed_window}, \code{window_net}), one that ends
#' where it started (\code{net}), a window whose ends coincide
#' (\code{window_net}), positions with no spread, or steps none of which
#' moves (\code{pca_axis}), a \code{pca_axis} track whose net displacement has
#' no component along its axis (the axis then has no sign), and a track that
#' stays at the origin (\code{distal}). \code{window_net} and
#' \code{maxspeed_window} shrink their window to fit a track shorter than
#' \code{window_n}.
#' @return data.frame with columns id, time (approx), heading (radians, unit-circle
#'   convention), plus the rule-specific construction columns above when
#'   `return_coords = TRUE`. For some rules there may be multiple headings per id.
#' @examples
#' h <- derive_headings(cpunctatus, rule = "distal")
#' head(h)
#' # ignore tracking outliers beyond the circumference and keep the point used
#' head(derive_headings(cpunctatus, rule = "distal", max_radius = 1,
#'                      return_coords = TRUE))
#' # per-trial reference frame, carrying a condition column for grouping
#' head(derive_headings(cpunctatus, rule = "net", coords = "relative",
#'                      carry = "arc"))
#' @export
setGeneric(
  "derive_headings",
  function(
    x,
    rule,
    ...,
    coords = c("absolute", "relative")
  ) standardGeneric("derive_headings")
)

# ---- crossing rule -----------------------------------------------------------
# Finds successive crossings of two radii circ0 < circ1 (default outward) and
# projects the inner->outer crossing vector forward onto the unit circle (the
# circumference); the heading is the bearing of that boundary intersection.
# If multiple crossing pairs occur, returns one row per pair unless
# first_only=TRUE. When return_coords=TRUE, also returns x_inner/y_inner (inner
# circ0 crossing) and x_outer/y_outer (outer circ1 crossing).

# Forward intersection of the ray from (x0,y0) toward (x1,y1) with the unit
# circle: the inner->outer crossing vector projected to the circumference.
# (x0,y0) is inside the unit circle, so the forward root is unique and positive.
# Returns c(px, py), or c(NA, NA) when the direction is degenerate.
.crossing_boundary_point <- function(x0, y0, x1, y1) {
  dx <- x1 - x0; dy <- y1 - y0
  len <- sqrt(dx^2 + dy^2)
  if (!is.finite(len) || len == 0) return(c(NA_real_, NA_real_))
  ux <- dx / len; uy <- dy / len
  b    <- x0 * ux + y0 * uy
  disc <- b^2 - (x0^2 + y0^2 - 1)
  if (!is.finite(disc) || disc < 0) return(c(NA_real_, NA_real_))
  t <- -b + sqrt(disc)              # forward root: toward the periphery
  c(x0 + t * ux, y0 + t * uy)
}

.set_headings_crossing_one <- function(d, id, tc, xc, yc, circ0, circ1,
                                       direction = c("outward", "inward"),
                                       first_only = FALSE,
                                       return_coords = FALSE) {
  direction <- match.arg(direction)
  r <- sqrt(d[[xc]]^2 + d[[yc]]^2)
  n <- nrow(d)
  out <- list(); k <- 1L
  i <- 1L
  while (i < n) {
    if (direction == "outward") {
      cond0 <- r[ i ] <= circ0 & r[i+1] >  circ0
    } else {
      cond0 <- r[ i ] >= circ0 & r[i+1] <  circ0
    }
    if (cond0) {
      c0 <- .segment_cross(d[[xc]][i], d[[yc]][i], d[[xc]][i+1], d[[yc]][i+1], circ0)
      j <- i
      found1 <- FALSE
      while (j < n && !found1) {
        if (direction == "outward") found1 <- r[j] <= circ1 & r[j+1] > circ1 else found1 <- r[j] >= circ1 & r[j+1] < circ1
        if (!found1) j <- j + 1L
      }
      if (found1) {
        c1 <- .segment_cross(d[[xc]][j], d[[yc]][j], d[[xc]][j+1], d[[yc]][j+1], circ1)
        p  <- .crossing_boundary_point(c0[1], c0[2], c1[1], c1[2])
        heading <- atan2(p[2], p[1])
        row <- data.frame(id = d[[id]][i],
                          time = mean(c(d[[tc]][i] + c0[3] * (d[[tc]][i+1] - d[[tc]][i]),
                                        d[[tc]][j] + c1[3] * (d[[tc]][j+1] - d[[tc]][j]))),
                          heading = .wrap_to_2pi(heading))
        if (return_coords) {
          row$x_inner <- c0[1]
          row$y_inner <- c0[2]
          row$x_outer <- c1[1]
          row$y_outer <- c1[2]
        }
        out[[k]] <- row
        k <- k + 1L
        i <- j + 1L
        if (first_only) break
        next
      }
    }
    i <- i + 1L
  }
  if (length(out)) {
    do.call(rbind, out)
  } else {
    if (return_coords) {
      data.frame(id = d[[id]][1], time = d[[tc]][1], heading = NA_real_,
                 x_inner = NA_real_, y_inner = NA_real_,
                 x_outer = NA_real_, y_outer = NA_real_)
    } else {
      data.frame(id = d[[id]][1], time = d[[tc]][1], heading = NA_real_)
    }
  }
}

# Direction of a displacement, in [0, 2pi). One of zero length has no direction,
# though atan2(0, 0) is 0.
.displacement_direction <- function(dx, dy) {
  if (isTRUE(dx == 0 && dy == 0)) NA_real_ else .wrap_to_2pi(atan2(dy, dx))
}

# ---- distal rule ------------------------------------------------------------- -------------------------------------------------------------
# Heading = angle of the most distal point (max radius from origin)
.set_headings_distal_one <- function(d, id, tc, xc, yc, return_coords = FALSE,
                                     max_radius = Inf) {
  r <- sqrt(d[[xc]]^2 + d[[yc]]^2)
  ok <- which(is.finite(r) & r <= max_radius)
  if (!length(ok)) {
    row <- data.frame(id = d[[id]][1], time = d[[tc]][1], heading = NA_real_)
    if (return_coords) { row$x_distal <- NA_real_; row$y_distal <- NA_real_ }
    return(row)
  }
  m <- ok[which.max(r[ok])]
  row <- data.frame(id = d[[id]][m], time = d[[tc]][m],
                    heading = .displacement_direction(d[[xc]][m], d[[yc]][m]))
  if (return_coords) {
    row$x_distal <- d[[xc]][m]
    row$y_distal <- d[[yc]][m]
  }
  row
}

# ---- straight rule -----------------------------------------------------------
# Heading = direction of the first step of the longest run of moving steps whose
# successive turns are all <= tol. A pause (a step of zero length) has no
# direction: turns are measured between successive moving steps, so a pause
# neither breaks a run nor counts toward min_len (which counts moving steps).
.set_headings_straight_one <- function(d, id, tc, xc, yc, tol = pi/18, min_len = 5L,
                                       return_coords = FALSE) {
  na_row <- function() {
    row <- data.frame(id = d[[id]][1], time = d[[tc]][1], heading = NA_real_)
    if (return_coords) {
      row$x_seg0 <- NA_real_; row$y_seg0 <- NA_real_
      row$x_seg1 <- NA_real_; row$y_seg1 <- NA_real_
    }
    row
  }
  vx <- diff(d[[xc]]); vy <- diff(d[[yc]])
  mv <- which(vx != 0 | vy != 0)   # moving steps; step i joins points i and i + 1
  k  <- length(mv)
  if (k < min_len) return(na_row())
  ux <- vx[mv]; uy <- vy[mv]
  turn <- atan2(ux[-k] * uy[-1] - uy[-k] * ux[-1], ux[-k] * ux[-1] + uy[-k] * uy[-1])
  r <- rle(abs(turn) <= tol)
  ends <- cumsum(r$lengths)
  starts <- ends - r$lengths + 1L
  idx_ok <- which(r$values)
  if (!length(idx_ok)) return(na_row())
  lens <- r$lengths[idx_ok]
  j <- which.max(lens)
  if (lens[j] + 1L < min_len) return(na_row())   # m turns span m + 1 steps
  first <- mv[starts[idx_ok[j]]]                 # first moving step of the run
  p1    <- mv[ends[idx_ok[j]] + 1L] + 1L         # end point of its last step
  row <- data.frame(id = d[[id]][first], time = d[[tc]][floor((first + p1) / 2)],
                    heading = .wrap_to_2pi(atan2(vy[first], vx[first])))
  if (return_coords) {
    row$x_seg0 <- d[[xc]][first]; row$y_seg0 <- d[[yc]][first]
    row$x_seg1 <- d[[xc]][p1];    row$y_seg1 <- d[[yc]][p1]
  }
  row
}

# Weighted mean direction, in [0, 2pi). An angle whose weight is zero or not
# finite has no say: a step of zero length and a point at the origin have no
# direction, though atan2(0, 0) is 0. NA when no angle is left or the weighted
# unit vectors cancel. (circular::mean.circular() has no weights argument.)
.weighted_mean_direction <- function(th, w) {
  ok <- is.finite(th) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  s  <- sum(w[ok] * sin(th[ok]))
  co <- sum(w[ok] * cos(th[ok]))
  if (sqrt(s^2 + co^2) <= sum(w[ok]) * .Machine$double.eps) return(NA_real_)
  .wrap_to_2pi(atan2(s, co))
}

# Weights of the steps of a path: their length, or 1 for each step that moves.
.step_weights <- function(vx, vy, weight_by) {
  len <- sqrt(vx^2 + vy^2)
  if (weight_by == "step_length") len else as.numeric(len > 0)
}

# ---- origin_mean rule --------------------------------------------------------
# Heading = mean of position bearings from origin, weighted by radius^r_power
# (r_power = 1 gives the bearing of the centroid)
.set_headings_origin_mean_one <- function(d, id, tc, xc, yc, r_power = 0) {
  th <- atan2(d[[yc]], d[[xc]])
  r  <- sqrt(d[[xc]]^2 + d[[yc]]^2)
  w  <- ifelse(r > 0, r^r_power, 0)
  data.frame(id = d[[id]][1], time = stats::median(d[[tc]], na.rm = TRUE),
             heading = .weighted_mean_direction(th, w))
}

# ---- net rule ----------------------------------------------------------------
# Heading = angle from first to last point (net displacement)
.set_headings_net_one <- function(d, id, tc, xc, yc, return_coords = FALSE) {
  s <- 1L; e <- nrow(d)
  row <- data.frame(id = d[[id]][s], time = stats::median(d[[tc]], na.rm = TRUE),
                    heading = .displacement_direction(d[[xc]][e] - d[[xc]][s],
                                                      d[[yc]][e] - d[[yc]][s]))
  if (return_coords) {
    row$x_start <- d[[xc]][s]; row$y_start <- d[[yc]][s]
    row$x_end   <- d[[xc]][e]; row$y_end   <- d[[yc]][e]
  }
  row
}

# ---- window_net rule ---------------------------------------------------------
# Sliding window net heading; returns one heading per window center
.set_headings_window_net_one <- function(d, id, tc, xc, yc, window_n = 21L, stride = 1L) {
  n <- nrow(d)
  if (n < 2L || window_n < 2L) return(data.frame(id = d[[id]][1], time = d[[tc]][1], heading = NA_real_))
  # a window spans 2 * half + 1 points: shrink it to fit a shorter track
  half <- min(floor(window_n/2), (n - 1L) %/% 2L)
  if (half < 1L) return(data.frame(id = d[[id]][1], time = d[[tc]][1], heading = NA_real_))
  idx <- seq(1+half, n-half, by = max(1L, stride))
  out <- lapply(idx, function(i) {
    s <- i - half; e <- i + half
    data.frame(id = d[[id]][i], time = d[[tc]][i],
               heading = .displacement_direction(d[[xc]][e] - d[[xc]][s],
                                                 d[[yc]][e] - d[[yc]][s]))
  })
  do.call(rbind, out)
}

# ---- goal_bias rule ----------------------------------------------------------
# Heading from vector sum of step directions weighted by radial change (outward positive, inward negative)
.set_headings_goal_bias_one <- function(d, id, tc, xc, yc) {
  if (nrow(d) < 2L) return(data.frame(id = d[[id]][1], time = d[[tc]][1], heading = NA_real_))
  vx <- diff(d[[xc]]); vy <- diff(d[[yc]])
  th <- atan2(vy, vx)
  r  <- sqrt(d[[xc]]^2 + d[[yc]]^2)
  dr <- diff(r)
  sx <- sum(dr * cos(th), na.rm = TRUE)
  sy <- sum(dr * sin(th), na.rm = TRUE)
  data.frame(id = d[[id]][1], time = stats::median(d[[tc]], na.rm = TRUE),
             heading = .displacement_direction(sx, sy))
}

# ---- pca_axis rule -----------------------------------------------------------
# Heading of the first principal axis of the positions (centred) or of the steps
# (about zero); sign from the net displacement, NA when that gives none
.set_headings_pca_axis_one <- function(d, id, tc, xc, yc, source = c("position","velocity"),
                                       return_coords = FALSE) {
  source <- match.arg(source)
  na_row <- function() {
    row <- data.frame(id = d[[id]][1], time = d[[tc]][1], heading = NA_real_)
    if (return_coords) {
      row$x_centroid <- NA_real_; row$y_centroid <- NA_real_
      row$axis_x <- NA_real_;     row$axis_y <- NA_real_
    }
    row
  }
  if (nrow(d) < 2L) return(na_row())
  if (source == "position") {
    M <- cbind(d[[xc]], d[[yc]])
    size2 <- max(abs(M))^2
    M <- scale(M, center = TRUE, scale = FALSE)
    S <- crossprod(M) / (nrow(M) - 1)
  } else {
    # the axis of the steps about zero, not about their mean: a track of equal
    # steps has their direction as its axis
    M <- cbind(diff(d[[xc]]), diff(d[[yc]]))
    size2 <- max(abs(M))^2
    S <- crossprod(M) / nrow(M)
  }
  eg <- eigen(S, symmetric = TRUE)
  # no spread beyond rounding (same points, or no step moves), so no axis
  if (eg$values[1] <= .Machine$double.eps * size2) return(na_row())
  ev <- eg$vectors[, 1]
  # the axis has no sign of its own: take the net displacement's, and give no
  # heading when the net displacement has none along the axis
  net  <- c(d[[xc]][nrow(d)] - d[[xc]][1], d[[yc]][nrow(d)] - d[[yc]][1])
  proj <- sum(ev * net)
  if (abs(proj) <= 64 * .Machine$double.eps * max(abs(c(d[[xc]], d[[yc]]))))
    return(na_row())
  if (proj < 0) ev <- -ev
  heading <- atan2(ev[2], ev[1])
  row <- data.frame(id = d[[id]][1], time = stats::median(d[[tc]], na.rm = TRUE),
                    heading = .wrap_to_2pi(heading))
  if (return_coords) {
    row$x_centroid <- mean(d[[xc]]); row$y_centroid <- mean(d[[yc]])
    row$axis_x <- ev[1];             row$axis_y <- ev[2]
  }
  row
}

# ---- ransac_straight rule ----------------------------------------------------
# Robust line fit (RANSAC) to positions; heading = angle of best line
.set_headings_ransac_straight_one <- function(d, id, tc, xc, yc, eps = 0.02, min_inliers = 10L, n_iter = 200L) {
  n <- nrow(d)
  if (n < min_inliers) return(data.frame(id = d[[id]][1], time = d[[tc]][1], heading = NA_real_))
  X <- cbind(d[[xc]], d[[yc]])
  best_in <- integer(0); best_dir <- c(NA_real_, NA_real_)
  for (it in seq_len(n_iter)) {
    idx <- sample.int(n, 2L)
    p <- X[idx[1], ]; q <- X[idx[2], ]
    v <- q - p
    nv <- sqrt(sum(v^2)); if (!is.finite(nv) || nv == 0) next
    u <- v / nv
    # perpendicular distance from points to line through p with direction u
    w <- sweep(X, 2, p)
    dist <- abs(w[,1]*(-u[2]) + w[,2]*u[1])
    inl <- which(dist <= eps)
    if (length(inl) > length(best_in)) {
      best_in <- inl
      # refine with PCA on inliers
      M <- scale(X[inl, , drop = FALSE], center = TRUE, scale = FALSE)
      ev <- eigen(crossprod(M)/(nrow(M)-1), symmetric=TRUE)$vectors[,1]
      if (sum(ev * u) < 0) ev <- -ev
      best_dir <- ev
    }
  }
  if (!length(best_in)) return(data.frame(id = d[[id]][1], time = d[[tc]][1], heading = NA_real_))
  heading <- atan2(best_dir[2], best_dir[1])
  data.frame(id = d[[id]][1], time = stats::median(d[[tc]][best_in], na.rm = TRUE), heading = .wrap_to_2pi(heading))
}

# ---- maxspeed_window rule ----------------------------------------------------
# Pick window with maximum mean speed; heading = net displacement over that window
.set_headings_maxspeed_window_one <- function(d, id, tc, xc, yc, window_n = 21L) {
  n <- nrow(d); if (n < 3L) return(data.frame(id = d[[id]][1], time = d[[tc]][1], heading = NA_real_))
  if (window_n > n) window_n <- n
  half <- floor(window_n/2)
  vx <- diff(d[[xc]]); vy <- diff(d[[yc]])
  speed <- sqrt(vx^2 + vy^2)
  # rolling mean speed via convolution
  k <- rep(1/window_n, window_n)
  # pad speed to length n by adding zeros at ends
  sp <- c(0, speed)
  rs <- stats::filter(sp, filter = rep(1, window_n-1), sides = 2) / (window_n-1)
  rs[is.na(rs)] <- -Inf
  center <- which.max(rs)
  s <- max(1L, center - half); e <- min(n, center + half)
  data.frame(id = d[[id]][center], time = d[[tc]][center],
             heading = .displacement_direction(d[[xc]][e] - d[[xc]][s],
                                               d[[yc]][e] - d[[yc]][s]))
}

# ---- vm_fit rule -------------------------------------------------------------
# Von Mises fit to step headings; mu is heading. The maximum-likelihood mean
# direction of a von Mises is the (weighted) mean direction, so this is
# velocity_mean under another name.
.set_headings_vm_fit_one <- function(...) .set_headings_velocity_mean_one(...)

# ---- exit/entry rules --------------------------------------------------------
# Heading at first outward exit from radius r0 (exit) or first inward entry (entry)
.set_headings_exit_entry_one <- function(d, id, tc, xc, yc, r0, mode = c("exit","entry")) {
  mode <- match.arg(mode)
  r <- sqrt(d[[xc]]^2 + d[[yc]]^2)
  n <- nrow(d)
  for (i in 1:(n-1)) {
    if (mode == "exit") {
      if (r[i] <= r0 && r[i+1] > r0) {
        heading <- atan2(d[[yc]][i+1] - d[[yc]][i], d[[xc]][i+1] - d[[xc]][i])
        return(data.frame(id = d[[id]][i], time = d[[tc]][i+1], heading = .wrap_to_2pi(heading)))
      }
    } else {
      if (r[i] >= r0 && r[i+1] < r0) {
        heading <- atan2(d[[yc]][i+1] - d[[yc]][i], d[[xc]][i+1] - d[[xc]][i])
        return(data.frame(id = d[[id]][i], time = d[[tc]][i+1], heading = .wrap_to_2pi(heading)))
      }
    }
  }
  data.frame(id = d[[id]][1], time = d[[tc]][1], heading = NA_real_)
}

# ---- ring_tangent rule -------------------------------------------------------
# At radius r*, compute local tangent direction using smoothed positions and finite differences
.set_headings_ring_tangent_one <- function(d, id, tc, xc, yc, r_star, smooth_n = 5L, deriv_span = 1L, direction = c("either","outward","inward")) {
  direction <- match.arg(direction)
  n <- nrow(d)
  if (n < max(3L, smooth_n)) return(data.frame(id = d[[id]][1], time = d[[tc]][1], heading = NA_real_))
  # simple moving average smoothing
  k <- rep(1/smooth_n, smooth_n)
  xs <- stats::filter(d[[xc]], k, sides = 2)
  ys <- stats::filter(d[[yc]], k, sides = 2)
  xs[is.na(xs)] <- d[[xc]][is.na(xs)]
  ys[is.na(ys)] <- d[[yc]][is.na(ys)]
  r <- sqrt(xs^2 + ys^2)
  # find crossing of r_star
  for (i in 1:(n-1)) {
    cond <- (r[i] - r_star) * (r[i+1] - r_star) <= 0
    if (!cond) next
    if (direction == "outward" && !(r[i] <= r_star && r[i+1] > r_star)) next
    if (direction == "inward"  && !(r[i] >= r_star && r[i+1] < r_star)) next
    s <- max(1L, i - deriv_span); e <- min(n, i + deriv_span)
    dx <- xs[e] - xs[s]; dy <- ys[e] - ys[s]
    heading <- atan2(dy, dx)
    return(data.frame(id = d[[id]][i], time = d[[tc]][i], heading = .wrap_to_2pi(heading)))
  }
  data.frame(id = d[[id]][1], time = d[[tc]][1], heading = NA_real_)
}

# ---- velocity_mean rule ------------------------------------------------------
# Heading = circular mean of instantaneous step headings; weights default to step length
.set_headings_velocity_mean_one <- function(d, id, tc, xc, yc, weight_by = c("uniform","step_length")) {
  weight_by <- match.arg(weight_by)
  if (nrow(d) < 2L) return(data.frame(id = d[[id]][1], time = d[[tc]][1], heading = NA_real_))
  vx <- diff(d[[xc]]); vy <- diff(d[[yc]])
  data.frame(id = d[[id]][1], time = stats::median(d[[tc]], na.rm = TRUE),
             heading = .weighted_mean_direction(atan2(vy, vx),
                                                .step_weights(vx, vy, weight_by)))
}

# ---- velocity_axis rule ------------------------------------------------------
# Heading = axial mean of instantaneous step headings (angle-doubling); the
# back-and-forth movement axis in [0, pi). Weights default to step length.
.set_headings_velocity_axis_one <- function(d, id, tc, xc, yc,
                                            weight_by = c("step_length", "uniform")) {
  weight_by <- match.arg(weight_by)
  if (nrow(d) < 2L)
    return(data.frame(id = d[[id]][1], time = d[[tc]][1], heading = NA_real_))
  vx <- diff(d[[xc]]); vy <- diff(d[[yc]])
  th <- atan2(vy, vx)
  w  <- .step_weights(vx, vy, weight_by)
  ok <- is.finite(th) & is.finite(w) & w > 0
  axis <- if (any(ok)) {
    mv <- sum(w[ok] * exp(1i * 2 * th[ok])) / sum(w[ok])
    (Arg(mv) / 2) %% pi
  } else NA_real_
  data.frame(id = d[[id]][1], time = stats::median(d[[tc]], na.rm = TRUE),
             heading = axis)
}

# Options each built-in rule reads from `...`. `return_coords` is accepted by
# every built-in (the rules that do not draw construction coordinates ignore
# it, as documented). Registered and function rules take their own `...`.
.builtin_rule_options <- list(
  crossing        = c("circ0", "circ1", "direction"),
  distal          = "max_radius",
  straight        = c("tol", "min_len"),
  origin_mean     = "r_power",
  net             = character(),
  velocity_mean   = "weight_by",
  velocity_axis   = "weight_by",
  window_net      = c("window_n", "stride"),
  goal_bias       = character(),
  pca_axis        = "source",
  ransac_straight = c("eps", "min_inliers", "n_iter"),
  maxspeed_window = "window_n",
  vm_fit          = "weight_by",
  exit            = "r0",
  entry           = "r0",
  ring_tangent    = c("r_star", "smooth_n", "deriv_span", "direction")
)

.check_rule_options <- function(rule, dots) {
  if (!length(dots)) return(invisible(NULL))
  nms <- names(dots) %||% rep("", length(dots))
  ok  <- c(.builtin_rule_options[[rule]], "return_coords")
  bad <- !nzchar(nms) | !nms %in% ok
  if (!any(bad)) return(invisible(NULL))
  shown <- ifelse(nzchar(nms[bad]), paste0("`", nms[bad], "`"), "an unnamed argument")
  rlang::abort(
    sprintf("derive_headings(rule = \"%s\") does not take %s. It takes: %s.",
            rule, paste(shown, collapse = ", "),
            paste0("`", ok, "`", collapse = ", ")),
    class = "radiatR_invalid_argument", arg = "...")
}

# ---- method for Tracks ------------------------------------------------------
#' @param first_only logical; if TRUE, return only the first matching heading per trajectory
#' @param carry optional character vector of columns from the source data to append via nearest time
#' @rdname derive_headings
#' @export
setMethod("derive_headings", "Tracks", function(
    x,
    rule,
    ...,
    first_only = FALSE,
    carry = NULL,
    on_missing = c("warn", "error", "quiet"),
    coords = c("absolute", "relative")) {

  coords     <- match.arg(coords)
  on_missing <- match.arg(on_missing)
  builtins <- names(.builtin_rule_options)
  if (missing(rule))
    stop("derive_headings: `rule` is required. Built-in rules: ",
         paste(builtins, collapse = ", "), ". Registered rules: ",
         if (length(list_heading_rules())) paste(list_heading_rules(), collapse = ", ") else "none",
         ".", call. = FALSE)
  if (!is.function(rule) && !(is.character(rule) && length(rule) == 1L && !is.na(rule)))
    stop("derive_headings: `rule` must be a single rule name or a function, not ",
         deparse1(rule), ".", call. = FALSE)
  rule_label <- if (is.function(rule)) "custom" else rule

  id <- x@cols$id; tc <- x@cols$time

  if (coords == "relative") {
    if (is.null(x@cols$rel_x) || is.null(x@cols$rel_y))
      stop("coords='relative' requires rel_x and rel_y registered in Tracks@cols.")
    xc <- x@cols$rel_x
    yc <- x@cols$rel_y
  } else {
    xc <- x@cols$x
    yc <- x@cols$y
  }

  if (is.null(xc) || is.null(yc))
    stop("derive_headings: Tracks needs x/y columns for these rules.")

  d <- as.data.frame(x)
  sp <- split(seq_len(nrow(d)), d[[id]])

  # locate user rule if provided as function or registered name
  dots <- list(...)
  if (is.function(rule)) {
    user_fun <- rule
    res <- do.call(rbind, lapply(sp, function(ii) {
      out <- user_fun(d[ii, , drop = FALSE], cols = x@cols, ...)
      stopifnot(all(c("id","time","heading") %in% names(out)))
      out
    }))
  } else if (is.character(rule) && !(rule %in% builtins)) {
    if (exists(rule, envir = .heading_registry, inherits = FALSE)) {
      user_fun <- get(rule, envir = .heading_registry, inherits = FALSE)
      res <- do.call(rbind, lapply(sp, function(ii) {
        out <- user_fun(d[ii, , drop = FALSE], cols = x@cols, ...)
        stopifnot(all(c("id","time","heading") %in% names(out)))
        out
      }))
    } else {
      stop(sprintf("Unknown heading rule '%s'. Register via register_heading_rule() or supply a function.", rule))
    }
  } else {
    .check_rule_options(rule, dots)
    res <- switch(rule,
      crossing = {
        circ0 <- dots$circ0; circ1 <- dots$circ1
        direction     <- dots$direction     %||% "outward"
        return_coords <- dots$return_coords %||% FALSE
        if (is.null(circ0) || is.null(circ1)) stop("crossing rule requires circ0 and circ1")
        do.call(rbind, lapply(sp, function(ii) .set_headings_crossing_one(d[ii, , drop = FALSE], id, tc, xc, yc,
                                                                          circ0 = circ0, circ1 = circ1,
                                                                          direction = direction,
                                                                          first_only = first_only,
                                                                          return_coords = return_coords)))
      },
      distal   = {
        return_coords <- dots$return_coords %||% FALSE
        max_radius    <- dots$max_radius    %||% Inf
        do.call(rbind, lapply(sp, function(ii) .set_headings_distal_one(d[ii, , drop = FALSE], id, tc, xc, yc,
                                                                        return_coords = return_coords,
                                                                        max_radius = max_radius)))
      },
      straight = {
        tol <- dots$tol %||% (pi/18); min_len <- dots$min_len %||% 5L
        return_coords <- dots$return_coords %||% FALSE
        do.call(rbind, lapply(sp, function(ii) .set_headings_straight_one(d[ii, , drop = FALSE], id, tc, xc, yc,
                                                                          tol = tol, min_len = as.integer(min_len),
                                                                          return_coords = return_coords)))
      },
      origin_mean = {
        r_power <- dots$r_power %||% 0
        if (!is.numeric(r_power) || length(r_power) != 1L || !is.finite(r_power))
          rlang::abort("`r_power` must be one finite number.",
                       class = "radiatR_invalid_argument", arg = "r_power")
        do.call(rbind, lapply(sp, function(ii) .set_headings_origin_mean_one(d[ii, , drop = FALSE], id, tc, xc, yc, r_power = r_power)))
      },
      net = {
        return_coords <- dots$return_coords %||% FALSE
        do.call(rbind, lapply(sp, function(ii) .set_headings_net_one(d[ii, , drop = FALSE], id, tc, xc, yc,
                                                                     return_coords = return_coords)))
      },
      velocity_mean = {
        weight_by <- dots$weight_by %||% "uniform"
        do.call(rbind, lapply(sp, function(ii) .set_headings_velocity_mean_one(d[ii, , drop = FALSE], id, tc, xc, yc, weight_by = weight_by)))
      },
      velocity_axis = {
        weight_by <- dots$weight_by %||% "step_length"
        do.call(rbind, lapply(sp, function(ii)
          .set_headings_velocity_axis_one(d[ii, , drop = FALSE], id, tc, xc, yc,
                                          weight_by = weight_by)))
      },
      window_net = {
        window_n <- as.integer(dots$window_n %||% 21L); stride <- as.integer(dots$stride %||% 1L)
        do.call(rbind, lapply(sp, function(ii) .set_headings_window_net_one(d[ii, , drop = FALSE], id, tc, xc, yc, window_n = window_n, stride = stride)))
      },
      goal_bias = do.call(rbind, lapply(sp, function(ii) .set_headings_goal_bias_one(d[ii, , drop = FALSE], id, tc, xc, yc))),
      pca_axis = {
        source <- dots$source %||% "position"
        return_coords <- dots$return_coords %||% FALSE
        do.call(rbind, lapply(sp, function(ii) .set_headings_pca_axis_one(d[ii, , drop = FALSE], id, tc, xc, yc,
                                                                          source = source, return_coords = return_coords)))
      },
      ransac_straight = {
        eps <- dots$eps %||% 0.02; min_inliers <- as.integer(dots$min_inliers %||% 10L); n_iter <- as.integer(dots$n_iter %||% 200L)
        do.call(rbind, lapply(sp, function(ii) .set_headings_ransac_straight_one(d[ii, , drop = FALSE], id, tc, xc, yc, eps = eps, min_inliers = min_inliers, n_iter = n_iter)))
      },
      maxspeed_window = {
        window_n <- as.integer(dots$window_n %||% 21L)
        do.call(rbind, lapply(sp, function(ii) .set_headings_maxspeed_window_one(d[ii, , drop = FALSE], id, tc, xc, yc, window_n = window_n)))
      },
      vm_fit = {
        weight_by <- dots$weight_by %||% "uniform"
        do.call(rbind, lapply(sp, function(ii) .set_headings_vm_fit_one(d[ii, , drop = FALSE], id, tc, xc, yc, weight_by = weight_by)))
      },
      exit = {
        r0 <- dots$r0; if (is.null(r0)) stop("exit rule requires r0")
        do.call(rbind, lapply(sp, function(ii) .set_headings_exit_entry_one(d[ii, , drop = FALSE], id, tc, xc, yc, r0 = r0, mode = "exit")))
      },
      entry = {
        r0 <- dots$r0; if (is.null(r0)) stop("entry rule requires r0")
        do.call(rbind, lapply(sp, function(ii) .set_headings_exit_entry_one(d[ii, , drop = FALSE], id, tc, xc, yc, r0 = r0, mode = "entry")))
      },
      ring_tangent = {
        r_star <- dots$r_star; if (is.null(r_star)) stop("ring_tangent rule requires r_star")
        smooth_n <- as.integer(dots$smooth_n %||% 5L); deriv_span <- as.integer(dots$deriv_span %||% 1L)
        direction <- dots$direction %||% "either"
        do.call(rbind, lapply(sp, function(ii) .set_headings_ring_tangent_one(d[ii, , drop = FALSE], id, tc, xc, yc, r_star = r_star, smooth_n = smooth_n, deriv_span = deriv_span, direction = direction)))
      }
    )
  }

  rownames(res) <- NULL
  if (!is.null(carry)) res <- .carry_nearest(res, d, idc = id, tc = tc, cols = carry)

  # Attrition: trials for which the rule produced no heading (NA). Rows are
  # always retained; the attributes let downstream consumers report the
  # used-vs-total denominator. The policy only controls messaging.
  n_total     <- nrow(res)
  miss        <- is.na(res$heading)
  n_missing   <- sum(miss)
  missing_ids <- unique(res$id[miss])
  n_trials    <- length(unique(res$id))

  if (n_missing > 0L && on_missing != "quiet") {
    pct  <- round(100 * n_missing / n_total, 1)
    # a rule with several headings per trial (window_net, crossing) counts rows
    what <- if (n_trials == n_total)
      sprintf("%d of %d trials (%.1f%%) produced no heading", n_missing, n_total, pct)
    else
      sprintf("%d of %d headings (%.1f%%), from %d of %d trials, are missing",
              n_missing, n_total, pct, length(missing_ids), n_trials)
    msg <- paste0("derive_headings(rule = '", rule_label, "'): ", what,
                  " and are excluded from circular statistics. Rule-based ",
                  "failures are often non-random and can bias results; inspect ",
                  "attr(x, \"missing_ids\").")
    if (on_missing == "error")
      stop(msg, " Set on_missing = \"warn\" or \"quiet\" to proceed.", call. = FALSE)
    warning(msg, call. = FALSE)
  }

  # Return a durable headings_frame instead of a plain df + loose attrs. The
  # canonical display/heading metadata rides through dplyr verbs; the attrition
  # attributes (n_total/n_missing/n_trials/missing_ids) are re-attached for
  # downstream used-vs-total reporting.
  out <- new_headings_frame(
    res,
    display     = attr(res, "display", exact = TRUE) %||% circ_display(),
    heading_col = "heading",
    colour_col  = attr(res, "colour_col", exact = TRUE),
    coords      = coords
  )
  attr(out, "n_total")     <- n_total
  attr(out, "n_missing")   <- n_missing
  attr(out, "n_trials")    <- n_trials
  attr(out, "missing_ids") <- missing_ids
  out
})

# ---- circular summaries over headings ----------------------------------------
#' Circular statistics over derived headings
#'
#' Derives one heading per trial via `derive_headings()`, then computes
#' circular summary statistics (mean direction, resultant length, concentration)
#' optionally grouped by one or more metadata columns.
#'
#' @param x A [`Tracks`] object.
#' @param rule Required. Heading derivation rule passed to [derive_headings()].
#'   One of `"crossing"`, `"distal"`, `"straight"`, `"origin_mean"`, `"net"`,
#'   or `"velocity_mean"`.
#' @param group_by Character vector of column names used to group headings
#'   before summarising. Default `"id"` returns one row per trial. Use `NULL`
#'   for a single global summary row. Other columns must first be carried
#'   onto the heading table with `carry` (e.g. `carry = "arc"`).
#' @param ... Additional arguments forwarded to [derive_headings()], such as
#'   `circ0`, `circ1`, `carry`, `return_coords`, or `coords`.
#'
#' @return A `data.frame` with grouping columns followed by `mean_dir`
#'   (radians, unit-circle convention, 0 to \eqn{2\pi}{2*pi}), `resultant_R` (0–1), `kappa`
#'   (von Mises concentration, with no small-sample bias correction; see
#'   \code{\link{circ_summary}()}), and `n` (number of valid headings in the group).
#'
#' @examples
#' data(cpunctatus)
#' # per-trial headings (default)
#' head(circ_summary_headings(cpunctatus, rule = "crossing",
#'                            circ0 = 0.2, circ1 = 0.4))
#'
#' # per-condition summary: carry the "arc" column through to group by it
#' circ_summary_headings(cpunctatus, rule = "crossing",
#'                       circ0 = 0.2, circ1 = 0.4,
#'                       carry = "arc", group_by = "arc")
#'
#' @export
circ_summary_headings <- function(x, rule, group_by = "id", ...) {
  if (missing(rule))
    stop("circ_summary_headings: `rule` is required: one of \"crossing\", ",
         "\"distal\", \"straight\", \"origin_mean\", \"net\", \"velocity_mean\".",
         call. = FALSE)
  rule <- match.arg(rule, c("crossing","distal","straight","origin_mean","net","velocity_mean"))

  hd <- derive_headings(x, rule = rule, ...)
  coords <- attr(hd, "coords") %||% "absolute"

  if (nrow(hd) == 0L || all(is.na(hd$heading))) {
    return(data.frame(mean_dir = NA_real_, resultant_R = NA_real_, kappa = NA_real_, n = 0L))
  }

  hd$.uc_heading <- hd$heading   # always UC radians

  # group keys
  if (is.null(group_by)) {
    hd$..grp <- factor("all")
  } else {
    if (!all(group_by %in% names(hd))) {
      if (!identical(group_by, "id"))
        stop("group_by contains unknown column(s) in derived heading table")
    }
    if (length(group_by) == 1L && group_by == "id") {
      hd$..grp <- hd$id
    } else {
      key <- interaction(hd[, group_by, drop = FALSE], drop = TRUE, lex.order = TRUE)
      hd$..grp <- key
    }
  }

  sp <- split(hd$.uc_heading, hd$..grp)
  res <- lapply(names(sp), function(g) {
    th <- sp[[g]]
    th <- th[is.finite(th)]
    if (!length(th)) return(data.frame(.grp = g, mean_dir = NA_real_, resultant_R = NA_real_, kappa = NA_real_, n = 0L))
    tc  <- .as_circ(th)
    mu  <- circular::mean.circular(tc, na.rm = TRUE)
    R   <- circular::rho.circular(tc,  na.rm = TRUE)
    kap <- .kappa_from_Rbar(as.numeric(R))
    out_mu <- .wrap_to_2pi(as.numeric(mu))
    data.frame(.grp = g, mean_dir = out_mu, resultant_R = as.numeric(R),
               kappa = as.numeric(kap), n = length(th))
  })
  res <- do.call(rbind, res)

  # if grouped by id, replace .grp with id; if by multiple columns, split back
  if (is.null(group_by)) {
    res$group <- "all"
    res$.grp <- NULL
  } else if (length(group_by) == 1L && group_by == "id") {
    names(res)[names(res) == ".grp"] <- "id"
  } else {
    parts <- do.call(rbind, strsplit(as.character(res$.grp), split = "\\."))
    parts <- as.data.frame(parts, stringsAsFactors = FALSE)
    names(parts) <- group_by
    res <- cbind(parts, res[, setdiff(names(res), ".grp"), drop = FALSE])
  }
  rownames(res) <- NULL
  res
}

# ---- user-defined rule registry ----------------------------------------------
.heading_registry <- new.env(parent = emptyenv())

#' Register a custom heading derivation rule
#'
#' Adds a named function to the heading rule registry so it can be called by
#' \code{\link{derive_headings}} via \code{rule = "name"}.  The function must
#' accept \code{(df, cols, ...)} and return a data frame with columns
#' \code{id}, \code{time}, and \code{heading} (radians).
#'
#' @param name Character; unique rule name.
#' @param fun Function with signature \code{function(df, cols, ...)}.
#' @param overwrite Logical; replace an existing rule of the same name.
#' @return The rule name, invisibly.
#' @seealso \code{\link{list_heading_rules}}, \code{\link{derive_headings}}
#' @examples
#' last_bearing <- function(df, cols, ...) {
#'   n <- nrow(df)
#'   data.frame(id = df[[cols$id]][n], time = df[[cols$time]][n],
#'              heading = atan2(df[[cols$y]][n], df[[cols$x]][n]) %% (2 * pi))
#' }
#' register_heading_rule("last_bearing", last_bearing, overwrite = TRUE)
#' head(derive_headings(cpunctatus, rule = "last_bearing"))
#' @export
register_heading_rule <- function(name, fun, overwrite = FALSE) {
  stopifnot(is.character(name), length(name) == 1L, is.function(fun))
  if (exists(name, envir = .heading_registry, inherits = FALSE) && !overwrite)
    stop("A rule named '", name, "' already exists; set overwrite = TRUE to replace")
  assign(name, fun, envir = .heading_registry)
  invisible(name)
}

#' List registered custom heading rules
#'
#' @return Sorted character vector of registered rule names.
#' @seealso \code{\link{register_heading_rule}}
#' @examples
#' list_heading_rules()
#' @export
list_heading_rules <- function() sort(ls(envir = .heading_registry))

# Heading from a rigid body axis defined by two tracked bodypart points.
# Requires the Tracks to carry <anterior>_x, <anterior>_y, <posterior>_x,
# <posterior>_y columns — load data with dialect = "deeplabcut" and
# bodypart = c("<anterior>", "<posterior>") to ensure they are present.
#
# frame_select controls which frame contributes the reported heading:
#   "distal" (default) — axis angle at the frame of maximum radial distance
#   "mean"             — circular mean of per-frame axis angles
#   "last"             — axis angle at the final frame
register_heading_rule("bodypart_axis", function(df, cols, anterior, posterior,
                                                frame_select = c("distal", "mean", "last", "all")) {
  if (missing(anterior) || missing(posterior))
    stop("bodypart_axis: provide anterior and posterior bodypart name prefixes")
  frame_select <- match.arg(frame_select)
  id_col   <- cols$id   %||% "id"
  time_col <- cols$time %||% "time"
  xc       <- cols$x    %||% "x"
  yc       <- cols$y    %||% "y"

  axc <- paste0(anterior,  "_x");  ayc <- paste0(anterior,  "_y")
  pxc <- paste0(posterior, "_x");  pyc <- paste0(posterior, "_y")
  miss <- setdiff(c(axc, ayc, pxc, pyc), names(df))
  if (length(miss))
    stop("bodypart_axis: column(s) not found: ", paste(miss, collapse = ", "),
         ". Load with dialect='deeplabcut', bodypart=c('", anterior, "','", posterior, "')")

  ax <- as.numeric(df[[axc]]); ay <- as.numeric(df[[ayc]])
  px <- as.numeric(df[[pxc]]); py <- as.numeric(df[[pyc]])
  angles <- atan2(ay - py, ax - px)  # anterior → forward

  if (frame_select == "all") {
    return(data.frame(id = rep(df[[id_col]][1L], nrow(df)),
                      time = df[[time_col]], heading = angles,
                      stringsAsFactors = FALSE))
  }
  if (frame_select == "distal") {
    r    <- sqrt(as.numeric(df[[xc]])^2 + as.numeric(df[[yc]])^2)
    idx  <- which.max(r)
    h    <- angles[idx]
    tval <- df[[time_col]][idx]
  } else if (frame_select == "mean") {
    h    <- atan2(mean(sin(angles), na.rm = TRUE), mean(cos(angles), na.rm = TRUE))
    tval <- df[[time_col]][ceiling(nrow(df) / 2L)]
  } else {
    h    <- angles[nrow(df)]
    tval <- df[[time_col]][nrow(df)]
  }

  data.frame(id = df[[id_col]][1L], time = tval, heading = h,
             stringsAsFactors = FALSE)
})

# Heading from a pre-computed orientation angle column (e.g. a theta column).
# Use when the tool has already fitted a body-axis angle per frame — no separate
# anterior/posterior point columns are needed.
# theta_col: name of the angle column in the Tracks data (default "theta").
# frame_select: same semantics as bodypart_axis.
register_heading_rule("ellipse_axis", function(df, cols, theta_col = "theta",
                                               frame_select = c("distal", "mean", "last", "all")) {
  frame_select <- match.arg(frame_select)
  if (!theta_col %in% names(df))
    stop("ellipse_axis: column '", theta_col, "' not found. ",
         "ellipse_axis needs a pre-computed orientation column (e.g. 'theta').")
  id_col   <- cols$id   %||% "id"
  time_col <- cols$time %||% "time"
  xc       <- cols$x    %||% "x"
  yc       <- cols$y    %||% "y"

  angles <- as.numeric(df[[theta_col]])

  if (frame_select == "all") {
    return(data.frame(id = rep(df[[id_col]][1L], nrow(df)),
                      time = df[[time_col]], heading = angles,
                      stringsAsFactors = FALSE))
  }
  if (frame_select == "distal") {
    r    <- sqrt(as.numeric(df[[xc]])^2 + as.numeric(df[[yc]])^2)
    idx  <- which.max(r)
    h    <- angles[idx]
    tval <- df[[time_col]][idx]
  } else if (frame_select == "mean") {
    h    <- atan2(mean(sin(angles), na.rm = TRUE), mean(cos(angles), na.rm = TRUE))
    tval <- df[[time_col]][ceiling(nrow(df) / 2L)]
  } else {
    h    <- angles[nrow(df)]
    tval <- df[[time_col]][nrow(df)]
  }

  data.frame(id = df[[id_col]][1L], time = tval, heading = h,
             stringsAsFactors = FALSE)
})

# ---- pose_to_headings --------------------------------------------------------

#' Derive per-frame headings from pose data without a Tracks
#'
#' Computes a heading angle for every row in \code{df} from either two
#' bodypart keypoint columns or a pre-computed orientation angle column.
#' Intended for tethered or mostly stationary subjects where the position
#' trajectory is absent or uninformative and body pose is the primary signal,
#' but also useful for extracting a dense heading time series from trajectory
#' data.  The output is compatible with \code{\link{circ_dispersion}},
#' \code{\link{sector_summary}}, \code{\link{add_heading_points}}, and
#' \code{\link{add_angle_rose}}.
#'
#' @param df Data frame with at least id, time, and keypoint or angle columns.
#' @param anterior Prefix of the anterior bodypart columns
#'   (\code{<anterior>_x} and \code{<anterior>_y} must exist).
#' @param posterior Prefix of the posterior bodypart columns.
#' @param theta_col Name of a pre-computed orientation angle column (alternative
#'   to \code{anterior}/\code{posterior}).
#' @param id_col Column identifying trials or individuals.  Auto-detected from
#'   common names; defaults to a single group \code{"1"} if absent.
#' @param time_col Column of frame indices or timestamps.  Defaults to row
#'   position if absent.
#' @param angle_convention \code{"unit_circle"} (default) or \code{"clock"}.
#' @return Data frame with columns \code{id}, \code{time}, \code{heading} (in
#'   radians), with an \code{angle_convention} attribute for downstream
#'   compatibility.
#' @examples
#' pose <- data.frame(
#'   id     = "fish1", frame = 1:4,
#'   head_x = c(0.1, 0.2, 0.2, 0.1), head_y = c(0.0, 0.1, 0.2, 0.3),
#'   tail_x = c(0.0, 0.1, 0.2, 0.2), tail_y = c(0.0, 0.0, 0.1, 0.2)
#' )
#' pose_to_headings(pose, anterior = "head", posterior = "tail")
#' @export
pose_to_headings <- function(df, anterior = NULL, posterior = NULL,
                              theta_col = NULL,
                              id_col = NULL, time_col = NULL,
                              angle_convention = c("unit_circle", "clock")) {
  angle_convention <- match.arg(angle_convention)
  stopifnot(is.data.frame(df))
  nms <- names(df)

  id <- id_col %||%
    .guess_col(nms, c("id", "individual", "track", "subject", "animal")) %||%
    "..pose_id"
  if (id == "..pose_id") { df[["..pose_id"]] <- "1"; id <- "..pose_id" }

  time <- time_col %||%
    .guess_col(nms, c("time", "frame", "frame_idx", "t", "timestamp")) %||%
    "..pose_time"
  if (time == "..pose_time") { df[["..pose_time"]] <- seq_len(nrow(df)); time <- "..pose_time" }

  if (!is.null(theta_col)) {
    if (!theta_col %in% nms)
      stop("pose_to_headings: column '", theta_col, "' not found")
    angles <- as.numeric(df[[theta_col]])
  } else if (!is.null(anterior) && !is.null(posterior)) {
    axc <- paste0(anterior,  "_x"); ayc <- paste0(anterior,  "_y")
    pxc <- paste0(posterior, "_x"); pyc <- paste0(posterior, "_y")
    miss <- setdiff(c(axc, ayc, pxc, pyc), nms)
    if (length(miss))
      stop("pose_to_headings: column(s) not found: ", paste(miss, collapse = ", "))
    angles <- atan2(as.numeric(df[[ayc]]) - as.numeric(df[[pyc]]),
                   as.numeric(df[[axc]]) - as.numeric(df[[pxc]]))
  } else {
    stop("pose_to_headings: supply either anterior + posterior, or theta_col")
  }

  if (angle_convention == "clock") angles <- (pi / 2) - angles

  out <- data.frame(id = df[[id]], time = df[[time]], heading = angles,
                    stringsAsFactors = FALSE)
  attr(out, "angle_convention") <- angle_convention
  out
}
