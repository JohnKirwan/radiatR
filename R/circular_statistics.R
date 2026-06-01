# Circular statistics bridge helpers
#
# Users should rely on the `circular` package for low-level statistics such as
# means, resultant length, and dispersion. This module focuses on higher-level
# summaries that combine those primitives for trajectory sets.

#' Circular summary statistics for trajectories
#'
#' Computes per-trial or global circular statistics (mean direction, resultant
#' length, concentration) from the step-angle column of a `TrajSet`.
#'
#' @param x A [`TrajSet`] object.
#' @param w Character. Name of a weight column in `x@data`. When `NULL`
#'   (default), all steps are weighted equally.
#' @param by Character. `"id"` (default) returns one row per trial;
#'   `"global"` pools all observations into a single summary row.
#' @param angle_convention Character. Output convention for `mean_dir`:
#'   `"unit_circle"` (default; 0 = East, counterclockwise) or `"clock"`
#'   (0 = North, clockwise).
#'
#' @return A `data.frame` with columns `id`, `n`, `t_start`, `t_end`,
#'   `mean_dir` (radians, 0 to 2pi), `resultant_R` (0--1), and `kappa`
#'   (von Mises concentration; `NA` when estimation fails).
#'
#' @examples
#' \dontrun{
#' data(plividus)
#' circ_summary(plividus, by = "id")
#' circ_summary(plividus, by = "global")
#' }
#'
#' @export
#' @importFrom circular mean.circular rho.circular circular
setGeneric("circ_summary", function(x, w = NULL, by = c("id","global"),
                                    angle_convention = c("unit_circle","clock"))
  standardGeneric("circ_summary"))

.est_kappa_safe <- function(tc, fallback = NA_real_, ...) {
  if (exists("est.kappa", envir = asNamespace("circular"), inherits = FALSE)) {
    fun <- get("est.kappa", envir = asNamespace("circular"), inherits = FALSE)
    res <- tryCatch(fun(tc, ...), error = function(e) NA_real_)
    if (is.numeric(res) && length(res)) {
      res <- as.numeric(res)[1]
      if (is.finite(res)) return(res)
    }
  }
  fallback
}

#' @rdname circ_summary
#' @export
setMethod("circ_summary", "TrajSet", function(x, w = NULL, by = c("id","global"),
                                              angle_convention = c("unit_circle","clock")) {
  by               <- match.arg(by)
  angle_convention <- match.arg(angle_convention)
  id <- x@cols$id; tm <- x@cols$time; th <- x@cols$angle
  wcol <- if (is.null(w)) x@cols$weight else w
  d <- x@data
  split_idx <- if (by == "id") split(seq_len(nrow(d)), d[[id]]) else list(all = seq_len(nrow(d)))

  rows <- lapply(names(split_idx), function(k) {
    ii <- split_idx[[k]]
    theta <- d[[th]][ii]
    wts <- if (!is.null(wcol) && wcol %in% names(d)) d[[wcol]][ii] else NULL

    valid <- !is.na(theta)
    if (!is.null(wts)) {
      valid <- valid & !is.na(wts)
    }
    theta_valid <- theta[valid]
    wts_valid <- if (!is.null(wts)) wts[valid] else NULL

    tc <- circular::circular(theta_valid, units = "radians", modulo = "2pi")
    mu <- if (length(theta_valid)) {
      circular::mean.circular(tc, na.rm = TRUE, weights = wts_valid)
    } else {
      NA_real_
    }

    if (length(theta_valid)) {
      if (is.null(wts_valid)) {
        mean_cos <- mean(cos(theta_valid))
        mean_sin <- mean(sin(theta_valid))
      } else {
        w_norm <- wts_valid / sum(wts_valid)
        mean_cos <- sum(w_norm * cos(theta_valid))
        mean_sin <- sum(w_norm * sin(theta_valid))
      }
      R <- sqrt(mean_cos^2 + mean_sin^2)
    } else {
      R <- NA_real_
    }

    kap <- .est_kappa_safe(tc, fallback = NA_real_, w = wts_valid)
    uc_mu  <- .wrap_to_2pi(as.numeric(mu))
    out_mu <- if (angle_convention == "clock") (pi/2 - uc_mu) %% (2*pi) else uc_mu
    data.frame(
      id          = if (by == "id") k else "global",
      n           = sum(!is.na(theta)),
      t_start     = d[[tm]][ii[1]],
      t_end       = d[[tm]][ii[length(ii)]],
      mean_dir    = out_mu,
      resultant_R = as.numeric(R),
      kappa       = as.numeric(kap),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
})

#' Dwell-time proportions across quadrant x ring zones
#'
#' Classifies each trajectory observation into one of N quadrant sectors and M
#' annular rings, then returns per-trial frame counts and proportions. Applicable
#' to any circular arena experiment where spatial dwell time is of interest
#' (water maze, open-field, Drosophila preference assay, etc.).
#'
#' The target quadrant (Q1) is centred on `target_angle`; Q2--Q4 follow
#' counter-clockwise. Observations outside `max(ring_breaks)` are excluded from
#' both counts and the proportion denominator.
#'
#' @param x A [`TrajSet`] object with x/y (or rel_x/rel_y) columns registered.
#' @param target_angle Numeric. Radians. Direction of the target zone from the
#'   arena centre. Q1 spans +/-45degrees around this angle.
#' @param target_radius Numeric. Accepted for API symmetry with
#'   [count_goal_entries()] but not used in zone assignment. Default `1`.
#' @param ring_breaks Numeric vector. Annular ring boundaries, must start at
#'   `0`. Default `c(0, 0.5, 0.8, 1)` gives three rings: inner / middle /
#'   outer (thigmotaxis).
#' @param coords Character. `"absolute"` (default) uses `@cols$x`/`@cols$y`;
#'   `"relative"` uses `@cols$rel_x`/`@cols$rel_y`.
#'
#' @return A `data.frame` with one row per observed (id x quadrant x ring)
#'   combination, with columns `id`, `quadrant` (integer, 1 = target),
#'   `ring` (integer, 1 = innermost), `zone` (e.g. `"Q1.R3"`), `n_frames`
#'   (integer), and `proportion` (numeric). Combinations with zero observations
#'   are omitted.
#'
#' @examples
#' \dontrun{
#' # Water maze probe trial: platform was at 45 degrees (NE)
#' dwell <- zone_dwell(ts, target_angle = pi / 4,
#'                     ring_breaks = c(0, 0.5, 0.8, 1))
#' # Q1 proportion > 0.25 indicates above-chance target preference
#' }
#'
#' @seealso [count_goal_entries()]
#' @export
zone_dwell <- function(x, target_angle, target_radius = 1,
                       ring_breaks = c(0, 0.5, 0.8, 1),
                       coords = c("absolute", "relative")) {
  coords <- match.arg(coords)
  if (!is.numeric(ring_breaks) || length(ring_breaks) < 2L)
    stop("ring_breaks must be a numeric vector of length >= 2.")
  if (ring_breaks[1L] != 0)
    stop("ring_breaks must start at 0.")
  if (is.unsorted(ring_breaks))
    stop("ring_breaks must be in increasing order.")
  if (coords == "relative") {
    if (is.null(x@cols$rel_x) || is.null(x@cols$rel_y))
      stop("coords='relative' requires rel_x and rel_y registered in TrajSet@cols.")
    xc <- x@cols$rel_x
    yc <- x@cols$rel_y
  } else {
    xc <- x@cols$x
    yc <- x@cols$y
  }
  if (is.null(xc) || is.null(yc))
    stop("zone_dwell: TrajSet needs x/y columns.")

  id_col <- x@cols$id
  d      <- x@data
  px     <- d[[xc]]
  py     <- d[[yc]]
  r      <- sqrt(px^2 + py^2)

  ring <- findInterval(r, ring_breaks, rightmost.closed = TRUE)
  ring[ring == 0L | ring >= length(ring_breaks)] <- NA_integer_

  rel_angle <- (atan2(py, px) - target_angle + 2 * pi) %% (2 * pi)
  quadrant  <- floor((rel_angle + pi / 4) %% (2 * pi) / (pi / 2)) + 1L

  rows <- lapply(split(seq_len(nrow(d)), d[[id_col]]), function(ii) {
    r_sub <- ring[ii]
    q_sub <- quadrant[ii]
    id_sub <- d[[id_col]][ii]
    valid <- !is.na(r_sub)
    if (!any(valid)) return(NULL)
    total <- sum(valid)
    agg <- aggregate(
      list(n_frames = rep(1L, sum(valid))),
      by  = list(quadrant = q_sub[valid], ring = r_sub[valid]),
      FUN = sum
    )
    agg$id         <- as.character(id_sub[1L])
    agg$zone       <- paste0("Q", agg$quadrant, ".R", agg$ring)
    agg$quadrant   <- as.integer(agg$quadrant)
    agg$n_frames   <- as.integer(agg$n_frames)
    agg$proportion <- agg$n_frames / total
    agg[, c("id", "quadrant", "ring", "zone", "n_frames", "proportion")]
  })

  result <- do.call(rbind, Filter(Negate(is.null), rows))
  if (is.null(result)) {
    return(data.frame(id = character(), quadrant = integer(), ring = integer(),
                      zone = character(), n_frames = integer(),
                      proportion = numeric(), stringsAsFactors = FALSE))
  }
  result
}

#' Count entries into a goal zone for circular arena trajectories
#'
#' For each trial, counts the number of times the trajectory enters a circular
#' zone of radius `crossing_radius` centred on the goal location. Applicable to
#' any circular arena experiment with a defined goal (hidden platform in a water
#' maze, reward zone in an open-field, etc.).
#'
#' An "entry" is a `FALSE -> TRUE` transition in the `distance < crossing_radius`
#' sequence (ordered by time). An animal that starts inside the zone on the
#' first frame counts as one entry.
#'
#' @param x A [`TrajSet`] object with x/y (or rel_x/rel_y) columns registered.
#' @param target_angle Numeric. Radians. Direction of the goal from the arena centre.
#' @param target_radius Numeric. Distance of the goal from the arena centre.
#'   Default `1` (wall). Together with `target_angle` gives the goal position:
#'   `gx = target_radius * cos(target_angle)`, `gy = target_radius * sin(target_angle)`.
#' @param crossing_radius Numeric. Radius of the goal zone in unit-circle
#'   coordinates. Default `0.15` (15\% of arena radius; roughly a 10 cm platform
#'   in a 60 cm pool).
#' @param coords Character. `"absolute"` (default) or `"relative"`.
#'   See [zone_dwell()].
#'
#' @return A `data.frame` with one row per trial: `id` (character) and
#'   `n_entries` (integer).
#'
#' @examples
#' \dontrun{
#' # Water maze probe trial: former platform at 45 degrees (NE), at wall
#' entries <- count_goal_entries(ts, target_angle = pi / 4,
#'                               crossing_radius = 0.15)
#' # n_entries > 1 indicates memory of the platform location
#' }
#'
#' @seealso [zone_dwell()]
#' @export
count_goal_entries <- function(x, target_angle, target_radius = 1,
                               crossing_radius = 0.15,
                               coords = c("absolute", "relative")) {
  coords <- match.arg(coords)
  if (!is.numeric(crossing_radius) || length(crossing_radius) != 1L || crossing_radius <= 0)
    stop("crossing_radius must be a single positive number.")
  if (coords == "relative") {
    if (is.null(x@cols$rel_x) || is.null(x@cols$rel_y))
      stop("coords='relative' requires rel_x and rel_y registered in TrajSet@cols.")
    xc <- x@cols$rel_x
    yc <- x@cols$rel_y
  } else {
    xc <- x@cols$x
    yc <- x@cols$y
  }
  if (is.null(xc) || is.null(yc))
    stop("count_goal_entries: TrajSet needs x/y columns.")

  id_col <- x@cols$id
  d      <- x@data
  gx     <- target_radius * cos(target_angle)
  gy     <- target_radius * sin(target_angle)

  rows <- lapply(split(seq_len(nrow(d)), d[[id_col]]), function(ii) {
    dist   <- sqrt((d[[xc]][ii] - gx)^2 + (d[[yc]][ii] - gy)^2)
    inside <- dist < crossing_radius
    inside[is.na(inside)] <- FALSE
    n_entries <- sum(diff(c(FALSE, inside)) == 1L)
    data.frame(id = as.character(d[[id_col]][ii[1L]]), n_entries = as.integer(n_entries),
               stringsAsFactors = FALSE)
  })

  result <- do.call(rbind, rows)
  if (is.null(result)) {
    return(data.frame(id = character(), n_entries = integer(),
                      stringsAsFactors = FALSE))
  }
  result
}

# ---------------------------------------------------------------------------
# circ_summarise -- tidy grouped circular summary

.circ_summarise_one <- function(angles, stats, angle_convention, coords) {
  angles <- angles[is.finite(angles)]
  n      <- length(angles)
  uc_angles <- if (angle_convention == "clock") .clock_to_uc(angles, coords) else angles

  if (n == 0L) {
    uc_mu <- NA_real_
    R     <- NA_real_
    kap   <- NA_real_
  } else {
    tc    <- circular::circular(uc_angles, units = "radians", modulo = "2pi")
    uc_mu <- .wrap_to_2pi(as.numeric(circular::mean.circular(tc)))
    R     <- as.numeric(circular::rho.circular(tc))
    kap   <- if (n >= 3L) .est_kappa_safe(tc) else NA_real_
  }

  out_mu <- if (!is.na(uc_mu) && angle_convention == "clock") {
    if (coords == "relative") (-uc_mu) %% (2 * pi) else rad2clock(uc_mu)
  } else {
    uc_mu
  }

  row <- vector("list", length(stats))
  names(row) <- stats
  for (s in stats) {
    row[[s]] <- switch(s,
      n            = as.integer(n),
      mean_dir     = out_mu,
      mean_dir_deg = if (is.na(out_mu)) NA_real_ else out_mu * 180 / pi,
      resultant_R  = R,
      kappa        = kap
    )
  }
  row
}

#' Tidy circular summary of a grouped data frame
#'
#' Computes circular summary statistics from a data frame column of angles.
#' Supports grouped tibbles and an explicit \code{.by} argument, returning
#' one row per group. Output \code{mean_dir} is always in radians;
#' \code{mean_dir_deg} is the same value converted to degrees.
#'
#' @param data A data frame or grouped tibble.
#' @param col Unquoted or quoted name of the column containing angles.
#' @param units Units of the angle column: \code{"radians"} or
#'   \code{"degrees"}. No default -- must be specified explicitly. Values are
#'   converted to radians internally before computation; output is always
#'   in radians (\code{mean_dir}) or degrees (\code{mean_dir_deg}).
#'   A warning is issued when the value range appears inconsistent with the
#'   declared units (e.g. values > 2pi when \code{units = "radians"}).
#'   Suppress range warnings with
#'   \code{options(radiatR.check_units = FALSE)}.
#' @param .by Character vector of grouping column names. Overrides any
#'   \code{group_by()} groups on \code{data}.
#' @param stats Character vector selecting which statistics to compute. Order
#'   determines column order in the output. Valid values: \code{"n"},
#'   \code{"mean_dir"}, \code{"mean_dir_deg"}, \code{"resultant_R"},
#'   \code{"kappa"}. Default: all five.
#' @param angle_convention \code{"unit_circle"} (0 = East, CCW) or
#'   \code{"clock"} (0 = North, CW). When \code{NULL}, read from
#'   \code{attr(data, "angle_convention")}; defaults to \code{"unit_circle"}.
#' @param coords \code{"relative"} or \code{"absolute"}. Only used when
#'   \code{angle_convention = "clock"}. When \code{NULL}, read from
#'   \code{attr(data, "coords")}; defaults to \code{"absolute"}.
#'
#' @return An ungrouped \code{tibble} with group columns first followed by
#'   requested stat columns in the order given in \code{stats}.
#'
#' @examples
#' hd <- data.frame(heading = c(0, pi/4, pi/2), arc = c("a", "a", "b"))
#' circ_summarise(hd, heading, units = "radians")
#' circ_summarise(hd, heading, units = "radians", .by = "arc")
#' circ_summarise(hd, heading, units = "radians", .by = "arc",
#'                stats = c("n", "mean_dir"))
#'
#' @importFrom rlang ensym as_string
#' @importFrom tibble as_tibble
#' @importFrom circular circular mean.circular rho.circular
#' @export
circ_summarise <- function(data,
                           col,
                           units,
                           .by              = NULL,
                           stats            = c("n", "mean_dir", "mean_dir_deg",
                                               "resultant_R", "kappa"),
                           angle_convention = NULL,
                           coords           = NULL) {
  col_name <- rlang::as_string(rlang::ensym(col))
  if (!col_name %in% names(data))
    stop(sprintf("`col` column '%s' not found in data.", col_name))

  if (missing(units))
    stop("'units' must be specified: use \"radians\" or \"degrees\".\n",
         "  Hint: most behavioral data is recorded in degrees.")
  units <- match.arg(units, c("radians", "degrees"))

  valid_stats <- c("n", "mean_dir", "mean_dir_deg", "resultant_R", "kappa")
  unknown <- setdiff(stats, valid_stats)
  if (length(unknown))
    stop(sprintf("Unknown stats: '%s'. Valid values are: %s.",
                 paste(unknown, collapse = "', '"),
                 paste(valid_stats, collapse = ", ")))

  if (is.null(angle_convention))
    angle_convention <- if (!is.null(attr(data, "angle_convention")))
      attr(data, "angle_convention") else "unit_circle"
  angle_convention <- match.arg(angle_convention, c("unit_circle", "clock"))

  if (is.null(coords))
    coords <- if (!is.null(attr(data, "coords"))) attr(data, "coords") else "absolute"
  coords <- match.arg(coords, c("relative", "absolute"))

  .check_angle_units(data[[col_name]], units, col_name)
  if (units == "degrees") data[[col_name]] <- data[[col_name]] * pi / 180

  group_vars <- if (!is.null(.by)) {
    missing_by <- setdiff(.by, names(data))
    if (length(missing_by))
      stop(sprintf(".by column '%s' not found in data.", missing_by[1L]))
    .by
  } else if (inherits(data, "grouped_df")) {
    grp_attr <- attr(data, "groups")
    if (is.null(grp_attr)) character(0) else setdiff(names(grp_attr), ".rows")
  } else {
    character(0)
  }

  data_df <- as.data.frame(data)

  if (length(group_vars) == 0L) {
    srow <- .circ_summarise_one(data_df[[col_name]], stats, angle_convention, coords)
    return(tibble::as_tibble(as.data.frame(srow, stringsAsFactors = FALSE)))
  }

  split_key <- if (length(group_vars) == 1L) {
    data_df[[group_vars]]
  } else {
    interaction(data_df[group_vars], drop = TRUE, sep = "\001")
  }
  idx_list <- split(seq_len(nrow(data_df)), split_key, drop = TRUE)

  result_rows <- lapply(names(idx_list), function(k) {
    ii   <- idx_list[[k]]
    srow <- .circ_summarise_one(data_df[[col_name]][ii], stats, angle_convention, coords)
    krow <- data_df[ii[1L], group_vars, drop = FALSE]
    rownames(krow) <- NULL
    cbind(krow, as.data.frame(srow, stringsAsFactors = FALSE))
  })

  result <- do.call(rbind, result_rows)
  rownames(result) <- NULL
  tibble::as_tibble(result)
}

# ---- circ_dispersion ---------------------------------------------------------

#' Per-group circular dispersion statistics for a dense heading series
#'
#' Computes circular mean direction, mean resultant length \emph{R}, circular
#' standard deviation, and sample size for each group.  Designed for
#' within-trial summaries from \code{\link{pose_to_headings}} or
#' \code{derive_headings(\ldots, frame_select = "all")}, but accepts any data
#' frame with an angle column.
#'
#' @param hd Data frame containing headings in radians.
#' @param group_col Column(s) to group by (e.g. \code{"id"} for per-trial
#'   summaries).  \code{NULL} treats the entire data frame as one group.
#' @param angle_col Name of the heading column.  Default \code{"heading"}.
#' @return Data frame with columns \code{group_col} (if supplied),
#'   \code{mean_dir}, \code{resultant_R}, \code{circ_sd}, \code{n}.
#'   Circular standard deviation is \eqn{\sqrt{-2 \log R}}.
#' @export
circ_dispersion <- function(hd, group_col = NULL, angle_col = "heading") {
  stopifnot(is.data.frame(hd))
  if (!angle_col %in% names(hd))
    stop("circ_dispersion: column '", angle_col, "' not found")

  .one <- function(sub) {
    a <- as.numeric(sub[[angle_col]]); a <- a[is.finite(a)]
    if (!length(a))
      return(data.frame(mean_dir = NA_real_, resultant_R = NA_real_,
                        circ_sd  = NA_real_, n = 0L))
    S <- mean(sin(a)); C <- mean(cos(a))
    R <- sqrt(S^2 + C^2)
    data.frame(mean_dir    = atan2(S, C),
               resultant_R = R,
               circ_sd     = sqrt(-2 * log(max(R, .Machine$double.eps))),
               n           = length(a))
  }

  if (is.null(group_col)) return(.one(hd))

  groups <- unique(hd[[group_col]])
  rows <- lapply(groups, function(g) {
    r <- .one(hd[hd[[group_col]] == g, , drop = FALSE])
    r[[group_col]] <- g
    r[, c(group_col, "mean_dir", "resultant_R", "circ_sd", "n")]
  })
  do.call(rbind, rows)
}

# ---- sector_summary ----------------------------------------------------------

#' Proportion of time spent in angular sectors
#'
#' Bins heading angles into sectors and returns count and proportion per sector,
#' optionally grouped by trial or condition.  Useful for dwell-time analysis of
#' dense per-frame heading series (e.g. gaze direction from a tethered animal).
#'
#' @param hd Data frame containing headings in radians.
#' @param sectors Either a single integer (number of equal sectors spanning the
#'   full circle, default \code{8}) or a numeric vector of break points in
#'   radians.  Break points need not include \eqn{\pm\pi}; they are added
#'   automatically.
#' @param group_col Column(s) to group by.  \code{NULL} uses all rows.
#' @param angle_col Name of the heading column.  Default \code{"heading"}.
#' @return Data frame with columns \code{group_col} (if supplied),
#'   \code{sector} (degree label), \code{mid_angle} (sector midpoint in
#'   radians), \code{count}, \code{proportion}.
#' @export
sector_summary <- function(hd, sectors = 8L, group_col = NULL,
                            angle_col = "heading") {
  stopifnot(is.data.frame(hd))
  if (!angle_col %in% names(hd))
    stop("sector_summary: column '", angle_col, "' not found")

  if (length(sectors) == 1L && is.numeric(sectors)) {
    bk <- seq(-pi, pi, length.out = as.integer(sectors) + 1L)
  } else {
    bk <- sort(unique(c(-pi, as.numeric(sectors), pi)))
  }
  mids   <- (head(bk, -1L) + tail(bk, -1L)) / 2
  labels <- sprintf("%.0fdegrees", round(mids * 180 / pi))

  .wrap <- function(a) { a <- a %% (2*pi); a[a > pi] <- a[a > pi] - 2*pi; a }

  .one <- function(sub) {
    a    <- .wrap(as.numeric(sub[[angle_col]][is.finite(sub[[angle_col]])]))
    bins <- cut(a, breaks = bk, labels = labels, include.lowest = TRUE, right = FALSE)
    bins <- factor(bins, levels = labels)
    tbl  <- table(bins)
    data.frame(sector     = labels,
               mid_angle  = mids,
               count      = as.integer(tbl),
               proportion = as.numeric(tbl) / max(sum(tbl), 1L),
               stringsAsFactors = FALSE)
  }

  if (is.null(group_col)) return(.one(hd))

  groups <- unique(hd[[group_col]])
  rows <- lapply(groups, function(g) {
    r <- .one(hd[hd[[group_col]] == g, , drop = FALSE])
    r[[group_col]] <- g
    r[, c(group_col, "sector", "mid_angle", "count", "proportion")]
  })
  do.call(rbind, rows)
}

# ---- vonmises_fit ------------------------------------------------------------

#' Fit a von Mises distribution to per-group heading data
#'
#' Estimates the mean direction \eqn{\mu} and concentration \eqn{\kappa} of a
#' von Mises distribution via maximum likelihood, together with asymptotic
#' standard errors and a confidence interval on \eqn{\mu}.  Intended as a
#' parametric companion to \code{\link{circ_dispersion}}: where
#' \code{circ_dispersion} returns the empirical resultant length \emph{R} and
#' circular SD, \code{vonmises_fit} returns the MLE \eqn{\hat{\kappa}} with
#' its uncertainty.
#'
#' \eqn{\kappa = 0} corresponds to a uniform distribution (no preferred
#' direction); larger values indicate increasing concentration.  The confidence
#' interval on \eqn{\mu} uses a normal approximation and is unreliable for
#' \eqn{\kappa < 0.5} or small samples.
#'
#' @param hd Data frame containing headings in radians.
#' @param group_col Column(s) to group by.  \code{NULL} fits a single model to
#'   all rows.
#' @param angle_col Name of the heading column.  Default \code{"heading"}.
#' @param conf Confidence level for the interval on \eqn{\mu}.
#'   Default \code{0.95}.
#' @return Data frame with columns \code{group_col} (if supplied), \code{mu}
#'   (MLE mean direction, radians), \code{mu_deg} (degrees), \code{kappa}
#'   (MLE concentration), \code{se_mu}, \code{se_kappa} (asymptotic standard
#'   errors), \code{ci_lo} and \code{ci_hi} (\code{conf}-level interval on
#'   \eqn{\mu}, radians), \code{n}.
#' @export
vonmises_fit <- function(hd, group_col = NULL, angle_col = "heading",
                          conf = 0.95) {
  stopifnot(is.data.frame(hd))
  if (!angle_col %in% names(hd))
    stop("vonmises_fit: column '", angle_col, "' not found")
  stopifnot(conf > 0, conf < 1)

  z <- stats::qnorm((1 + conf) / 2)

  .one <- function(sub) {
    a <- as.numeric(sub[[angle_col]])
    a <- a[is.finite(a)]
    n <- length(a)
    na_row <- data.frame(
      mu = NA_real_, mu_deg = NA_real_, kappa = NA_real_,
      se_mu = NA_real_, se_kappa = NA_real_,
      ci_lo = NA_real_, ci_hi = NA_real_, n = n
    )
    if (n < 2L) return(na_row)
    fit <- tryCatch(
      circular::mle.vonmises(
        circular::circular(a, units = "radians", type = "angles")
      ),
      error = function(e) NULL
    )
    if (is.null(fit)) return(na_row)
    mu    <- as.numeric(fit$mu)
    kappa <- as.numeric(fit$kappa)
    se_mu <- as.numeric(fit$se.mu)
    se_k  <- as.numeric(fit$se.kappa)
    data.frame(
      mu       = mu,
      mu_deg   = mu * 180 / pi,
      kappa    = kappa,
      se_mu    = se_mu,
      se_kappa = se_k,
      ci_lo    = mu - z * se_mu,
      ci_hi    = mu + z * se_mu,
      n        = n
    )
  }

  if (is.null(group_col)) return(.one(hd))

  groups <- unique(hd[[group_col]])
  rows <- lapply(groups, function(g) {
    r <- .one(hd[hd[[group_col]] == g, , drop = FALSE])
    r[[group_col]] <- g
    r[, c(group_col, "mu", "mu_deg", "kappa", "se_mu",
          "se_kappa", "ci_lo", "ci_hi", "n")]
  })
  do.call(rbind, rows)
}

# ---- test_uniformity ---------------------------------------------------------

#' Per-group tests of circular uniformity
#'
#' Tests whether each group's headings are uniformly distributed (i.e. no
#' preferred direction), using any of four classical tests.  The Rayleigh test
#' (\code{"rayleigh"}, default) returns an exact numeric p-value; the other
#' three (\code{"kuiper"}, \code{"rao"}, \code{"watson"}) use look-up tables
#' and the \code{p_value} column contains the tabled significance level rather
#' than a continuous p-value.
#'
#' @param hd Data frame with a heading column in radians.
#' @param group_col Column to group by.  \code{NULL} tests the whole data frame
#'   as one group.
#' @param angle_col Heading column name.  Default \code{"heading"}.
#' @param test One of \code{"rayleigh"} (default), \code{"kuiper"},
#'   \code{"rao"}, or \code{"watson"}.
#' @return Tidy data frame with columns \code{group_col} (if supplied),
#'   \code{statistic}, \code{p_value}, \code{n}, \code{test}.
#' @export
test_uniformity <- function(hd, group_col = NULL, angle_col = "heading",
                             test = c("rayleigh", "kuiper", "rao", "watson")) {
  test <- match.arg(test)
  stopifnot(is.data.frame(hd))
  if (!angle_col %in% names(hd))
    stop("test_uniformity: column '", angle_col, "' not found")

  .run <- function(a_circ) {
    switch(test,
      rayleigh = {
        r <- circular::rayleigh.test(a_circ)
        list(statistic = as.numeric(r$statistic), p_value = as.numeric(r$p.value))
      },
      kuiper = {
        r <- circular::kuiper.test(a_circ)
        p <- if (!is.null(r$alpha) && r$alpha > 0) as.numeric(r$alpha) else NA_real_
        list(statistic = as.numeric(r$statistic), p_value = p)
      },
      rao = {
        r <- circular::rao.spacing.test(a_circ)
        p <- if (!is.null(r$alpha) && r$alpha > 0) as.numeric(r$alpha) else NA_real_
        list(statistic = as.numeric(r$statistic), p_value = p)
      },
      watson = {
        r <- circular::watson.test(a_circ)
        p <- if (!is.null(r$alpha) && r$alpha > 0) as.numeric(r$alpha) else NA_real_
        list(statistic = as.numeric(r$statistic), p_value = p)
      }
    )
  }

  .one <- function(sub) {
    a <- as.numeric(sub[[angle_col]]); a <- a[is.finite(a)]
    if (length(a) < 3L) return(NULL)
    a_c <- circular::circular(a, units = "radians", type = "angles")
    r   <- tryCatch(.run(a_c), error = function(e) NULL)
    if (is.null(r)) return(NULL)
    data.frame(statistic = r$statistic, p_value = r$p_value,
               n = length(a), test = test, stringsAsFactors = FALSE)
  }

  if (is.null(group_col)) return(.one(hd))
  if (!group_col %in% names(hd))
    stop("test_uniformity: '", group_col, "' not found")

  groups <- unique(hd[[group_col]])
  rows   <- lapply(groups, function(g) {
    r <- .one(hd[hd[[group_col]] == g, , drop = FALSE])
    if (is.null(r)) return(NULL)
    r[[group_col]] <- g
    r[, c(group_col, "statistic", "p_value", "n", "test")]
  })
  do.call(rbind, rows[!vapply(rows, is.null, logical(1L))])
}

# ---- test_mean_directions ----------------------------------------------------

#' Test whether groups share the same mean direction (Watson-Williams)
#'
#' Wraps \code{\link[circular]{watson.williams.test}} -- the circular analogue
#' of the parametric \emph{F}-test for equal means.  Assumes von Mises-
#' distributed data with equal concentrations across groups; if concentrations
#' differ substantially or the distribution is non-von-Mises, consider a
#' non-parametric alternative.
#'
#' @param hd Data frame with heading and group columns.
#' @param group_col Column identifying conditions or groups.
#' @param angle_col Heading column in radians.  Default \code{"heading"}.
#' @param pairwise Logical.  \code{FALSE} (default) returns a single omnibus
#'   test across all groups.  \code{TRUE} returns all pairwise comparisons.
#' @return Tidy data frame.  Omnibus result has columns \code{n_groups},
#'   \code{statistic}, \code{df1}, \code{df2}, \code{p_value}, \code{test}.
#'   Pairwise result additionally has \code{group1} and \code{group2}.
#' @export
test_mean_directions <- function(hd, group_col, angle_col = "heading",
                                  pairwise = FALSE) {
  stopifnot(is.data.frame(hd))
  for (col in c(angle_col, group_col))
    if (!col %in% names(hd))
      stop("test_mean_directions: column '", col, "' not found")

  groups    <- unique(hd[[group_col]])
  circ_list <- stats::setNames(lapply(groups, function(g) {
    a <- as.numeric(hd[[angle_col]][hd[[group_col]] == g])
    a <- a[is.finite(a)]
    circular::circular(a, units = "radians", type = "angles")
  }), as.character(groups))
  circ_list <- Filter(function(x) length(x) >= 2L, circ_list)
  if (length(circ_list) < 2L)
    stop("test_mean_directions: need >= 2 groups with >= 2 observations each")

  .ww <- function(lst) {
    r <- tryCatch(
      suppressWarnings(circular::watson.williams.test(lst)),
      error = function(e) NULL
    )
    if (is.null(r)) return(NULL)
    data.frame(
      statistic = as.numeric(r$statistic),
      df1       = as.integer(r$parameter["df1"]),
      df2       = as.integer(r$parameter["df2"]),
      p_value   = as.numeric(r$p.value),
      stringsAsFactors = FALSE
    )
  }

  if (!pairwise) {
    out <- .ww(circ_list)
    if (is.null(out)) stop("test_mean_directions: test failed")
    out$n_groups <- length(circ_list)
    out$test     <- "Watson-Williams"
    return(out[, c("n_groups", "statistic", "df1", "df2", "p_value", "test")])
  }

  pairs <- utils::combn(names(circ_list), 2L, simplify = FALSE)
  rows  <- lapply(pairs, function(p) {
    r <- .ww(circ_list[p])
    if (is.null(r)) return(NULL)
    cbind(data.frame(group1 = p[1L], group2 = p[2L],
                     stringsAsFactors = FALSE), r)
  })
  rows <- rows[!vapply(rows, is.null, logical(1L))]
  out  <- do.call(rbind, rows)
  out$test <- "Watson-Williams"
  out
}

# ---- test_concentration ------------------------------------------------------

#' Test whether groups share the same concentration (dispersion)
#'
#' Two tests are available:
#' \describe{
#'   \item{\code{parametric = TRUE} (default)}{Likelihood-ratio test for equal
#'   von Mises \eqn{\kappa} across groups (\code{equal.kappa.test}).  Returns
#'   a chi-squared statistic with \eqn{k-1} degrees of freedom.}
#'   \item{\code{parametric = FALSE}}{Wallraff's non-parametric test for equal
#'   angular dispersions -- no distributional assumption required.}
#' }
#'
#' @param hd Data frame with heading and group columns.
#' @param group_col Column identifying conditions or groups.
#' @param angle_col Heading column in radians.  Default \code{"heading"}.
#' @param parametric Logical.  \code{TRUE} (default) uses
#'   \code{equal.kappa.test}; \code{FALSE} uses \code{wallraff.test}.
#' @return One-row tidy data frame with \code{statistic}, \code{df} (parametric
#'   only), \code{p_value}, and \code{test}.
#' @export
test_concentration <- function(hd, group_col, angle_col = "heading",
                                parametric = TRUE) {
  stopifnot(is.data.frame(hd))
  for (col in c(angle_col, group_col))
    if (!col %in% names(hd))
      stop("test_concentration: column '", col, "' not found")

  a   <- as.numeric(hd[[angle_col]]); keep <- is.finite(a)
  a_c <- circular::circular(a[keep], units = "radians", type = "angles")
  grp <- hd[[group_col]][keep]

  if (parametric) {
    r <- tryCatch(
      suppressWarnings(circular::equal.kappa.test(a_c, grp)),
      error = function(e) stop("test_concentration (equal.kappa): ", e$message)
    )
    data.frame(statistic = as.numeric(r$statistic),
               df        = as.integer(r$df),
               p_value   = as.numeric(r$p.value),
               test      = "equal.kappa",
               stringsAsFactors = FALSE)
  } else {
    r <- tryCatch(
      circular::wallraff.test(a_c, grp),
      error = function(e) stop("test_concentration (wallraff): ", e$message)
    )
    data.frame(statistic = as.numeric(r$statistic),
               df        = NA_integer_,
               p_value   = as.numeric(r$p.value),
               test      = "wallraff",
               stringsAsFactors = FALSE)
  }
}
