# Circular statistics bridge helpers
#
# Users should rely on the `circular` package for low-level statistics such as
# means, resultant length, and dispersion. This module focuses on higher-level
# summaries that combine those primitives for trajectory sets.

#' Circular summary statistics for trajectories
#'
#' Computes per-trial or global circular statistics (mean direction, resultant
#' length, concentration) from the step-angle column of a `Tracks`.
#'
#' @param x A [`Tracks`] object.
#' @param w Character. Name of a weight column in `x@data`. When `NULL`
#'   (default), all steps are weighted equally.
#' @param by Character. `"id"` (default) returns one row per trial;
#'   `"global"` pools all observations into a single summary row.
#' @param axial Logical. Treat the angles as axial (bidirectional, mod-pi)
#'   data: statistics are computed via the angle-doubling method and the mean is
#'   reported as an axis in `[0, pi)` radians / `[0, 180)` degrees. Default
#'   `FALSE` (ordinary directional data).
#' @return A `data.frame` with columns `id`, `n`, `t_start`, `t_end`,
#'   `mean_dir` (radians, unit-circle convention, 0 to 2pi), `resultant_R`
#'   (0--1), and `kappa`
#'   (von Mises concentration; `NA` when estimation fails).
#'
#' @examples
#' \dontrun{
#' data(cpunctatus)
#' circ_summary(cpunctatus, by = "id")
#' circ_summary(cpunctatus, by = "global")
#' }
#'
#' @export
#' @importFrom circular mean.circular rho.circular circular
setGeneric("circ_summary", function(x, w = NULL, by = c("id","global"), axial = FALSE)
  standardGeneric("circ_summary"))

# Fold angles into the frame where a k-axial distribution becomes unimodal.
# axial = TRUE multiplies by 2 (bidirectional / mod-pi data); FALSE is a no-op
# (ordinary directional data). Internal; the public API exposes only `axial`.
.fold_angles <- function(theta, axial = FALSE) {
  if (isTRUE(axial)) (2 * theta) %% (2 * pi) else theta
}

# Map a mean computed in the folded frame back to the data frame: an axis in
# [0, pi) when axial, else the directional mean wrapped to [0, 2pi).
.unfold_mean <- function(mu_folded, axial = FALSE) {
  if (isTRUE(axial)) (mu_folded / 2) %% pi else mu_folded %% (2 * pi)
}

# Hermans-Rasson skewness weight (Landler, Ruxton & Malkemper 2019, BMC Ecology
# 19:30, doi:10.1186/s12898-019-0246-8).
.HR_BETA <- 2.895

# Hermans-Rasson T statistic of circular uniformity on a numeric radian vector.
# Sums a dispersion term |pi - |dtheta|| and a beta-weighted skewness term
# |sin(dtheta)| over unordered pairs. The skewness term is subtracted, matching
# the reference's HermansRasson2T form (verified against the CircMLE
# implementation of Landler et al. 2019). Per-pair and per-n centering constants
# are omitted because they are constant given n and cancel in the Monte-Carlo
# comparison; for fixed n this statistic is an exact affine transform of the
# reference T (correlation 1), so it yields an identical permutation p-value.
# More powerful than the Rayleigh test against multimodal / non-symmetric
# alternatives.
.hr_statistic <- function(theta) {
  n <- length(theta)
  if (n < 2L) return(0)
  d  <- outer(theta, theta, "-")
  dt <- d[upper.tri(d)]
  sum(abs(pi - abs(dt)) - .HR_BETA * abs(sin(dt)))
}

# Pycke (2010) omnibus statistic of circular uniformity on a numeric radian
# vector. Sums a pairwise kernel -2*log(2*(1-cos(theta_i - theta_j))) over
# unordered pairs, normalized by (n-1). Rotation-invariant; rejects in the
# upper tail (larger statistic = more evidence against uniformity). Verified
# against the reference C++ implementation in the `sphunif` package
# (cir_stat_Pycke) to full double precision.
.pycke_statistic <- function(theta) {
  n <- length(theta)
  if (n < 2L) return(0)
  d  <- outer(theta, theta, "-")
  dt <- d[upper.tri(d)]
  sum(-2 * log(2 * (1 - cos(dt)))) / (n - 1)
}

#' @rdname circ_summary
#' @export
setMethod("circ_summary", "Tracks", function(x, w = NULL, by = c("id","global"), axial = FALSE) {
  by <- match.arg(by)
  id <- x@cols$id; tm <- x@cols$time; th <- x@cols$angle
  wcol <- if (is.null(w)) x@cols$weight else w
  d <- as.data.frame(x)
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

    folded <- .fold_angles(theta_valid, axial)
    tc <- circular::circular(folded, units = "radians", modulo = "2pi")
    mu <- if (length(folded)) {
      circular::mean.circular(tc, na.rm = TRUE, weights = wts_valid)
    } else {
      NA_real_
    }

    if (length(folded)) {
      if (is.null(wts_valid)) {
        mean_cos <- mean(cos(folded))
        mean_sin <- mean(sin(folded))
      } else {
        w_norm <- wts_valid / sum(wts_valid)
        mean_cos <- sum(w_norm * cos(folded))
        mean_sin <- sum(w_norm * sin(folded))
      }
      R <- sqrt(mean_cos^2 + mean_sin^2)
    } else {
      R <- NA_real_
    }

    kap    <- .est_kappa_safe(tc, fallback = NA_real_, w = wts_valid)
    out_mu <- .unfold_mean(.wrap_to_2pi(as.numeric(mu)), axial)
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
#' to any circular-field analysis where spatial dwell time is of interest
#' (e.g. water maze, open-field, Drosophila preference assay).
#'
#' The target quadrant (Q1) is centred on `target_angle`; Q2--Q4 follow
#' counter-clockwise. Observations outside `max(ring_breaks)` are excluded from
#' both counts and the proportion denominator.
#'
#' @param x A [`Tracks`] object with x/y (or rel_x/rel_y) columns registered.
#' @param target_angle Numeric. Radians. Direction of the target zone from the
#'   origin. Q1 spans +/-45degrees around this angle.
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
      stop("coords='relative' requires rel_x and rel_y registered in Tracks@cols.")
    xc <- x@cols$rel_x
    yc <- x@cols$rel_y
  } else {
    xc <- x@cols$x
    yc <- x@cols$y
  }
  if (is.null(xc) || is.null(yc))
    stop("zone_dwell: Tracks needs x/y columns.")

  id_col <- x@cols$id
  d      <- as.data.frame(x)
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

#' Count entries into a goal zone for trajectories in a circular field
#'
#' For each trial, counts the number of times the trajectory enters a circular
#' zone of radius `crossing_radius` centred on the goal location. Applicable to
#' any circular-field analysis with a defined goal (e.g. the hidden platform in
#' a water maze, a reward zone in an open-field).
#'
#' An "entry" is a `FALSE -> TRUE` transition in the `distance < crossing_radius`
#' sequence (ordered by time). A trajectory that starts inside the zone on the
#' first frame counts as one entry.
#'
#' @param x A [`Tracks`] object with x/y (or rel_x/rel_y) columns registered.
#' @param target_angle Numeric. Radians. Direction of the goal from the origin.
#' @param target_radius Numeric. Distance of the goal from the origin.
#'   Default `1` (wall). Together with `target_angle` gives the goal position:
#'   `gx = target_radius * cos(target_angle)`, `gy = target_radius * sin(target_angle)`.
#' @param crossing_radius Numeric. Radius of the goal zone in unit-circle
#'   coordinates. Default `0.15` (15\% of the radius; roughly a 10 cm platform
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
      stop("coords='relative' requires rel_x and rel_y registered in Tracks@cols.")
    xc <- x@cols$rel_x
    yc <- x@cols$rel_y
  } else {
    xc <- x@cols$x
    yc <- x@cols$y
  }
  if (is.null(xc) || is.null(yc))
    stop("count_goal_entries: Tracks needs x/y columns.")

  id_col <- x@cols$id
  d      <- as.data.frame(x)
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

.circ_summarise_one <- function(angles, stats, display, axial = FALSE) {
  n_total   <- length(angles)
  angles    <- angles[is.finite(angles)]
  n         <- length(angles)
  n_missing <- n_total - n

  if (n == 0L) {
    uc_mu <- NA_real_; R <- NA_real_; kap <- NA_real_
  } else {
    folded <- .fold_angles(angles, axial)
    tc     <- circular::circular(folded, units = "radians", modulo = "2pi")
    uc_mu  <- .unfold_mean(.wrap_to_2pi(as.numeric(circular::mean.circular(tc))),
                           axial)
    R      <- as.numeric(circular::rho.circular(tc))
    kap    <- if (n >= 3L) .est_kappa_safe(tc) else NA_real_
  }

  row <- vector("list", length(stats))
  names(row) <- stats
  for (s in stats) {
    row[[s]] <- switch(s,
      n            = as.integer(n),
      n_total      = as.integer(n_total),
      n_missing    = as.integer(n_missing),
      mean_dir     = uc_mu,
      mean_dir_deg = if (is.na(uc_mu)) NA_real_ else .uc_angle_to_display(uc_mu, display),
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
#'   determines column order in the output. Valid values: \code{"n"} (count of
#'   valid, non-missing headings), \code{"n_total"} (group size including
#'   missing), \code{"n_missing"} (excluded, non-finite headings),
#'   \code{"mean_dir"}, \code{"mean_dir_deg"}, \code{"resultant_R"},
#'   \code{"kappa"}. Default: \code{"n"}, \code{"mean_dir"},
#'   \code{"mean_dir_deg"}, \code{"resultant_R"}, \code{"kappa"}
#'   (\code{"n_total"}/\code{"n_missing"} are opt-in).
#' @param display A [`circ_display`] object. When supplied, `mean_dir_deg` is
#'   converted using the display convention (clockwise, `zero` offset). When
#'   `NULL` (default), `mean_dir_deg` is the raw degree equivalent of the
#'   unit-circle radian angle.
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
#' @param axial Logical. Treat the angles as axial (bidirectional, mod-pi)
#'   data: statistics are computed via the angle-doubling method and the mean is
#'   reported as an axis in `[0, pi)` radians / `[0, 180)` degrees. Default
#'   `FALSE` (ordinary directional data).
#' @export
circ_summarise <- function(data,
                           col,
                           units,
                           .by     = NULL,
                           axial   = FALSE,
                           stats   = c("n", "mean_dir", "mean_dir_deg",
                                       "resultant_R", "kappa"),
                           display = circ_display()) {
  col_name <- rlang::as_string(rlang::ensym(col))
  if (!col_name %in% names(data))
    stop(sprintf("`col` column '%s' not found in data.", col_name))

  if (missing(units))
    stop("'units' must be specified: use \"radians\" or \"degrees\".\n",
         "  Hint: most behavioral data is recorded in degrees.")
  units <- match.arg(units, c("radians", "degrees"))

  valid_stats <- c("n", "n_total", "n_missing", "mean_dir", "mean_dir_deg",
                   "resultant_R", "kappa")
  unknown <- setdiff(stats, valid_stats)
  if (length(unknown))
    stop(sprintf("Unknown stats: '%s'. Valid values are: %s.",
                 paste(unknown, collapse = "', '"),
                 paste(valid_stats, collapse = ", ")))

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
    srow <- .circ_summarise_one(data_df[[col_name]], stats, display, axial)
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
    srow <- .circ_summarise_one(data_df[[col_name]][ii], stats, display, axial)
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
#' @param axial Logical. Treat the angles as axial (bidirectional, mod-pi)
#'   data: statistics are computed via the angle-doubling method and the mean is
#'   reported as an axis in `[0, pi)` radians / `[0, 180)` degrees. Default
#'   `FALSE` (ordinary directional data).
#' @export
circ_dispersion <- function(hd, group_col = NULL, angle_col = "heading",
                            axial = FALSE) {
  stopifnot(is.data.frame(hd))
  if (!angle_col %in% names(hd))
    stop("circ_dispersion: column '", angle_col, "' not found")

  .one <- function(sub) {
    a <- as.numeric(sub[[angle_col]]); a <- a[is.finite(a)]
    if (!length(a))
      return(data.frame(mean_dir = NA_real_, resultant_R = NA_real_,
                        circ_sd  = NA_real_, n = 0L))
    folded <- .fold_angles(a, axial)
    S <- mean(sin(folded)); C <- mean(cos(folded))
    R <- sqrt(S^2 + C^2)
    data.frame(mean_dir    = .unfold_mean(atan2(S, C), axial),
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
#' dense per-frame heading series (e.g. gaze direction from a tethered subject).
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
#' @param axial Logical; when `TRUE`, fit an axial (bidirectional, mod-pi)
#'   von Mises via the doubled-angle method: `mu`/`mu_deg` are the mean **axis**
#'   in [0, pi), `kappa` is the concentration about that axis (estimated in the
#'   doubled-angle frame), and `se_mu`/`ci_lo`/`ci_hi` are halved accordingly.
#'   Default `FALSE` (directional).
#' @return Data frame with columns \code{group_col} (if supplied), \code{mu}
#'   (MLE mean direction, radians), \code{mu_deg} (degrees), \code{kappa}
#'   (MLE concentration), \code{se_mu}, \code{se_kappa} (asymptotic standard
#'   errors), \code{ci_lo} and \code{ci_hi} (\code{conf}-level interval on
#'   \eqn{\mu}, radians), \code{n}.
#' @export
vonmises_fit <- function(hd, group_col = NULL, angle_col = "heading",
                          conf = 0.95, axial = FALSE) {
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
    a_fit <- .fold_angles(a, axial)
    fit <- tryCatch(
      circular::mle.vonmises(
        circular::circular(a_fit, units = "radians", type = "angles")
      ),
      error = function(e) NULL
    )
    if (is.null(fit)) return(na_row)
    mu_d    <- as.numeric(fit$mu)
    kappa   <- as.numeric(fit$kappa)
    se_mu_d <- as.numeric(fit$se.mu)
    se_k    <- as.numeric(fit$se.kappa)
    mu     <- if (isTRUE(axial)) .unfold_mean(mu_d, axial = TRUE) else mu_d
    se_mu  <- if (isTRUE(axial)) se_mu_d / 2 else se_mu_d
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

# ---- wrappedcauchy_fit -------------------------------------------------------

#' Fit a wrapped Cauchy distribution to per-group heading data
#'
#' Estimates the mean direction \eqn{\mu} and concentration \eqn{\rho} of a
#' wrapped Cauchy distribution via maximum likelihood.  The wrapped Cauchy has
#' heavier tails than the von Mises and is more appropriate for data with
#' outliers, weak or noisy directionality, or when a von Mises fit looks
#' visually poor on a rose diagram.
#'
#' \eqn{\rho = 0} is a uniform distribution (no preferred direction);
#' \eqn{\rho = 1} is a point mass (perfect concentration).  Unlike von Mises
#' \eqn{\kappa}, the wrapped Cauchy \eqn{\rho} is bounded to \eqn{[0, 1)}.
#'
#' Standard errors are not computed by \code{mle.wrappedcauchy}; check
#' \code{convergence} is the \code{\link[stats]{optim}} return code (0 = fully
#' converged; 1 = iteration limit reached but estimates are typically still
#' reliable).  For uncertainty
#' estimation use \code{\link{vonmises_fit}} with the same data and compare
#' model fits visually via \code{\link{add_vonmises_density}} and
#' \code{\link{add_wrappedcauchy_density}}.
#'
#' @param hd Data frame containing headings in radians.
#' @param group_col Column(s) to group by.  \code{NULL} fits a single model.
#' @param angle_col Name of the heading column.  Default \code{"heading"}.
#' @param axial Logical; when `TRUE`, fit an axial (bidirectional, mod-pi)
#'   wrapped Cauchy via the doubled-angle method: `mu`/`mu_deg` are the mean
#'   **axis** in [0, pi) and `rho` is the concentration about that axis
#'   (estimated in the doubled-angle frame). Default `FALSE` (directional).
#' @return Data frame with columns \code{group_col} (if supplied), \code{mu}
#'   (MLE mean direction, radians), \code{mu_deg} (degrees), \code{rho}
#'   (concentration, 0--1), \code{convergence} (0 = converged), \code{n}.
#' @seealso \code{\link{vonmises_fit}}, \code{\link{add_wrappedcauchy_density}}
#' @export
wrappedcauchy_fit <- function(hd, group_col = NULL, angle_col = "heading",
                              axial = FALSE) {
  stopifnot(is.data.frame(hd))
  if (!angle_col %in% names(hd))
    stop("wrappedcauchy_fit: column '", angle_col, "' not found")

  .one <- function(sub) {
    a <- as.numeric(sub[[angle_col]]); a <- a[is.finite(a)]; n <- length(a)
    na_row <- data.frame(mu = NA_real_, mu_deg = NA_real_, rho = NA_real_,
                         convergence = NA_integer_, n = n)
    if (n < 2L) return(na_row)
    a_fit <- .fold_angles(a, axial)
    fit <- tryCatch(
      circular::mle.wrappedcauchy(
        circular::circular(a_fit, units = "radians", type = "angles")
      ),
      error = function(e) NULL
    )
    if (is.null(fit)) return(na_row)
    mu_d <- as.numeric(fit$mu)
    mu   <- if (isTRUE(axial)) .unfold_mean(mu_d, axial = TRUE) else mu_d
    data.frame(mu          = mu,
               mu_deg      = mu * 180 / pi,
               rho         = as.numeric(fit$rho),
               convergence = as.integer(fit$convergence),
               n           = n)
  }

  if (is.null(group_col)) return(.one(hd))

  groups <- unique(hd[[group_col]])
  rows   <- lapply(groups, function(g) {
    r <- .one(hd[hd[[group_col]] == g, , drop = FALSE])
    r[[group_col]] <- g
    r[, c(group_col, "mu", "mu_deg", "rho", "convergence", "n")]
  })
  do.call(rbind, rows)
}

# ---- circ_cor ----------------------------------------------------------------

#' Circular correlation between headings and a covariate
#'
#' Computes the association between a heading (angle) series and either a
#' continuous linear covariate (\code{x_type = "linear"}, default) or a
#' second set of angles (\code{x_type = "circular"}).
#'
#' \strong{Circular-linear} (T-linear association, Mardia and Jupp 2000):
#' \deqn{r^2 = (r_{cx}^2 + r_{cy}^2 - 2 r_{cx} r_{cy} r_{xy}) / (1 - r_{xy}^2)}
#' where \eqn{r_{cx}}, \eqn{r_{cy}}, and \eqn{r_{xy}} are the Pearson
#' correlations of \eqn{x} with \eqn{\cos\theta} and \eqn{\sin\theta}, and of
#' \eqn{\cos\theta} with \eqn{\sin\theta}. \eqn{r} lies in \eqn{[0, 1]}; the
#' test statistic \eqn{n r^2} is approximately chi-squared with 2 degrees of
#' freedom under the null. Note: \eqn{r} is unsigned (association strength
#' only, not direction).
#'
#' \strong{Circular-circular} (Fisher's \eqn{\rho}, via
#' \code{\link[circular]{cor.circular}}): \eqn{r \in [-1, 1]}.
#'
#' @param hd Data frame containing the heading and covariate columns.
#' @param x_col Name of the covariate column.
#' @param angle_col Heading column in radians.  Default \code{"heading"}.
#' @param group_col Column to group by.  \code{NULL} uses all rows.
#' @param x_type \code{"linear"} (default) or \code{"circular"}.
#' @param test Logical; include hypothesis test.  Default \code{TRUE}.
#' @return Tidy data frame with columns \code{group_col} (if supplied),
#'   \code{r}, \code{n}, \code{type}, and when \code{test = TRUE} also
#'   \code{statistic}, \code{df}, \code{p_value}.
#' @export
circ_cor <- function(hd, x_col, angle_col = "heading",
                      group_col = NULL,
                      x_type = c("linear", "circular"),
                      test = TRUE) {
  x_type <- match.arg(x_type)
  stopifnot(is.data.frame(hd))
  for (col in c(angle_col, x_col))
    if (!col %in% names(hd))
      stop("circ_cor: column '", col, "' not found")

  .one <- function(sub) {
    theta <- as.numeric(sub[[angle_col]])
    xval  <- as.numeric(sub[[x_col]])
    ok    <- is.finite(theta) & is.finite(xval)
    theta <- theta[ok]; xval <- xval[ok]; n <- length(theta)
    na_row <- data.frame(r = NA_real_, n = n, type = x_type,
                         stringsAsFactors = FALSE)

    if (n < 4L) {
      if (test) { na_row$statistic <- NA_real_; na_row$df <- NA_real_
                  na_row$p_value  <- NA_real_ }
      return(na_row)
    }

    if (x_type == "linear") {
      r_cx <- stats::cor(cos(theta), xval)
      r_cy <- stats::cor(sin(theta), xval)
      r_xy <- stats::cor(cos(theta), sin(theta))
      denom <- 1 - r_xy^2
      if (!is.finite(denom) || abs(denom) < .Machine$double.eps)
        return(na_row)
      r2 <- (r_cx^2 + r_cy^2 - 2*r_cx*r_cy*r_xy) / denom
      r  <- sqrt(max(r2, 0))
      out <- data.frame(r = r, n = n, type = "circular-linear",
                        stringsAsFactors = FALSE)
      if (test) {
        stat <- n * r2
        out$statistic <- stat
        out$df        <- 2L
        out$p_value   <- 1 - stats::pchisq(stat, df = 2L)
      }
    } else {
      th_c <- circular::circular(theta, units = "radians", type = "angles")
      xc   <- circular::circular(xval,  units = "radians", type = "angles")
      res  <- tryCatch(
        circular::cor.circular(th_c, xc, test = test),
        error = function(e) NULL
      )
      if (is.null(res)) return(na_row)
      out <- data.frame(r = as.numeric(res$cor), n = n,
                        type = "circular-circular",
                        stringsAsFactors = FALSE)
      if (test) {
        out$statistic <- as.numeric(res$statistic)
        out$df        <- NA_integer_
        out$p_value   <- as.numeric(res$p.value)
      }
    }
    out
  }

  if (is.null(group_col)) return(.one(hd))
  if (!group_col %in% names(hd))
    stop("circ_cor: '", group_col, "' not found")

  groups <- unique(hd[[group_col]])
  cols   <- if (test) c(group_col, "r", "n", "type", "statistic", "df",
                        "p_value") else c(group_col, "r", "n", "type")
  rows <- lapply(groups, function(g) {
    r <- .one(hd[hd[[group_col]] == g, , drop = FALSE])
    r[[group_col]] <- g
    r[, cols]
  })
  do.call(rbind, rows)
}

# ---- test_uniformity ---------------------------------------------------------

# Monte-Carlo uniformity p-value for the table-based one-sample tests. Recomputes
# the chosen statistic on `n_sim` uniform samples of the same size; Kuiper's V,
# Watson's U^2, and Rao's spacing all reject uniformity in the upper tail.
.mc_uniformity_pvalue <- function(a, test, n_sim) {
  cc <- function(x) circular::circular(x, units = "radians", type = "angles")
  stat_fn <- switch(test,
    kuiper = function(x) as.numeric(suppressWarnings(circular::kuiper.test(cc(x))$statistic)),
    rao    = function(x) as.numeric(suppressWarnings(circular::rao.spacing.test(cc(x))$statistic)),
    watson = function(x) as.numeric(suppressWarnings(circular::watson.test(cc(x))$statistic))
  )
  n    <- length(a)
  t0   <- stat_fn(a)
  sims <- replicate(n_sim, stat_fn(stats::runif(n, 0, 2 * pi)))
  list(statistic = t0, p_value = (1 + sum(sims >= t0)) / (n_sim + 1))
}

#' Per-group tests of circular uniformity
#'
#' Tests whether each group's headings are uniformly distributed (i.e. no
#' preferred direction), using any of four classical tests.  The Rayleigh test
#' (\code{"rayleigh"}, default) returns an exact numeric p-value; the other
#' three (\code{"kuiper"}, \code{"rao"}, \code{"watson"}) use look-up tables,
#' so by default the \code{p_value} column contains the tabled significance
#' level rather than a continuous p-value. Set \code{p_method = "monte_carlo"}
#' to obtain a continuous p-value for those three by simulation instead (usable
#' with \code{p_adjust}).
#'
#' @param hd Data frame with a heading column in radians.
#' @param group_col Column to group by.  \code{NULL} tests the whole data frame
#'   as one group.
#' @param angle_col Heading column name.  Default \code{"heading"}.
#' @param test One of \code{"rayleigh"} (default), \code{"vtest"},
#'   \code{"kuiper"}, \code{"rao"}, \code{"watson"}, or \code{"hermans_rasson"}.
#'   \code{"vtest"} is the V-test: uniformity against a \emph{specified} mean
#'   direction \code{mu}, considerably more powerful than the Rayleigh test when
#'   a bearing is expected a priori. The Hermans-Rasson omnibus test (Landler,
#'   Ruxton & Malkemper 2019) is far more powerful than Rayleigh against
#'   multimodal / non-symmetric alternatives; its \code{p_value} is obtained by
#'   Monte-Carlo simulation.
#' @param mu Numeric scalar, radians. The expected mean direction for the V-test
#'   (\code{test = "vtest"}); required for that test and ignored otherwise.
#' @param p_adjust Multiple-comparison correction method passed to
#'   \code{\link[stats]{p.adjust}}.  Default \code{"none"}.  Applies only when
#'   \code{group_col} is supplied; a \code{p_value_adj} column is added to the
#'   result.  Recommended: \code{"BH"} (Benjamini-Hochberg) when testing many
#'   conditions.  Meaningful only for tests with continuous p-values: Rayleigh,
#'   the V-test, Hermans-Rasson, and any test run with
#'   \code{p_method = "monte_carlo"}.
#' @param p_method For \code{"kuiper"}, \code{"rao"}, and \code{"watson"} only:
#'   \code{"table"} (default) reports the tabled significance level from the
#'   \code{circular} package (or \code{NA} when the statistic falls outside the
#'   table); \code{"monte_carlo"} instead simulates \code{n_sim} uniform samples
#'   to obtain a continuous p-value. Ignored by the other tests.
#' @return Tidy data frame with columns \code{group_col} (if supplied),
#'   \code{statistic}, \code{p_value}, \code{n}, \code{test}, and
#'   \code{p_value_adj} (when \code{p_adjust != "none"}).
#' @param axial Logical. Treat the angles as axial (bidirectional, mod-pi)
#'   data: the uniformity test is run via the angle-doubling method (testing for
#'   an axis). Default `FALSE` (ordinary directional data).
#' @param n_sim Number of Monte-Carlo replicates for the \code{"hermans_rasson"}
#'   p-value and for \code{p_method = "monte_carlo"}. Default \code{9999}. Set
#'   the RNG seed with \code{\link{set.seed}} for reproducible p-values.
#' @references Landler, L., Ruxton, G.D. & Malkemper, E.P. (2019). The
#'   Hermans-Rasson test as a powerful alternative to the Rayleigh test for
#'   circular statistics in biology. BMC Ecology 19:30.
#'   \doi{10.1186/s12898-019-0246-8}.
#' @export
test_uniformity <- function(hd, group_col = NULL, angle_col = "heading",
                             test = c("rayleigh", "vtest", "kuiper", "rao",
                                      "watson", "hermans_rasson", "pycke"),
                             p_adjust = "none", axial = FALSE, n_sim = 9999L,
                             mu = NULL,
                             p_method = c("table", "monte_carlo")) {
  test <- match.arg(test)
  p_method <- match.arg(p_method)
  stopifnot(is.data.frame(hd))
  if (!angle_col %in% names(hd))
    stop("test_uniformity: column '", angle_col, "' not found")
  if (test == "vtest" && is.null(mu))
    stop("test_uniformity: test = \"vtest\" requires `mu` ",
         "(the expected mean direction, in radians).")

  mu_c <- if (test == "vtest")
    circular::circular(.fold_angles(mu, axial), units = "radians",
                       type = "angles") else NULL

  .run <- function(a_circ) {
    switch(test,
      rayleigh = {
        r <- circular::rayleigh.test(a_circ)
        list(statistic = as.numeric(r$statistic), p_value = as.numeric(r$p.value))
      },
      vtest = {
        r <- circular::rayleigh.test(a_circ, mu = mu_c)
        list(statistic = as.numeric(r$statistic), p_value = as.numeric(r$p.value))
      },
      kuiper = {
        if (p_method == "monte_carlo")
          .mc_uniformity_pvalue(as.numeric(a_circ), "kuiper", n_sim)
        else {
          r <- circular::kuiper.test(a_circ)
          p <- if (!is.null(r$alpha) && r$alpha > 0) as.numeric(r$alpha) else NA_real_
          list(statistic = as.numeric(r$statistic), p_value = p)
        }
      },
      rao = {
        if (p_method == "monte_carlo")
          .mc_uniformity_pvalue(as.numeric(a_circ), "rao", n_sim)
        else {
          r <- circular::rao.spacing.test(a_circ)
          p <- if (!is.null(r$alpha) && r$alpha > 0) as.numeric(r$alpha) else NA_real_
          list(statistic = as.numeric(r$statistic), p_value = p)
        }
      },
      watson = {
        if (p_method == "monte_carlo")
          .mc_uniformity_pvalue(as.numeric(a_circ), "watson", n_sim)
        else {
          r <- circular::watson.test(a_circ)
          p <- if (!is.null(r$alpha) && r$alpha > 0) as.numeric(r$alpha) else NA_real_
          list(statistic = as.numeric(r$statistic), p_value = p)
        }
      },
      hermans_rasson = {
        a    <- as.numeric(a_circ)
        t0   <- .hr_statistic(a)
        sims <- replicate(n_sim,
                  .hr_statistic(stats::runif(length(a), 0, 2 * pi)))
        p    <- (1 + sum(sims >= t0)) / (n_sim + 1)
        list(statistic = t0, p_value = p)
      },
      pycke = {
        a    <- as.numeric(a_circ)
        t0   <- .pycke_statistic(a)
        sims <- replicate(n_sim,
                  .pycke_statistic(stats::runif(length(a), 0, 2 * pi)))
        p    <- (1 + sum(sims >= t0)) / (n_sim + 1)
        list(statistic = t0, p_value = p)
      }
    )
  }

  .one <- function(sub) {
    a <- as.numeric(sub[[angle_col]]); a <- a[is.finite(a)]
    if (length(a) < 3L) return(NULL)
    a_c <- circular::circular(.fold_angles(a, axial), units = "radians", type = "angles")
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
  out <- do.call(rbind, rows[!vapply(rows, is.null, logical(1L))])
  if (p_adjust != "none")
    out$p_value_adj <- stats::p.adjust(out$p_value, method = p_adjust)
  out
}

# ---- test_mean_directions ----------------------------------------------------

# Between/within resultant-length ratio for a list of numeric angle vectors:
# (sum of per-group resultant lengths - pooled resultant length) divided by the
# within-group remainder. Larger = more separated mean directions. Constant df
# factors are dropped because they cancel under label permutation.
.mean_sep_stat <- function(vecs) {
  all_a <- unlist(vecs, use.names = FALSE)
  n     <- length(all_a)
  r_pool <- sqrt(sum(cos(all_a))^2 + sum(sin(all_a))^2)
  r_grp  <- sum(vapply(vecs, function(v)
    sqrt(sum(cos(v))^2 + sum(sin(v))^2), numeric(1L)))
  within <- n - r_grp
  if (within <= .Machine$double.eps) return(Inf)
  (r_grp - r_pool) / within
}

# Permutation p-value for equality of mean directions across a list of numeric
# angle vectors, shuffling group labels over the pooled sample.
.mean_perm_pvalue <- function(vecs, n_perm) {
  t0    <- .mean_sep_stat(vecs)
  sizes <- lengths(vecs)
  pool  <- unlist(vecs, use.names = FALSE)
  key   <- rep(seq_along(sizes), sizes)
  sims  <- replicate(n_perm,
    .mean_sep_stat(split(pool[sample.int(length(pool))], key)))
  (1 + sum(sims >= t0)) / (n_perm + 1)
}

#' Test whether groups share the same mean direction
#'
#' Two tests of equal mean direction across groups:
#' \describe{
#'   \item{\code{method = "watson_williams"} (default)}{
#'   \code{\link[circular]{watson.williams.test}} -- the circular analogue of
#'   the parametric \emph{F}-test for equal means. Assumes von Mises-distributed
#'   data with equal concentrations across groups.}
#'   \item{\code{method = "permutation"}}{A distribution-free permutation test
#'   using a between-/within-group resultant-length statistic, with the p-value
#'   obtained by shuffling group labels. Makes no von Mises assumption and is
#'   robust when Watson-Williams' \emph{F} calibration is doubtful (moderate
#'   concentration, non-von-Mises shape). Note it assumes exchangeability under
#'   the null, so like Watson-Williams it is most trustworthy when group
#'   concentrations are similar.}
#' }
#'
#' @param hd Data frame with heading and group columns.
#' @param group_col Column identifying conditions or groups.
#' @param angle_col Heading column in radians.  Default \code{"heading"}.
#' @param method One of \code{"watson_williams"} (default) or
#'   \code{"permutation"}.
#' @param pairwise Logical.  \code{FALSE} (default) returns a single omnibus
#'   test across all groups.  \code{TRUE} returns all pairwise comparisons.
#' @param p_adjust Multiple-comparison correction method passed to
#'   \code{\link[stats]{p.adjust}}.  Default \code{"none"}.  Applies only to
#'   the pairwise output; a \code{p_value_adj} column is added.  Strongly
#'   recommended when \code{pairwise = TRUE}: use \code{"BH"}
#'   (Benjamini-Hochberg) or \code{"holm"} (family-wise control).  Ignored
#'   for the omnibus test (single p-value, no adjustment needed).
#' @param n_perm Number of label permutations for
#'   \code{method = "permutation"}. Default \code{9999}. Ignored otherwise. Set
#'   the RNG seed with \code{\link{set.seed}} for reproducible p-values.
#' @return Tidy data frame.  Omnibus result has columns \code{n_groups},
#'   \code{statistic}, \code{df1}, \code{df2}, \code{p_value}, \code{test}
#'   (\code{df1}/\code{df2} are \code{NA} for the permutation test).
#'   Pairwise result additionally has \code{group1}, \code{group2}, and
#'   \code{p_value_adj} (when \code{p_adjust != "none"}).
#' @param axial Logical. Treat the angles as axial (bidirectional, mod-pi)
#'   data: the test is run via the angle-doubling method, comparing group axes.
#'   Default `FALSE` (ordinary directional data).
#' @seealso \code{\link{test_distributions}}, \code{\link{test_concentration}}
#' @export
test_mean_directions <- function(hd, group_col, angle_col = "heading",
                                  method = c("watson_williams", "permutation"),
                                  pairwise = FALSE, p_adjust = "none",
                                  axial = FALSE, n_perm = 9999L) {
  method <- match.arg(method)
  stopifnot(is.data.frame(hd))
  for (col in c(angle_col, group_col))
    if (!col %in% names(hd))
      stop("test_mean_directions: column '", col, "' not found")

  groups    <- unique(hd[[group_col]])
  circ_list <- stats::setNames(lapply(groups, function(g) {
    a <- as.numeric(hd[[angle_col]][hd[[group_col]] == g])
    a <- a[is.finite(a)]
    circular::circular(.fold_angles(a, axial), units = "radians", type = "angles")
  }), as.character(groups))
  circ_list <- Filter(function(x) length(x) >= 2L, circ_list)
  if (length(circ_list) < 2L)
    stop("test_mean_directions: need >= 2 groups with >= 2 observations each")

  test_label <- if (method == "watson_williams") "Watson-Williams" else "permutation"

  .one <- function(lst) {
    if (method == "watson_williams") {
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
    } else {
      vecs <- lapply(lst, as.numeric)
      data.frame(
        statistic = .mean_sep_stat(vecs),
        df1       = NA_integer_,
        df2       = NA_integer_,
        p_value   = .mean_perm_pvalue(vecs, n_perm),
        stringsAsFactors = FALSE
      )
    }
  }

  if (!pairwise) {
    out <- .one(circ_list)
    if (is.null(out)) stop("test_mean_directions: test failed")
    out$n_groups <- length(circ_list)
    out$test     <- test_label
    return(out[, c("n_groups", "statistic", "df1", "df2", "p_value", "test")])
  }

  pairs <- utils::combn(names(circ_list), 2L, simplify = FALSE)
  rows  <- lapply(pairs, function(p) {
    r <- .one(circ_list[p])
    if (is.null(r)) return(NULL)
    cbind(data.frame(group1 = p[1L], group2 = p[2L],
                     stringsAsFactors = FALSE), r)
  })
  rows <- rows[!vapply(rows, is.null, logical(1L))]
  out  <- do.call(rbind, rows)
  out$test <- test_label
  if (p_adjust != "none")
    out$p_value_adj <- stats::p.adjust(out$p_value, method = p_adjust)
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
#' @param axial Logical. Treat the angles as axial (bidirectional, mod-pi)
#'   data: the test is run via the angle-doubling method, comparing axial
#'   concentrations. Default `FALSE` (ordinary directional data).
#' @export
test_concentration <- function(hd, group_col, angle_col = "heading",
                                parametric = TRUE, axial = FALSE) {
  stopifnot(is.data.frame(hd))
  for (col in c(angle_col, group_col))
    if (!col %in% names(hd))
      stop("test_concentration: column '", col, "' not found")

  a   <- as.numeric(hd[[angle_col]]); keep <- is.finite(a)
  a_c <- circular::circular(.fold_angles(a[keep], axial), units = "radians", type = "angles")
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

# ---- test_distributions ------------------------------------------------------

# Map a Watson two-sample U^2 statistic to a tabled significance level. The
# `circular` package holds these critical values only inside print.watson.two.test
# and never returns a p-value; we reproduce the same bands here and report the
# upper edge of the band (e.g. 0.05 means 0.01 < p < 0.05). NA means p > 0.10.
.watson_two_pvalue <- function(u2) {
  if (!is.finite(u2))    return(NA_real_)
  if (u2 > 0.385)        return(0.001)
  if (u2 > 0.268)        return(0.01)
  if (u2 > 0.187)        return(0.05)
  if (u2 > 0.152)        return(0.10)
  NA_real_
}

# Permutation p-value for Watson's two-sample U^2, shuffling the pooled sample
# into the two group sizes. Continuous alternative to the tabled band above.
.watson_two_perm_pvalue <- function(x, y, n_perm) {
  cc <- function(z) circular::circular(z, units = "radians", type = "angles")
  stat_fn <- function(a, b)
    as.numeric(suppressWarnings(circular::watson.two.test(cc(a), cc(b))$statistic))
  pool <- c(as.numeric(x), as.numeric(y))
  nx   <- length(x); N <- length(pool)
  t0   <- stat_fn(x, y)
  sims <- replicate(n_perm, {
    idx <- sample.int(N, nx)
    stat_fn(pool[idx], pool[-idx])
  })
  (1 + sum(sims >= t0)) / (n_perm + 1)
}

# Run one distribution-equality test on a named list of circular vectors and
# return a data frame of (component, statistic, df, p_value). `rao` yields two
# rows (mean, dispersion); the others yield a single "distribution" row.
# For watson_two, p_method = "monte_carlo" replaces the tabled band with a
# permutation p-value using n_perm shuffles.
.dist_test_rows <- function(cl, method, p_method = "table", n_perm = 9999L) {
  switch(method,
    watson_wheeler = {
      r <- suppressWarnings(circular::watson.wheeler.test(cl))
      data.frame(component = "distribution",
                 statistic = as.numeric(r$statistic),
                 df        = as.numeric(r$parameter),
                 p_value   = as.numeric(r$p.value),
                 stringsAsFactors = FALSE)
    },
    watson_two = {
      r <- suppressWarnings(circular::watson.two.test(cl[[1L]], cl[[2L]]))
      u2 <- as.numeric(r$statistic)
      p  <- if (p_method == "monte_carlo")
        .watson_two_perm_pvalue(cl[[1L]], cl[[2L]], n_perm) else .watson_two_pvalue(u2)
      data.frame(component = "distribution",
                 statistic = u2,
                 df        = NA_real_,
                 p_value   = p,
                 stringsAsFactors = FALSE)
    },
    rao = {
      r <- suppressWarnings(circular::rao.test(cl))
      data.frame(component = c("mean", "dispersion"),
                 statistic = as.numeric(r$statistic),
                 df        = as.numeric(r$df),
                 p_value   = as.numeric(r$p.value),
                 stringsAsFactors = FALSE)
    }
  )
}

#' Test whether groups share the same circular distribution
#'
#' Compares the angular distributions of two or more groups. Complements
#' \code{\link{test_mean_directions}} (which tests only the mean direction) and
#' \code{\link{test_concentration}} (which tests only the spread) by testing the
#' distributions as a whole, or -- for \code{method = "rao"} -- the mean and the
#' dispersion jointly.
#'
#' \describe{
#'   \item{\code{"watson_wheeler"} (default)}{Mardia-Watson-Wheeler uniform-scores
#'   test: a non-parametric k-sample test that makes no von Mises assumption.
#'   Returns an exact (chi-squared) p-value.}
#'   \item{\code{"watson_two"}}{Watson's two-sample \eqn{U^2} test of homogeneity.
#'   Strictly two-sample: with more than two groups set \code{pairwise = TRUE}.
#'   The \code{circular} package returns only tabled critical values, so by
#'   default \code{p_value} is the upper edge of a tabled band (e.g. \code{0.05}
#'   means \eqn{0.01 < p < 0.05}) and is \code{NA} when \eqn{p > 0.10}. Set
#'   \code{p_method = "monte_carlo"} for a continuous permutation p-value.}
#'   \item{\code{"rao"}}{Rao's test for homogeneity of both mean directions and
#'   dispersions across k groups; returns two rows per comparison, one for each
#'   \code{component} (\code{"mean"}, \code{"dispersion"}).}
#' }
#'
#' @param hd Data frame with heading and group columns.
#' @param group_col Column identifying the groups to compare.
#' @param angle_col Heading column in radians. Default \code{"heading"}.
#' @param method One of \code{"watson_wheeler"} (default), \code{"watson_two"},
#'   or \code{"rao"}.
#' @param pairwise Logical. \code{FALSE} (default) runs a single omnibus test
#'   across all groups (\code{"watson_two"} then requires exactly two groups).
#'   \code{TRUE} runs every pairwise comparison.
#' @param p_adjust Multiple-comparison correction passed to
#'   \code{\link[stats]{p.adjust}}. Default \code{"none"}. Applies only to the
#'   pairwise output, where a \code{p_value_adj} column is added; \code{"BH"} or
#'   \code{"holm"} are recommended. Meaningless for the tabled
#'   \code{"watson_two"} p-values.
#' @param axial Logical. Treat the angles as axial (bidirectional, mod-pi) data:
#'   the test is run via the angle-doubling method. Default \code{FALSE}.
#' @param p_method For \code{method = "watson_two"} only: \code{"table"}
#'   (default) reports the tabled significance band; \code{"monte_carlo"} instead
#'   returns a continuous permutation p-value from \code{n_perm} label shuffles.
#'   Ignored by the other methods (which already give continuous p-values).
#' @param n_perm Number of label permutations for
#'   \code{p_method = "monte_carlo"}. Default \code{9999}. Set the RNG seed with
#'   \code{\link{set.seed}} for reproducible p-values.
#' @return A tidy data frame. Omnibus output has columns \code{method},
#'   \code{component}, \code{statistic}, \code{df}, \code{p_value},
#'   \code{n_groups}, \code{n}. Pairwise output replaces \code{n_groups} with
#'   \code{group1}/\code{group2} and adds \code{p_value_adj} when
#'   \code{p_adjust != "none"}.
#' @seealso \code{\link{test_mean_directions}}, \code{\link{test_concentration}},
#'   \code{\link{test_uniformity}}
#' @export
test_distributions <- function(hd, group_col, angle_col = "heading",
                               method = c("watson_wheeler", "watson_two", "rao"),
                               pairwise = FALSE, p_adjust = "none",
                               axial = FALSE,
                               p_method = c("table", "monte_carlo"),
                               n_perm = 9999L) {
  method   <- match.arg(method)
  p_method <- match.arg(p_method)
  stopifnot(is.data.frame(hd))
  for (col in c(angle_col, group_col))
    if (!col %in% names(hd))
      stop("test_distributions: column '", col, "' not found")

  groups    <- unique(hd[[group_col]])
  circ_list <- stats::setNames(lapply(groups, function(g) {
    a <- as.numeric(hd[[angle_col]][hd[[group_col]] == g])
    a <- a[is.finite(a)]
    circular::circular(.fold_angles(a, axial), units = "radians", type = "angles")
  }), as.character(groups))
  circ_list <- Filter(function(x) length(x) >= 2L, circ_list)
  if (length(circ_list) < 2L)
    stop("test_distributions: need >= 2 groups with >= 2 observations each")

  if (!pairwise) {
    if (method == "watson_two" && length(circ_list) != 2L)
      stop("test_distributions: method = \"watson_two\" is a two-sample test; ",
           "with more than two groups use pairwise = TRUE or ",
           "method = \"watson_wheeler\".")
    out <- .dist_test_rows(circ_list, method, p_method, n_perm)
    out$n_groups <- length(circ_list)
    out$n        <- sum(lengths(circ_list))
    out$method   <- method
    return(out[, c("method", "component", "statistic", "df", "p_value",
                   "n_groups", "n")])
  }

  pairs <- utils::combn(names(circ_list), 2L, simplify = FALSE)
  rows  <- lapply(pairs, function(p) {
    r <- .dist_test_rows(circ_list[p], method, p_method, n_perm)
    cbind(data.frame(group1 = p[1L], group2 = p[2L], stringsAsFactors = FALSE),
          r, n = sum(lengths(circ_list[p])))
  })
  out <- do.call(rbind, rows)
  out$method <- method
  out <- out[, c("method", "group1", "group2", "component", "statistic", "df",
                 "p_value", "n")]
  if (p_adjust != "none")
    out$p_value_adj <- stats::p.adjust(out$p_value, method = p_adjust)
  out
}

# ---- boot_mean_ci ------------------------------------------------------------

# Wrap angular deviations to (-pi, pi].
.wrap_to_pi <- function(x) ((x + pi) %% (2 * pi)) - pi

#' Bootstrap confidence interval for a mean direction
#'
#' Computes a nonparametric bootstrap confidence interval for the mean direction
#' of each group, resampling angles with replacement and taking the percentiles
#' of the bootstrap mean directions (centred on the sample mean). Unlike the
#' normal-approximation interval from \code{\link{vonmises_fit}}, it makes no
#' distributional assumption and stays usable at low concentration or small
#' sample size, where that approximation is unreliable.
#'
#' @param hd Data frame containing headings in radians.
#' @param group_col Column(s) to group by.  \code{NULL} (default) treats the
#'   whole data frame as one group.
#' @param angle_col Name of the heading column.  Default \code{"heading"}.
#' @param conf Confidence level. Default \code{0.95}.
#' @param R Number of bootstrap resamples. Default \code{999}. Set the RNG seed
#'   with \code{\link{set.seed}} for reproducible intervals.
#' @param axial Logical; when \code{TRUE}, treat the angles as axial
#'   (bidirectional, mod-pi): \code{mu}/\code{mu_deg} are the mean \strong{axis}
#'   in \eqn{[0, \pi)} and the interval is scaled accordingly. Default
#'   \code{FALSE} (directional).
#' @return Data frame with columns \code{group_col} (if supplied), \code{mu}
#'   (mean direction, radians), \code{mu_deg} (degrees), \code{ci_lo} and
#'   \code{ci_hi} (\code{conf}-level bootstrap interval on \code{mu}, radians;
#'   bounds may fall outside \eqn{[0, 2\pi)} to describe an arc),
#'   \code{resultant_R}, and \code{n}. Groups with fewer than two finite angles
#'   yield an all-\code{NA} interval.
#' @seealso \code{\link{vonmises_fit}}, \code{\link{circ_dispersion}}
#' @export
boot_mean_ci <- function(hd, group_col = NULL, angle_col = "heading",
                         conf = 0.95, R = 999L, axial = FALSE) {
  stopifnot(is.data.frame(hd))
  if (!angle_col %in% names(hd))
    stop("boot_mean_ci: column '", angle_col, "' not found")
  stopifnot(conf > 0, conf < 1, R >= 1L)

  alpha <- (1 - conf) / 2

  .one <- function(sub) {
    a <- as.numeric(sub[[angle_col]]); a <- a[is.finite(a)]; n <- length(a)
    if (n < 2L)
      return(data.frame(mu = NA_real_, mu_deg = NA_real_, ci_lo = NA_real_,
                        ci_hi = NA_real_, resultant_R = NA_real_, n = n))
    folded <- .fold_angles(a, axial)
    mu_hat <- atan2(mean(sin(folded)), mean(cos(folded)))
    dev <- vapply(seq_len(R), function(.) {
      s <- folded[sample.int(n, n, replace = TRUE)]
      .wrap_to_pi(atan2(mean(sin(s)), mean(cos(s))) - mu_hat)
    }, numeric(1L))
    q <- stats::quantile(dev, c(alpha, 1 - alpha), names = FALSE, type = 7)
    res_r <- sqrt(mean(cos(folded))^2 + mean(sin(folded))^2)
    mu <- .unfold_mean(.wrap_to_2pi(mu_hat), axial)
    scale <- if (isTRUE(axial)) 0.5 else 1
    data.frame(
      mu          = mu,
      mu_deg      = mu * 180 / pi,
      ci_lo       = mu + q[1L] * scale,
      ci_hi       = mu + q[2L] * scale,
      resultant_R = res_r,
      n           = n
    )
  }

  if (is.null(group_col)) return(.one(hd))
  if (!group_col %in% names(hd))
    stop("boot_mean_ci: '", group_col, "' not found")

  groups <- unique(hd[[group_col]])
  rows <- lapply(groups, function(g) {
    r <- .one(hd[hd[[group_col]] == g, , drop = FALSE])
    r[[group_col]] <- g
    r[, c(group_col, "mu", "mu_deg", "ci_lo", "ci_hi", "resultant_R", "n")]
  })
  do.call(rbind, rows)
}

# ---- test_symmetry -----------------------------------------------------------

# Pewsey (2002) large-sample reflective-symmetry statistic on a numeric radian
# vector: the standardised second central sine moment about the sample mean
# direction. Centred moments a_p / b_p and the mean resultant length r follow
# the notation of the AS.circular reference implementation. Returns NA when the
# sample is too small or the variance estimate is non-positive.
.symmetry_stat <- function(theta) {
  n <- length(theta)
  if (n < 4L) return(list(statistic = NA_real_, p_value = NA_real_))
  mu  <- atan2(mean(sin(theta)), mean(cos(theta)))
  cen <- theta - mu
  a2 <- mean(cos(2 * cen)); a3 <- mean(cos(3 * cen)); a4 <- mean(cos(4 * cen))
  b2 <- mean(sin(2 * cen)); r  <- mean(cos(cen))
  v  <- ((1 - a4) / 2 - 2 * a2 + (2 * a2 / r) * (a3 + a2 * (1 - a2) / r)) / n
  if (!is.finite(v) || v <= 0)
    return(list(statistic = NA_real_, p_value = NA_real_))
  ts <- abs(b2 / sqrt(v))
  list(statistic = ts, p_value = 2 * stats::pnorm(ts, lower.tail = FALSE))
}

#' Test a circular distribution for reflective symmetry
#'
#' Pewsey's (2002) omnibus test of reflective symmetry about an unspecified mean
#' direction, based on the standardised second central sine moment. The null
#' hypothesis is that the distribution is symmetric about its mean direction; a
#' small p-value indicates skewness (asymmetry). Useful as a precondition check
#' before applying methods that assume symmetry, such as a von Mises fit or the
#' Watson-Williams test.
#'
#' The test uses a large-sample normal approximation and can be liberal at small
#' \eqn{n} or low concentration; treat borderline results at small samples with
#' caution.
#'
#' @param hd Data frame with a heading column in radians.
#' @param group_col Column to group by. \code{NULL} (default) tests the whole
#'   data frame as one group.
#' @param angle_col Heading column name. Default \code{"heading"}.
#' @param p_adjust Multiple-comparison correction method passed to
#'   \code{\link[stats]{p.adjust}}. Default \code{"none"}. Applies only when
#'   \code{group_col} is supplied; a \code{p_value_adj} column is added.
#' @param axial Logical. Treat the angles as axial (bidirectional, mod-pi) data:
#'   symmetry is tested via the angle-doubling method. Default \code{FALSE}.
#' @return Tidy data frame with columns \code{group_col} (if supplied),
#'   \code{statistic} (the standardised \eqn{|\bar b_2|}), \code{p_value},
#'   \code{n}, \code{test}, and \code{p_value_adj} (when
#'   \code{p_adjust != "none"}). Groups with fewer than four finite angles yield
#'   \code{NA} statistics.
#' @references Pewsey, A. (2002). Testing circular symmetry. \emph{Canadian
#'   Journal of Statistics} 30(4), 591--600. \doi{10.2307/3316098}.
#' @seealso \code{\link{test_uniformity}}, \code{\link{vonmises_fit}}
#' @export
test_symmetry <- function(hd, group_col = NULL, angle_col = "heading",
                          p_adjust = "none", axial = FALSE) {
  stopifnot(is.data.frame(hd))
  if (!angle_col %in% names(hd))
    stop("test_symmetry: column '", angle_col, "' not found")

  .one <- function(sub) {
    a <- as.numeric(sub[[angle_col]]); a <- a[is.finite(a)]
    r <- .symmetry_stat(.fold_angles(a, axial))
    data.frame(statistic = r$statistic, p_value = r$p_value,
               n = length(a), test = "reflective_symmetry",
               stringsAsFactors = FALSE)
  }

  if (is.null(group_col)) return(.one(hd))
  if (!group_col %in% names(hd))
    stop("test_symmetry: '", group_col, "' not found")

  groups <- unique(hd[[group_col]])
  rows <- lapply(groups, function(g) {
    r <- .one(hd[hd[[group_col]] == g, , drop = FALSE])
    r[[group_col]] <- g
    r[, c(group_col, "statistic", "p_value", "n", "test")]
  })
  out <- do.call(rbind, rows)
  if (p_adjust != "none")
    out$p_value_adj <- stats::p.adjust(out$p_value, method = p_adjust)
  out
}

# ---- test_unimodality --------------------------------------------------------

# Likelihood-ratio statistic for unimodal (single von Mises) against bimodal
# (asymmetric two-component von Mises mixture): 2 * (logLik_bimodal -
# logLik_unimodal), floored at 0. Reuses vonmises_fit() and .fit_two_vm(). NULL
# when the unimodal fit fails. `mu`/`kappa` are the fitted unimodal parameters,
# used to simulate the parametric-bootstrap null.
.unimodality_lrt <- function(theta) {
  vm <- vonmises_fit(data.frame(heading = theta))
  if (is.na(vm$kappa)) return(NULL)
  th_c   <- circular::circular(theta, units = "radians", type = "angles")
  ll_uni <- sum(as.numeric(circular::dvonmises(
    th_c, mu = circular::circular(vm$mu), kappa = vm$kappa, log = TRUE)))
  bi     <- .fit_two_vm(theta)
  ll_bi  <- if (isTRUE(bi$converged)) bi$logLik else ll_uni
  list(lrt = max(0, 2 * (ll_bi - ll_uni)), mu = vm$mu, kappa = vm$kappa)
}

#' Test a circular sample for unimodality against bimodality
#'
#' A parametric-bootstrap likelihood-ratio test of a single von Mises
#' distribution (unimodal) against an asymmetric two-component von Mises mixture
#' (bimodal). The null hypothesis is unimodality; a small p-value is evidence of
#' a second mode.
#'
#' The mixture likelihood-ratio statistic has a non-standard null distribution (a
#' boundary / non-identifiability problem), so its p-value cannot be read from a
#' \eqn{\chi^2}. Instead it is obtained by a parametric bootstrap: \code{n_boot}
#' samples are simulated from the fitted von Mises, both models are refitted to
#' each, and the observed statistic is compared with the resulting null
#' distribution. This is deliberately \emph{not} a critical-bandwidth (Silverman)
#' test: the critical bandwidth's null distribution is not valid for circular
#' data (Fisher & Marron 2001), whereas this bootstrap is well calibrated.
#'
#' The test is computationally intensive -- every bootstrap replicate refits the
#' mixture -- so it is best run with a modest \code{n_boot}. Set the RNG seed
#' with \code{\link{set.seed}} for reproducible p-values. It reuses the fitters
#' behind \code{\link{circ_model_select}}, which offers a complementary
#' information-criterion view of the same unimodal / bimodal comparison.
#'
#' @param hd Data frame with a heading column in radians.
#' @param group_col Column to group by. \code{NULL} (default) tests the whole
#'   data frame as one group.
#' @param angle_col Heading column name. Default \code{"heading"}.
#' @param n_boot Number of parametric-bootstrap replicates. Default \code{199}.
#' @return Tidy data frame with columns \code{group_col} (if supplied),
#'   \code{statistic} (the likelihood-ratio statistic), \code{p_value}, \code{n},
#'   and \code{test}. Groups with fewer than ten finite angles, or where the
#'   von Mises fit fails, yield \code{NA} statistics.
#' @references Silverman, B.W. (1981). Using kernel density estimates to
#'   investigate multimodality. \emph{JRSS B} 43(1), 97--99. Fisher, N.I. &
#'   Marron, J.S. (2001). Mode testing via the excess mass estimate.
#'   \emph{Biometrika} 88(2), 499--517.
#' @seealso \code{\link{circ_model_select}}, \code{\link{test_symmetry}}
#' @export
test_unimodality <- function(hd, group_col = NULL, angle_col = "heading",
                             n_boot = 199L) {
  stopifnot(is.data.frame(hd))
  if (!angle_col %in% names(hd))
    stop("test_unimodality: column '", angle_col, "' not found")

  .one <- function(sub) {
    a <- as.numeric(sub[[angle_col]]); a <- a[is.finite(a)]; n <- length(a)
    na_row <- data.frame(statistic = NA_real_, p_value = NA_real_, n = n,
                         test = "unimodality_lrt", stringsAsFactors = FALSE)
    if (n < 10L) return(na_row)
    obs <- .unimodality_lrt(a)
    if (is.null(obs)) return(na_row)
    boot <- vapply(seq_len(n_boot), function(.) {
      sim <- as.numeric(circular::rvonmises(
        n, circular::circular(obs$mu), obs$kappa))
      b <- .unimodality_lrt(sim)
      if (is.null(b)) NA_real_ else b$lrt
    }, numeric(1L))
    n_ok <- sum(!is.na(boot))
    p <- (1 + sum(boot >= obs$lrt, na.rm = TRUE)) / (n_ok + 1)
    data.frame(statistic = obs$lrt, p_value = p, n = n,
               test = "unimodality_lrt", stringsAsFactors = FALSE)
  }

  if (is.null(group_col)) return(.one(hd))
  if (!group_col %in% names(hd))
    stop("test_unimodality: '", group_col, "' not found")

  groups <- unique(hd[[group_col]])
  rows <- lapply(groups, function(g) {
    r <- .one(hd[hd[[group_col]] == g, , drop = FALSE])
    r[[group_col]] <- g
    r[, c(group_col, "statistic", "p_value", "n", "test")]
  })
  do.call(rbind, rows)
}

# ---- boot_kappa_ci -----------------------------------------------------------

# Point estimates of concentration for a numeric radian vector (already folded
# for axial when relevant): von Mises kappa (est.kappa MLE, small-Rbar fallback)
# and the mean resultant length R. Returns c(kappa, R).
.boot_conc_estimates <- function(theta) {
  Rbar <- sqrt(mean(cos(theta))^2 + mean(sin(theta))^2)
  k <- .est_kappa_safe(
    circular::circular(theta, units = "radians", type = "angles"),
    fallback = .kappa_from_Rbar(Rbar))
  c(k, Rbar)
}

#' Bootstrap confidence intervals for circular concentration
#'
#' Computes nonparametric bootstrap confidence intervals for the concentration
#' of each group, for both the von Mises maximum-likelihood concentration
#' \eqn{\kappa} and the mean resultant length \eqn{R}. Angles are resampled with
#' replacement and the interval is the percentile envelope of the bootstrap
#' estimates. Unlike the normal-approximation \code{se_kappa} from
#' \code{\link{vonmises_fit}}, it makes no distributional assumption and stays
#' usable at low concentration or small sample size, where that approximation is
#' unreliable and where \eqn{\hat\kappa}/\eqn{\hat R} carry a known upward bias.
#'
#' The \code{kappa_bias}/\code{R_bias} columns report the bootstrap estimate of
#' that bias (bootstrap mean minus the sample estimate); the intervals
#' themselves are the raw percentile envelope and are not bias-shifted.
#'
#' @param hd Data frame containing headings in radians.
#' @param group_col Column(s) to group by. \code{NULL} (default) treats the
#'   whole data frame as one group.
#' @param angle_col Name of the heading column. Default \code{"heading"}.
#' @param conf Confidence level. Default \code{0.95}.
#' @param R Number of bootstrap resamples. Default \code{999}. Set the RNG seed
#'   with \code{\link{set.seed}} for reproducible intervals.
#' @param axial Logical; when \code{TRUE}, treat the angles as axial
#'   (bidirectional, mod-pi): concentration is estimated in the doubled-angle
#'   frame (about the axis) and, like \code{\link{vonmises_fit}}, is not
#'   rescaled. Default \code{FALSE} (directional).
#' @return Data frame with columns \code{group_col} (if supplied), \code{kappa}
#'   (von Mises MLE concentration), \code{kappa_ci_lo}/\code{kappa_ci_hi}
#'   (\code{conf}-level percentile CI on \eqn{\kappa}), \code{kappa_bias},
#'   \code{resultant_R} (mean resultant length in \eqn{[0,1]}),
#'   \code{R_ci_lo}/\code{R_ci_hi}, \code{R_bias}, and \code{n}. Groups with
#'   fewer than two finite angles yield an all-\code{NA} row.
#' @seealso \code{\link{boot_mean_ci}}, \code{\link{boot_kappa_contrast}},
#'   \code{\link{vonmises_fit}}, \code{\link{circ_dispersion}},
#'   \code{\link{test_concentration}}
#' @export
boot_kappa_ci <- function(hd, group_col = NULL, angle_col = "heading",
                          conf = 0.95, R = 999L, axial = FALSE) {
  stopifnot(is.data.frame(hd))
  if (!angle_col %in% names(hd))
    stop("boot_kappa_ci: column '", angle_col, "' not found")
  stopifnot(conf > 0, conf < 1, R >= 1L)

  probs <- c((1 - conf) / 2, 1 - (1 - conf) / 2)

  .one <- function(sub) {
    a <- as.numeric(sub[[angle_col]]); a <- a[is.finite(a)]; n <- length(a)
    if (n < 2L)
      return(data.frame(kappa = NA_real_, kappa_ci_lo = NA_real_,
                        kappa_ci_hi = NA_real_, kappa_bias = NA_real_,
                        resultant_R = NA_real_, R_ci_lo = NA_real_,
                        R_ci_hi = NA_real_, R_bias = NA_real_, n = n))
    folded <- .fold_angles(a, axial)
    pt <- .boot_conc_estimates(folded)
    boot <- vapply(seq_len(R), function(.) {
      .boot_conc_estimates(folded[sample.int(n, n, replace = TRUE)])
    }, numeric(2L))
    kq <- stats::quantile(boot[1L, ], probs, names = FALSE, type = 7)
    rq <- stats::quantile(boot[2L, ], probs, names = FALSE, type = 7)
    data.frame(
      kappa       = pt[1L],
      kappa_ci_lo = kq[1L],
      kappa_ci_hi = kq[2L],
      kappa_bias  = mean(boot[1L, ]) - pt[1L],
      resultant_R = pt[2L],
      R_ci_lo     = rq[1L],
      R_ci_hi     = rq[2L],
      R_bias      = mean(boot[2L, ]) - pt[2L],
      n           = n,
      row.names   = NULL
    )
  }

  if (is.null(group_col)) return(.one(hd))
  if (!group_col %in% names(hd))
    stop("boot_kappa_ci: '", group_col, "' not found")

  groups <- unique(hd[[group_col]])
  rows <- lapply(groups, function(g) {
    r <- .one(hd[hd[[group_col]] == g, , drop = FALSE])
    r[[group_col]] <- g
    r[, c(group_col, "kappa", "kappa_ci_lo", "kappa_ci_hi", "kappa_bias",
          "resultant_R", "R_ci_lo", "R_ci_hi", "R_bias", "n")]
  })
  do.call(rbind, rows)
}

#' Bootstrap confidence interval for a between-condition concentration contrast
#'
#' Contrasts the concentration of two (or more) groups via a stratified
#' nonparametric bootstrap: angles are resampled with replacement within each
#' group independently, and the contrast is recomputed on every resample. For a
#' pair of groups it reports the difference in von Mises concentration
#' (\eqn{\Delta\kappa}), the concentration ratio (\eqn{\kappa_1/\kappa_2}), and
#' the difference in mean resultant length (\eqn{\Delta R}), each with a
#' percentile confidence interval, plus a two-sided bootstrap p-value for
#' \eqn{\Delta\kappa = 0}. It is the assumption-light, estimation-focused
#' companion to \code{\link{test_concentration}} (parametric \code{equal.kappa}
#' / non-parametric Wallraff).
#'
#' The concentration ratio is skewed and can be unstable or non-finite when the
#' denominator concentration is near zero; \eqn{\Delta\kappa} is the more robust
#' default in the low-concentration regime.
#'
#' @param hd Data frame containing headings in radians.
#' @param group_col Grouping column. Required; must have at least two distinct
#'   values. With exactly two groups a single contrast row is returned; with
#'   more than two, every unordered pair is returned.
#' @param angle_col Name of the heading column. Default \code{"heading"}.
#' @param conf Confidence level. Default \code{0.95}.
#' @param R Number of bootstrap resamples. Default \code{999}. Set the RNG seed
#'   with \code{\link{set.seed}} for reproducible intervals.
#' @param axial Logical; when \code{TRUE}, treat the angles as axial and
#'   estimate concentration in the doubled-angle frame (see
#'   \code{\link{boot_kappa_ci}}). Default \code{FALSE}.
#' @return Data frame with one row per group pair: \code{group1}, \code{group2},
#'   \code{delta_kappa}, \code{delta_kappa_ci_lo}/\code{delta_kappa_ci_hi},
#'   \code{kappa_ratio}, \code{kappa_ratio_ci_lo}/\code{kappa_ratio_ci_hi},
#'   \code{delta_R}, \code{delta_R_ci_lo}/\code{delta_R_ci_hi}, \code{p_value}
#'   (two-sided bootstrap p for \eqn{\Delta\kappa = 0}), \code{n1}, \code{n2}.
#' @seealso \code{\link{boot_kappa_ci}}, \code{\link{test_concentration}}
#' @export
boot_kappa_contrast <- function(hd, group_col, angle_col = "heading",
                                conf = 0.95, R = 999L, axial = FALSE) {
  stopifnot(is.data.frame(hd))
  for (col in c(angle_col, group_col))
    if (!col %in% names(hd))
      stop("boot_kappa_contrast: column '", col, "' not found")
  stopifnot(conf > 0, conf < 1, R >= 1L)

  probs <- c((1 - conf) / 2, 1 - (1 - conf) / 2)

  groups <- unique(hd[[group_col]])
  if (length(groups) < 2L)
    stop("boot_kappa_contrast: need at least two groups in '", group_col, "'")

  # Per-group folded, finite angle vectors.
  angles <- lapply(groups, function(g) {
    a <- as.numeric(hd[[angle_col]][hd[[group_col]] == g])
    .fold_angles(a[is.finite(a)], axial)
  })
  names(angles) <- as.character(groups)

  pairs <- utils::combn(length(groups), 2L)

  .contrast <- function(ta, tb) {
    ea <- .boot_conc_estimates(ta); eb <- .boot_conc_estimates(tb)
    c(delta_kappa = ea[1L] - eb[1L],
      kappa_ratio = ea[1L] / eb[1L],
      delta_R     = ea[2L] - eb[2L])
  }

  rows <- lapply(seq_len(ncol(pairs)), function(j) {
    ia <- pairs[1L, j]; ib <- pairs[2L, j]
    ta <- angles[[ia]]; tb <- angles[[ib]]
    na <- length(ta);   nb <- length(tb)
    pt <- .contrast(ta, tb)
    boot <- vapply(seq_len(R), function(.) {
      .contrast(ta[sample.int(na, na, replace = TRUE)],
                tb[sample.int(nb, nb, replace = TRUE)])
    }, numeric(3L))
    dk <- stats::quantile(boot[1L, ], probs, names = FALSE, type = 7)
    kr <- stats::quantile(boot[2L, ], probs, names = FALSE, type = 7)
    dR <- stats::quantile(boot[3L, ], probs, names = FALSE, type = 7)
    p  <- min(1, 2 * min(mean(boot[1L, ] <= 0), mean(boot[1L, ] >= 0)))
    data.frame(
      group1 = groups[ia], group2 = groups[ib],
      delta_kappa = pt[1L],
      delta_kappa_ci_lo = dk[1L], delta_kappa_ci_hi = dk[2L],
      kappa_ratio = pt[2L],
      kappa_ratio_ci_lo = kr[1L], kappa_ratio_ci_hi = kr[2L],
      delta_R = pt[3L],
      delta_R_ci_lo = dR[1L], delta_R_ci_hi = dR[2L],
      p_value = p, n1 = na, n2 = nb,
      row.names = NULL, stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}
