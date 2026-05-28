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
#'   `mean_dir` (radians, 0 to 2π), `resultant_R` (0–1), and `kappa`
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
#' The target quadrant (Q1) is centred on `target_angle`; Q2–Q4 follow
#' counter-clockwise. Observations outside `max(ring_breaks)` are excluded from
#' both counts and the proportion denominator.
#'
#' @param x A [`TrajSet`] object with x/y (or rel_x/rel_y) columns registered.
#' @param target_angle Numeric. Radians. Direction of the target zone from the
#'   arena centre. Q1 spans ±45° around this angle.
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
# circ_summarise — tidy grouped circular summary

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
#' Computes circular summary statistics from any data frame column containing
#' angles in radians. Supports grouped tibbles and an explicit \code{.by}
#' argument, returning one row per group.
#'
#' @param data A data frame or grouped tibble.
#' @param col Unquoted or quoted name of the column containing angles (radians).
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
#' circ_summarise(hd, heading)
#' circ_summarise(hd, heading, .by = "arc")
#' circ_summarise(hd, heading, .by = "arc", stats = c("n", "mean_dir"))
#'
#' @importFrom rlang ensym as_string
#' @importFrom tibble as_tibble
#' @importFrom circular circular mean.circular rho.circular
#' @export
circ_summarise <- function(data,
                           col,
                           .by              = NULL,
                           stats            = c("n", "mean_dir", "mean_dir_deg",
                                               "resultant_R", "kappa"),
                           angle_convention = NULL,
                           coords           = NULL) {
  col_name <- rlang::as_string(rlang::ensym(col))
  if (!col_name %in% names(data))
    stop(sprintf("`col` column '%s' not found in data.", col_name))

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
