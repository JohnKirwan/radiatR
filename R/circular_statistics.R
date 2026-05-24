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

  d$.ring     <- ring
  d$.quadrant <- quadrant

  rows <- lapply(split(seq_len(nrow(d)), d[[id_col]]), function(ii) {
    sub <- d[ii, , drop = FALSE]
    sub <- sub[!is.na(sub$.ring), , drop = FALSE]
    if (nrow(sub) == 0L) return(NULL)
    total <- nrow(sub)
    agg <- aggregate(
      list(n_frames = rep(1L, nrow(sub))),
      by  = list(quadrant = sub$.quadrant, ring = sub$.ring),
      FUN = sum
    )
    agg$id         <- sub[[id_col]][1L]
    agg$zone       <- paste0("Q", agg$quadrant, ".R", agg$ring)
    agg$quadrant   <- as.integer(agg$quadrant)
    agg$n_frames   <- as.integer(agg$n_frames)
    agg$proportion <- agg$n_frames / total
    agg[, c("id", "quadrant", "ring", "zone", "n_frames", "proportion")]
  })

  do.call(rbind, Filter(Negate(is.null), rows))
}
