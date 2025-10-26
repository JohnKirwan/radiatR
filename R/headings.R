# Heading extraction rules and circular summaries for TrajSet
#
# Provides:
#   - derive_headings(TrajSet, rule=..., ...): per-trajectory (and optionally multi-event) headings
#   - circ_summary_headings(TrajSet, rule=..., group_by=...): circular summaries (mean direction, resultant length, etc.)
#   - gg helpers to overlay mean direction arrows with length equal to resultant R on polar plots
#   - rule registry & user-defined rules
#
# Dependencies: circular, ggplot2 (for plotting helpers)

#' @importFrom methods setGeneric setMethod
#' @importFrom circular circular mean.circular rho.circular
#' @importFrom ggplot2 ggplot aes geom_segment geom_point

# ---- utilities ---------------------------------------------------------------
`%||%` <- function(a, b) if (!is.null(a)) a else b
.wrap_to_2pi <- function(x) x %% (2*pi)
.as_circ <- function(theta) circular::circular(theta, units = "radians", type = "angles", modulo = "2pi", zero = 0)

.kappa_from_Rbar <- function(R) {
  out <- R
  low  <- R < 0.53
  mid  <- R >= 0.53 & R < 0.85
  high <- R >= 0.85
  out[low]  <- 2*R[low] + R[low]^3 + (5*R[low]^5)/6
  out[mid]  <- -0.4 + 1.39*R[mid] + 0.43/(1 - R[mid])
  out[high] <- 1/(R[high]^3 - 4*R[high]^2 + 3*R[high])
  out
}

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

# carry helper: add columns from nearest time per id
.carry_nearest <- function(res, data, idc, tc, cols) {
  if (is.null(cols) || !length(cols)) return(res)
  cols <- intersect(cols, names(data))
  if (!length(cols)) return(res)
  out <- res
  out[, cols] <- NA
  idx_by_id <- split(seq_len(nrow(data)), data[[idc]])
  for (i in seq_len(nrow(res))) {
    idv <- res$id[i]; tv <- res$time[i]
    idx <- idx_by_id[[as.character(idv)]]
    if (is.null(idx) || !length(idx)) next
    j <- idx[which.min(abs(data[[tc]][idx] - tv))]
    out[i, cols] <- data[j, cols, drop = FALSE]
  }
  out
}

.wrap_to_2pi <- function(x) x %% (2*pi)
.as_circ <- function(theta) circular::circular(theta, units = "radians", type = "angles", modulo = "2pi", zero = 0)

.kappa_from_Rbar <- function(R) {
  out <- R
  low  <- R < 0.53
  mid  <- R >= 0.53 & R < 0.85
  high <- R >= 0.85
  out[low]  <- 2*R[low] + R[low]^3 + (5*R[low]^5)/6
  out[mid]  <- -0.4 + 1.39*R[mid] + 0.43/(1 - R[mid])
  out[high] <- 1/(R[high]^3 - 4*R[high]^2 + 3*R[high])
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
#' @param x TrajSet
#' @param rule one of "crossing", "distal", "straight"
#' @param ... rule-specific parameters
#' @return data.frame with columns id, time (approx), heading (radians). For some rules there may be multiple headings per id.
#' @export
setGeneric(
  "derive_headings",
  function(
    x,
    rule = c("crossing","distal","straight","origin_mean","net","velocity_mean","window_net","goal_bias","pca_axis","ransac_straight","maxspeed_window","vm_fit","exit","entry","ring_tangent"),
    ...
  ) standardGeneric("derive_headings")
)

# ---- crossing rule -----------------------------------------------------------
# Picks vector between successive crossings of two radii circ0 < circ1 (default outward).
# Heading = atan2(p1 - p0). If multiple such pairs occur, return one row per pair unless first_only=TRUE.
.set_headings_crossing_one <- function(d, id, tc, xc, yc, circ0, circ1, direction = c("outward","inward"), first_only = FALSE) {
  direction <- match.arg(direction)
  r <- sqrt(d[[xc]]^2 + d[[yc]]^2)
  n <- nrow(d)
  out <- list(); k <- 1L
  i <- 1L
  while (i < n) {
    if (direction == "outward") {
      cond0 <- r[ i ] <= circ0 & r[i+1] >  circ0
      cond1 <- r[ i ] <= circ1 & r[i+1] >  circ1
    } else {
      cond0 <- r[ i ] >= circ0 & r[i+1] <  circ0
      cond1 <- r[ i ] >= circ1 & r[i+1] <  circ1
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
        heading <- atan2(c1[2] - c0[2], c1[1] - c0[1])
        out[[k]] <- data.frame(id = d[[id]][i],
                               time = mean(c(d[[tc]][i] + c0[3]*(d[[tc]][i+1]-d[[tc]][i]),
                                              d[[tc]][j] + c1[3]*(d[[tc]][j+1]-d[[tc]][j]))),
                               heading = .wrap_to_2pi(heading))
        k <- k + 1L
        i <- j + 1L
        if (first_only) break
        next
      }
    }
    i <- i + 1L
  }
  if (length(out)) do.call(rbind, out) else data.frame(id = d[[id]][1], time = d[[tc]][1], heading = NA_real_)
}

# ---- distal rule ------------------------------------------------------------- -------------------------------------------------------------
# Heading = angle of the most distal point (max radius from origin)
.set_headings_distal_one <- function(d, id, tc, xc, yc) {
  r <- sqrt(d[[xc]]^2 + d[[yc]]^2)
  m <- which.max(r)
  data.frame(id = d[[id]][m], time = d[[tc]][m], heading = .wrap_to_2pi(atan2(d[[yc]][m], d[[xc]][m])))
}

# ---- straight rule -----------------------------------------------------------
# Heading = angle of the longest contiguous segment where turning angle <= tol
# turning angle between steps i-1->i and i->i+1 via atan2(cross, dot)
.set_headings_straight_one <- function(d, id, tc, xc, yc, tol = pi/18, min_len = 5L) {
  n <- nrow(d)
  if (n < min_len + 1L) return(data.frame(id = d[[id]][1], time = d[[tc]][1], heading = NA_real_))
  vx <- diff(d[[xc]]); vy <- diff(d[[yc]])
  # angles between successive displacement vectors
  cross <- vx[-1]*vy[-length(vy)] - vy[-1]*vx[-length(vx)]
  dot   <- vx[-1]*vx[-length(vx)] + vy[-1]*vy[-length(vy)]
  turn  <- atan2(cross, dot)  # in (-pi, pi]
  ok <- is.finite(turn) & (abs(turn) <= tol)
  # run-length encoding to find longest ok-run
  r <- rle(ok)
  ends <- cumsum(r$lengths)
  starts <- ends - r$lengths + 1L
  idx_ok <- which(r$values)
  if (!length(idx_ok)) return(data.frame(id = d[[id]][1], time = d[[tc]][1], heading = NA_real_))
  # choose longest run meeting min_len-1 turning checks => segment length >= min_len
  lens <- r$lengths[idx_ok]
  ok_idx <- which.max(lens)
  if (lens[ok_idx] < (min_len - 1L)) return(data.frame(id = d[[id]][1], time = d[[tc]][1], heading = NA_real_))
  s_turn <- starts[idx_ok[ok_idx]]; e_turn <- ends[idx_ok[ok_idx]]
  s <- s_turn     # start step index
  e <- e_turn + 1 # end step index (because turn array shorter by 1)
  heading <- atan2(d[[yc]][s+1] - d[[yc]][s], d[[xc]][s+1] - d[[xc]][s])
  data.frame(id = d[[id]][s], time = d[[tc]][floor((s+e)/2)], heading = .wrap_to_2pi(heading))
}

# ---- origin_mean rule --------------------------------------------------------
# Heading = circular mean of position bearings from origin; optionally radius-weighted (equiv. angle of centroid when power=1)
.set_headings_origin_mean_one <- function(d, id, tc, xc, yc, r_power = 0) {
  th <- atan2(d[[yc]], d[[xc]])
  r  <- sqrt(d[[xc]]^2 + d[[yc]]^2)
  w  <- if (is.finite(r_power) && r_power != 0) r^r_power else NULL
  tcirc <- .as_circ(th)
  mu <- tryCatch(circular::mean.circular(tcirc, weights = w, na.rm = TRUE), error = function(e) NA_real_)
  data.frame(id = d[[id]][1], time = stats::median(d[[tc]], na.rm = TRUE), heading = .wrap_to_2pi(as.numeric(mu)))
}

# ---- net rule ----------------------------------------------------------------
# Heading = angle from first to last point (net displacement)
.set_headings_net_one <- function(d, id, tc, xc, yc) {
  s <- 1L; e <- nrow(d)
  heading <- atan2(d[[yc]][e] - d[[yc]][s], d[[xc]][e] - d[[xc]][s])
  data.frame(id = d[[id]][s], time = stats::median(d[[tc]], na.rm = TRUE), heading = .wrap_to_2pi(heading))
}

# ---- window_net rule ---------------------------------------------------------
# Sliding window net heading; returns one heading per window center
.set_headings_window_net_one <- function(d, id, tc, xc, yc, window_n = 21L, stride = 1L) {
  n <- nrow(d)
  if (n < 2L || window_n < 2L) return(data.frame(id = d[[id]][1], time = d[[tc]][1], heading = NA_real_))
  if (window_n > n) window_n <- n
  half <- floor(window_n/2)
  idx <- seq(1+half, n-half, by = max(1L, stride))
  out <- lapply(idx, function(i) {
    s <- i - half; e <- i + half
    heading <- atan2(d[[yc]][e] - d[[yc]][s], d[[xc]][e] - d[[xc]][s])
    data.frame(id = d[[id]][i], time = d[[tc]][i], heading = .wrap_to_2pi(heading))
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
  heading <- atan2(sy, sx)
  data.frame(id = d[[id]][1], time = stats::median(d[[tc]], na.rm = TRUE), heading = .wrap_to_2pi(heading))
}

# ---- pca_axis rule -----------------------------------------------------------
# Heading of first principal axis of positions or velocities; sign aligned with net displacement
.set_headings_pca_axis_one <- function(d, id, tc, xc, yc, source = c("position","velocity")) {
  source <- match.arg(source)
  if (source == "position") {
    M <- cbind(d[[xc]], d[[yc]])
    M <- scale(M, center = TRUE, scale = FALSE)
  } else {
    if (nrow(d) < 2L) return(data.frame(id = d[[id]][1], time = d[[tc]][1], heading = NA_real_))
    M <- cbind(diff(d[[xc]]), diff(d[[yc]]))
    M <- scale(M, center = TRUE, scale = FALSE)
  }
  if (nrow(M) < 2L) return(data.frame(id = d[[id]][1], time = d[[tc]][1], heading = NA_real_))
  S <- crossprod(M) / (nrow(M) - 1)
  ev <- eigen(S, symmetric = TRUE)$vectors[,1]
  # align sign with net displacement
  net <- c(d[[xc]][nrow(d)] - d[[xc]][1], d[[yc]][nrow(d)] - d[[yc]][1])
  if (sum(ev * net) < 0) ev <- -ev
  heading <- atan2(ev[2], ev[1])
  data.frame(id = d[[id]][1], time = stats::median(d[[tc]], na.rm = TRUE), heading = .wrap_to_2pi(heading))
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
  heading <- atan2(d[[yc]][e] - d[[yc]][s], d[[xc]][e] - d[[xc]][s])
  data.frame(id = d[[id]][center], time = d[[tc]][center], heading = .wrap_to_2pi(heading))
}

# ---- vm_fit rule -------------------------------------------------------------
# Von Mises fit to step headings; mu is heading; falls back to mean if MLE not available
.set_headings_vm_fit_one <- function(d, id, tc, xc, yc, weight_by = c("step_length","uniform")) {
  weight_by <- match.arg(weight_by)
  if (nrow(d) < 2L) return(data.frame(id = d[[id]][1], time = d[[tc]][1], heading = NA_real_))
  vx <- diff(d[[xc]]); vy <- diff(d[[yc]])
  th <- atan2(vy, vx)
  w  <- if (weight_by == "step_length") sqrt(vx^2 + vy^2) else NULL
  tcirc <- .as_circ(th)
  mu <- tryCatch({
      if ("mle.vonmises" %in% getNamespaceExports("circular")) {
        fit <- circular::mle.vonmises(tcirc, w = w)
        as.numeric(fit$mu)
      } else {
        as.numeric(circular::mean.circular(tcirc, weights = w, na.rm = TRUE))
      }
    }, error = function(e) as.numeric(circular::mean.circular(tcirc, weights = w, na.rm = TRUE)))
  data.frame(id = d[[id]][1], time = stats::median(d[[tc]], na.rm = TRUE), heading = .wrap_to_2pi(mu))
}

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
.set_headings_velocity_mean_one <- function(d, id, tc, xc, yc, weight_by = c("step_length","uniform")) {
  weight_by <- match.arg(weight_by)
  if (nrow(d) < 2L) return(data.frame(id = d[[id]][1], time = d[[tc]][1], heading = NA_real_))
  vx <- diff(d[[xc]]); vy <- diff(d[[yc]])
  th <- atan2(vy, vx)
  w  <- if (weight_by == "step_length") sqrt(vx^2 + vy^2) else NULL
  tcirc <- .as_circ(th)
  mu <- tryCatch(circular::mean.circular(tcirc, weights = w, na.rm = TRUE), error = function(e) NA_real_)
  data.frame(id = d[[id]][1], time = stats::median(d[[tc]], na.rm = TRUE), heading = .wrap_to_2pi(as.numeric(mu)))
}

# ---- method for TrajSet ------------------------------------------------------
#' @param first_only logical; if TRUE, return only the first matching heading per trajectory
#' @param carry optional character vector of columns from the source data to append via nearest time
#' @rdname derive_headings
#' @export
setMethod("derive_headings", "TrajSet", function(x, rule = c("crossing","distal","straight","origin_mean","net","velocity_mean","window_net","goal_bias","pca_axis","ransac_straight","maxspeed_window","vm_fit","exit","entry","ring_tangent"),
                                                  ..., first_only = FALSE, carry = NULL) {
  id <- x@cols$id; tc <- x@cols$time; xc <- x@cols$x; yc <- x@cols$y
  if (is.null(xc) || is.null(yc)) stop("derive_headings: TrajSet needs x/y columns for these rules.")
  d <- x@data
  sp <- split(seq_len(nrow(d)), d[[id]])

  # built-in dispatcher
  builtins <- c("crossing","distal","straight","origin_mean","net","velocity_mean","window_net","goal_bias","pca_axis","ransac_straight","maxspeed_window","vm_fit","exit","entry","ring_tangent")

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
    rule <- match.arg(rule)
    res <- switch(rule,
      crossing = {
        circ0 <- dots$circ0; circ1 <- dots$circ1; direction <- dots$direction %||% "outward"
        if (is.null(circ0) || is.null(circ1)) stop("crossing rule requires circ0 and circ1")
        do.call(rbind, lapply(sp, function(ii) .set_headings_crossing_one(d[ii, , drop = FALSE], id, tc, xc, yc,
                                                                          circ0 = circ0, circ1 = circ1,
                                                                          direction = direction,
                                                                          first_only = first_only)))
      },
      distal   = do.call(rbind, lapply(sp, function(ii) .set_headings_distal_one(d[ii, , drop = FALSE], id, tc, xc, yc))),
      straight = {
        tol <- dots$tol %||% (pi/18); min_len <- dots$min_len %||% 5L
        do.call(rbind, lapply(sp, function(ii) .set_headings_straight_one(d[ii, , drop = FALSE], id, tc, xc, yc,
                                                                          tol = tol, min_len = as.integer(min_len))))
      },
      origin_mean = {
        r_power <- dots$r_power %||% 0
        do.call(rbind, lapply(sp, function(ii) .set_headings_origin_mean_one(d[ii, , drop = FALSE], id, tc, xc, yc, r_power = r_power)))
      },
      net = do.call(rbind, lapply(sp, function(ii) .set_headings_net_one(d[ii, , drop = FALSE], id, tc, xc, yc))),
      velocity_mean = {
        weight_by <- dots$weight_by %||% "step_length"
        do.call(rbind, lapply(sp, function(ii) .set_headings_velocity_mean_one(d[ii, , drop = FALSE], id, tc, xc, yc, weight_by = weight_by)))
      },
      window_net = {
        window_n <- as.integer(dots$window_n %||% 21L); stride <- as.integer(dots$stride %||% 1L)
        do.call(rbind, lapply(sp, function(ii) .set_headings_window_net_one(d[ii, , drop = FALSE], id, tc, xc, yc, window_n = window_n, stride = stride)))
      },
      goal_bias = do.call(rbind, lapply(sp, function(ii) .set_headings_goal_bias_one(d[ii, , drop = FALSE], id, tc, xc, yc))),
      pca_axis = {
        source <- dots$source %||% "position"
        do.call(rbind, lapply(sp, function(ii) .set_headings_pca_axis_one(d[ii, , drop = FALSE], id, tc, xc, yc, source = source)))
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
        weight_by <- dots$weight_by %||% "step_length"
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
  res
})

# ---- circular summaries over headings ----------------------------------------
#' Circular statistics over derived headings
#' @param x TrajSet
#' @param rule heading derivation rule (passed to derive_headings)
#' @param group_by character vector of columns to group by (default = "id"). Use NULL for global.
#' @param ... parameters forwarded to derive_headings
#' @return data.frame with group columns + mean_dir, resultant_R, kappa, n
#' @export
circ_summary_headings <- function(x, rule = c("crossing","distal","straight","origin_mean","net","velocity_mean"), group_by = "id", ...) {
  rule <- match.arg(rule)
  hd <- derive_headings(x, rule = rule, ...)
  if (nrow(hd) == 0L || all(is.na(hd$heading))) {
    return(data.frame(mean_dir = NA_real_, resultant_R = NA_real_, kappa = NA_real_, n = 0L))
  }
  # group keys
  if (is.null(group_by)) {
    keys <- factor("all")
    grp_df <- data.frame(.grp = keys)
    hd$..grp <- keys
  } else {
    # ensure columns exist (only 'id' guaranteed)
    if (!all(group_by %in% names(hd))) {
      if (identical(group_by, "id")) {
        # ok; 'id' exists in hd
      } else stop("group_by contains unknown column(s) in derived heading table")
    }
    if (length(group_by) == 1L && group_by == "id") {
      hd$..grp <- hd$id
    } else {
      key <- interaction(hd[, group_by, drop = FALSE], drop = TRUE, lex.order = TRUE)
      hd$..grp <- key
    }
  }

  sp <- split(hd$heading, hd$..grp)
  res <- lapply(names(sp), function(g) {
    th <- sp[[g]]
    th <- th[is.finite(th)]
    if (!length(th)) return(data.frame(mean_dir = NA_real_, resultant_R = NA_real_, kappa = NA_real_, n = 0L))
    tc <- .as_circ(th)
    mu <- circular::mean.circular(tc, na.rm = TRUE)
    R  <- circular::rho.circular(tc,  na.rm = TRUE)
    kap <- .est_kappa_safe(tc, fallback = .kappa_from_Rbar(as.numeric(R)))
    data.frame(.grp = g, mean_dir = .wrap_to_2pi(as.numeric(mu)), resultant_R = as.numeric(R), kappa = as.numeric(kap), n = length(th))
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

# ---- plotting helpers: mean direction arrows ---------------------------------
#' Build a data frame of arrow segments representing mean direction vectors
#' Length equals resultant_R; angle equals mean_dir
#' @param stats_df output of circ_summary() or circ_summary_headings()
#' @param x0,y0 arrow origin (defaults 0,0)
#' @param scale multiply resultant_R by this factor when drawing
#' @return data.frame with x,y,xend,yend and any grouping columns kept
#' @export
circ_mean_segments <- function(stats_df, x0 = 0, y0 = 0, scale = 1) {
  stopifnot(all(c("mean_dir","resultant_R") %in% names(stats_df)))
  xend <- x0 + scale * stats_df$resultant_R * cos(stats_df$mean_dir)
  yend <- y0 + scale * stats_df$resultant_R * sin(stats_df$mean_dir)
  cbind(stats_df, x = x0, y = y0, xend = xend, yend = yend)
}

#' Add mean direction arrows to a ggplot polar plot
#' @param p ggplot built from gg_traj(...) or your own polar builder
#' @param segments_df data from circ_mean_segments()
#' @param color arrow color
#' @param linewidth segment linewidth
#' @param arrow_spec ggplot2::arrow(...) for arrow heads
#' @param inherit_aes whether to inherit aes (usually FALSE)
#' @export
gg_add_circ_mean <- function(p, segments_df, color = "black", linewidth = 0.8,
                             arrow_spec = ggplot2::arrow(length = grid::unit(0.02, "npc")),
                             inherit_aes = FALSE) {
  p + ggplot2::geom_segment(data = segments_df,
                           ggplot2::aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend),
                           color = color, linewidth = linewidth,
                           arrow = arrow_spec,
                           inherit.aes = inherit_aes)
}

# ---- user-defined rule registry ----------------------------------------------
#' Register a custom heading rule by name.
#' The function must accept (df, cols, ...) and return a data.frame with columns id, time, heading (radians)
#' @export
.heading_registry <- new.env(parent = emptyenv())
register_heading_rule <- function(name, fun, overwrite = FALSE) {
  stopifnot(is.character(name), length(name) == 1L, is.function(fun))
  if (exists(name, envir = .heading_registry, inherits = FALSE) && !overwrite)
    stop("A rule named '", name, "' already exists; set overwrite = TRUE to replace")
  assign(name, fun, envir = .heading_registry)
  invisible(name)
}

#' List registered custom heading rules
#' @export
list_heading_rules <- function() sort(ls(envir = .heading_registry))
