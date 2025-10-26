# Circular statistics helpers (means, resultant length, dispersion)
#

#' Calculate the circular mean of a set of angles in radians
#'
#' @param angles A numeric vector of angles in radians
#' @param normalize_range Logical, if TRUE (default), the output will be in the range (-pi, pi], otherwise in the range [0, 2 * pi)
#'
#' @return The circular mean angle in radians
#' @export
#'
#' @examples
#' angles <- c(0, pi, pi/2, 3*pi/2)
#' circular_mean(angles)
#' circular_mean(angles, normalize_range = FALSE)
circular_mean <- function(angles, normalize_range = TRUE) {
  if (!is.numeric(angles)) {
    stop("The input must be a numeric vector of angles in radians")
  }

  mean_cos <- mean(cos(angles))
  mean_sin <- mean(sin(angles))
  circular_mean_angle <- atan2(mean_sin, mean_cos)

  if (normalize_range) {
    if (circular_mean_angle <= -pi) {
      circular_mean_angle <- circular_mean_angle + 2 * pi
    }
  } else if (circular_mean_angle < 0) {
    circular_mean_angle <- circular_mean_angle + 2 * pi
  }

  circular_mean_angle
}

#' Calculate the mean resultant length of a set of angles in radians
#'
#' @param angles A numeric vector of angles in radians
#'
#' @return The mean resultant length (R)
#' @export
#'
#' @examples
#' angles <- c(0, pi, pi/2, 3*pi/2)
#' mean_resultant_length(angles)
mean_resultant_length <- function(angles) {
  if (!is.numeric(angles)) {
    stop("The input must be a numeric vector of angles in radians")
  }

  mean_cos <- mean(cos(angles))
  mean_sin <- mean(sin(angles))
  sqrt(mean_cos^2 + mean_sin^2)
}

#' Calculate the circular standard deviation using the mean resultant length (R)
#'
#' @param R The mean resultant length (R)
#'
#' @return The circular standard deviation
#' @export
#'
#' @examples
#' R <- mean_resultant_length(c(0, pi, pi/2, 3*pi/2))
#' circular_sd_from_R(R)
circular_sd_from_R <- function(R) {
  if (!is.numeric(R) || length(R) != 1) {
    stop("The input must be a single numeric value")
  }
  if (R <= 0 || R > 1) {
    stop("`R` must lie in the interval (0, 1].")
  }
  sqrt(-2 * log(R))
}

#' Circular summary statistics for trajectories
#'
#' @param x TrajSet input
#' @param w Optional weight column name
#' @param by Summarise per id or globally across all observations
#' @return data.frame with summary metrics per group
#' @export
#' @importFrom circular mean.circular rho.circular circular
setGeneric("circ_summary", function(x, w = NULL, by = c("id","global")) standardGeneric("circ_summary"))

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
setMethod("circ_summary", "TrajSet", function(x, w = NULL, by = c("id","global")) {
  by <- match.arg(by)
  id <- x@cols$id; tm <- x@cols$time; th <- x@cols$angle
  wcol <- if (is.null(w)) x@cols$weight else w
  d <- x@data
  split_idx <- if (by == "id") split(seq_len(nrow(d)), d[[id]]) else list(all = seq_len(nrow(d)))

  rows <- lapply(names(split_idx), function(k) {
    ii <- split_idx[[k]]
    theta <- d[[th]][ii]
    wts <- if (!is.null(wcol) && wcol %in% names(d)) d[[wcol]][ii] else NULL
    tc <- circular::circular(theta, units="radians", modulo="2pi")
    mu <- circular::mean.circular(tc, na.rm = TRUE, weights = wts)
    R  <- circular::rho.circular(tc,  na.rm = TRUE, weights = wts)
    kap <- .est_kappa_safe(tc, fallback = NA_real_, w = wts)
    data.frame(
      id          = if (by == "id") k else "global",
      n           = sum(!is.na(theta)),
      t_start     = d[[tm]][ii[1]],
      t_end       = d[[tm]][ii[length(ii)]],
      mean_dir    = .wrap_to_2pi(as.numeric(mu)),
      resultant_R = as.numeric(R),
      kappa       = as.numeric(kap),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
})
