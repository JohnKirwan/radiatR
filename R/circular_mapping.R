#' @title Circular coordinate utilities
#'
#' @description
#' A collection of helpers for converting between Cartesian coordinates and
#' unit-circle representations, including orientation conversions used
#' throughout the package. Most functions are internal, but the angle wrapping
#' helpers remain exported for backwards compatibility.
#'
#' @name circular_mapping
NULL

# ---- Angle wrapping -----------------------------------------------------------

#' Wrap angles to the interval (-pi, pi]
#'
#' @param theta Numeric vector of angles (radians).
#' @return Angles wrapped to (-pi, pi].
#' @examples
#' theta <- seq(from = -5, to = 5, length.out = 6)
#' rad_shepherd(theta)
#' @export
rad_shepherd <- function(theta) {
  theta <- as.numeric(theta)
  theta <- theta %% (2 * pi)
  theta[theta > pi] <- theta[theta > pi] - 2 * pi
  theta
}

#' Wrap angles to the interval [0, 2*pi)
#'
#' @param theta Numeric vector of angles (radians).
#' @return Angles wrapped to [0, 2*pi).
#' @examples
#' theta <- seq(from = -2, to = 8, length.out = 6)
#' rad_shepherd_clock(theta)
#' @export
rad_shepherd_clock <- function(theta) {
  theta <- as.numeric(theta)
  theta %% (2 * pi)
}

#' Convert unit-circle angles to clock orientation
#'
#' @param theta Angle in radians using the standard unit-circle convention.
#' @return Angle in radians measured clockwise with zero at the top.
#' @examples
#' theta <- seq(from = pi/2, to = -pi/2, length.out = 5)
#' rad2clock(theta)
#' @export
rad2clock <- function(theta) {
  theta <- as.numeric(theta)
  if (any(!is.finite(theta))) {
    warning("Non-finite values detected; returning NA for those entries.", call. = FALSE)
    theta[!is.finite(theta)] <- NA_real_
  }
  clock_theta <- (pi / 2) - theta
  rad_shepherd_clock(clock_theta)
}

#' Convert clock-oriented angles back to unit-circle orientation
#'
#' @param theta Angle (radians) measured clockwise with zero at the top.
#' @return Angle in radians using the standard unit-circle convention.
#' @examples
#' theta <- seq(from = 0, to = 2 * pi, length.out = 5)
#' rad_unclock(theta)
#' @export
rad_unclock <- function(theta) {
  theta <- as.numeric(theta)
  if (any(!is.finite(theta))) {
    warning("Non-finite values detected; returning NA for those entries.", call. = FALSE)
    theta[!is.finite(theta)] <- NA_real_
  }
  rad_shepherd((pi / 2) - theta)
}

# Internal wrappers -------------------------------------------------------------

wrap_to_2pi <- function(theta) {
  if (is.null(theta)) {
    return(theta)
  }
  twopi <- 2 * pi
  theta <- theta %% twopi
  theta[theta < 0] <- theta[theta < 0] + twopi
  theta
}

as_radians <- function(x, unit = c("radians", "degrees")) {
  unit <- match.arg(unit)
  if (unit == "degrees") {
    x <- x * pi / 180
  }
  wrap_to_2pi(as.numeric(x))
}

# ---- Cartesian/polar conversions ---------------------------------------------

cartesian_to_polar <- function(x, y, normalize = TRUE,
                               zero_tol = sqrt(.Machine$double.eps)) {
  stopifnot(length(x) == length(y))
  r <- sqrt(x^2 + y^2)
  zero <- r <= zero_tol | is.na(r)

  x_out <- x
  y_out <- y

  if (normalize) {
    x_out[!zero] <- x_out[!zero] / r[!zero]
    y_out[!zero] <- y_out[!zero] / r[!zero]
  }

  theta <- wrap_to_2pi(atan2(y_out, x_out))
  rho <- if (normalize) {
    out <- rep(NA_real_, length(r))
    out[!zero] <- 1
    out
  } else {
    r
  }

  list(theta = theta, rho = rho, x = x_out, y = y_out)
}

polar_to_cartesian <- function(theta, rho = 1) {
  if (length(rho) == 1L) {
    rho <- rep(rho, length(theta))
  } else if (length(theta) != length(rho)) {
    stop("`theta` and `rho` must have compatible lengths.")
  }
  list(
    x = rho * cos(theta),
    y = rho * sin(theta)
  )
}

# ---- Trial mapping -----------------------------------------------------------

#' Build a mapping from raw pixels to unit-circle coordinates
#'
#' @param origin Numeric vector of length 2 giving the circle origin (x, y).
#' @param reference Numeric vector of length 2 giving the landmark on the
#'   perimeter that defines the zero-degree heading.
#' @param flip_y Logical; if `TRUE` (default), invert the y-axis so positive
#'   values point upward (matching the unit-circle convention).
#' @return A list containing origin metadata, a forward mapping (`map`) that
#'   converts raw pixels to unit-circle coordinates, and an inverse mapping
#'   (`inverse`) that converts back to the original pixel space.
#' @keywords internal
build_unit_circle_mapping <- function(origin, reference, flip_y = TRUE) {
  origin <- as.numeric(origin)
  reference <- as.numeric(reference)
  if (length(origin) != 2L || length(reference) != 2L) {
    stop("`origin` and `reference` must be numeric vectors of length 2.")
  }

  ref_vec_x <- reference[1] - origin[1]
  ref_vec_y <- reference[2] - origin[2]
  if (flip_y) ref_vec_y <- -ref_vec_y
  radius <- sqrt(ref_vec_x^2 + ref_vec_y^2)
  if (!is.finite(radius) || radius == 0) {
    stop("Reference landmark must be distinct from origin (non-zero radius).")
  }

  stim_theta_unit <- wrap_to_2pi(atan2(ref_vec_y, ref_vec_x))
  stim_theta_clock <- rad2clock(stim_theta_unit)

  map_fn <- function(x, y) {
    stopifnot(length(x) == length(y))
    cx <- x - origin[1]
    cy <- y - origin[2]
    if (flip_y) cy <- -cy

    trans_x <- cx / radius
    trans_y <- cy / radius
    trans_rho <- sqrt(trans_x^2 + trans_y^2)

    abs_theta_clock <- rad2clock(atan2(trans_y, trans_x))
    abs_theta_unit <- rad_unclock(abs_theta_clock)
    rel_theta_unit <- rad_shepherd(abs_theta_unit - stim_theta_unit)

    list(
      trans_x = trans_x,
      trans_y = trans_y,
      trans_rho = trans_rho,
      abs_theta_clock = abs_theta_clock,
      abs_theta_unit = abs_theta_unit,
      rel_theta_unit = rel_theta_unit,
      rel_x = trans_rho * cos(rel_theta_unit),
      rel_y = trans_rho * sin(rel_theta_unit)
    )
  }

  # Convert unit-circle coordinates back to raw pixels.
  unmap_fn <- function(trans_x, trans_y) {
    stopifnot(length(trans_x) == length(trans_y))
    cx <- trans_x * radius
    cy <- trans_y * radius
    if (flip_y) cy <- -cy
    list(
      x = cx + origin[1],
      y = cy + origin[2]
    )
  }

  list(
    origin = origin,
    reference = reference,
    radius = radius,
    stim_theta_unit = stim_theta_unit,
    stim_theta_clock = stim_theta_clock,
    flip_y = flip_y,
    map = map_fn,
    inverse = unmap_fn
  )
}
