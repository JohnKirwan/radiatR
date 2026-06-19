#' Derive polar and reference-relative coordinates from unit-circle position
#'
#' Given a unit-circle Cartesian position (`trans_x`, `trans_y`) and a reference
#' direction, computes the dependent coordinate columns: the radius, the absolute
#' angle (unit-circle and clock conventions), and the reference-relative angle and
#' Cartesian position. This is the single source of the unit-circle ->
#' polar/relative transformation used across the package.
#'
#' @param trans_x,trans_y Numeric vectors of unit-circle Cartesian coordinates
#'   (same length).
#' @param reference Reference direction in unit-circle radians; a scalar applied
#'   to all points, or a vector recycled per element. Default `0` (relative frame
#'   equals absolute frame).
#' @return A data frame with `trans_rho`, `abs_theta_clock`, `abs_theta_unit`,
#'   `rel_theta_unit`, `rel_x`, `rel_y`.
#' @seealso [set_reference()], [reference()]
#' @examples
#' derive_coords(c(0.5, -0.3), c(0.2, 0.4), reference = pi / 2)
#' @export
derive_coords <- function(trans_x, trans_y, reference = 0) {
  stopifnot(length(trans_x) == length(trans_y))
  trans_rho       <- sqrt(trans_x^2 + trans_y^2)
  abs_theta_clock <- rad2clock(atan2(trans_y, trans_x))
  abs_theta_unit  <- rad_unclock(abs_theta_clock)
  # Relative angle in the canonical [0, 2*pi) convention (the Tracks angle
  # column requires it). wrap_to_2pi(rad_shepherd(t)) == wrap_to_2pi(t), so this
  # matches the value the loader pipeline ultimately stores.
  rel_theta_unit  <- wrap_to_2pi(abs_theta_unit - reference)
  data.frame(
    trans_rho       = trans_rho,
    abs_theta_clock = abs_theta_clock,
    abs_theta_unit  = abs_theta_unit,
    rel_theta_unit  = rel_theta_unit,
    rel_x           = trans_rho * cos(rel_theta_unit),
    rel_y           = trans_rho * sin(rel_theta_unit)
  )
}

#' Per-trajectory reference direction of a Tracks
#'
#' The reference direction (unit-circle radians) against which each trajectory's
#' relative frame (`rel_theta`/`rel_x`/`rel_y`) is defined. Trajectories with no
#' recorded reference default to `0` (relative frame equals absolute frame).
#'
#' @param x A [`Tracks`].
#' @return For `reference()`, a data frame with `id` and `ref_theta` (or `NULL`
#'   when none is set).
#' @seealso [set_reference()], [derive_coords()]
#' @rdname reference
#' @export
setGeneric("reference", function(x) standardGeneric("reference"))

#' @rdname reference
#' @export
setMethod("reference", "Tracks", function(x) x@meta$reference)

# Named id -> ref_theta vector covering every trajectory; absent ids default 0.
.reference_lookup <- function(x) {
  all_ids <- as.character(ids(x))
  lut <- stats::setNames(rep(0, length(all_ids)), all_ids)
  ref <- x@meta$reference
  if (!is.null(ref) && nrow(ref)) {
    lut[as.character(ref$id)] <- ref$ref_theta
  }
  lut
}

#' Set the per-trajectory reference and re-derive the relative frame
#'
#' Updates each trajectory's reference direction and re-derives its relative
#' columns (`rel_theta`/`rel_x`/`rel_y`) from the unit-circle position via
#' [derive_coords()], so the relative frame stays consistent. The step is
#' recorded in [transform_history()]. This is the drift-safe way to change the
#' reference frame -- prefer it over a manual [apply_transform()].
#'
#' @param x A [`Tracks`] with position roles (`cols$x`/`cols$y`) and relative
#'   roles (`cols$angle`/`cols$rel_x`/`cols$rel_y`) registered.
#' @param value Reference direction(s) in unit-circle radians: a scalar applied
#'   to all trajectories, or a named numeric vector / two-column
#'   (`id`,`ref_theta`) data frame setting them per trajectory.
#' @return The updated [`Tracks`].
#' @seealso [reference()], [derive_coords()]
#' @rdname set_reference
#' @export
setGeneric("set_reference", function(x, value) standardGeneric("set_reference"))

#' @rdname set_reference
#' @export
setMethod("set_reference", "Tracks", function(x, value) {
  ids0 <- as.character(ids(x))
  if (is.data.frame(value)) {
    stopifnot(all(c("id", "ref_theta") %in% names(value)))
    new_ref <- stats::setNames(value$ref_theta, as.character(value$id))
  } else if (length(value) == 1L && is.null(names(value))) {
    new_ref <- stats::setNames(rep(as.numeric(value), length(ids0)), ids0)
  } else {
    new_ref <- stats::setNames(as.numeric(value), as.character(names(value)))
  }

  x@meta$reference <- tibble::tibble(id = names(new_ref),
                                     ref_theta = unname(new_ref))

  cols <- x@cols
  rel_roles <- list(cols$angle, cols$rel_x, cols$rel_y, cols$x, cols$y)
  if (any(vapply(rel_roles, is.null, logical(1)))) {
    warning("set_reference: relative-frame roles not registered; ",
            "reference updated but no relative columns re-derived.")
    return(x)
  }

  d   <- x@data
  idc <- cols$id
  has_rel_xy <- all(c(cols$rel_x, cols$rel_y) %in% names(d))
  for (id in unique(as.character(d[[idc]]))) {
    rows <- which(as.character(d[[idc]]) == id)
    ref  <- if (id %in% names(new_ref)) new_ref[[id]] else 0
    dc <- derive_coords(d[[cols$x]][rows], d[[cols$y]][rows], reference = ref)
    d[[cols$angle]][rows] <- dc$rel_theta_unit
    if (has_rel_xy) {
      d[[cols$rel_x]][rows] <- dc$rel_x
      d[[cols$rel_y]][rows] <- dc$rel_y
    }
  }
  x@data <- d
  methods::validObject(x)
  log_transform(x, step = "set_reference", params = list(value = value))
})
