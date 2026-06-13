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
  rel_theta_unit  <- rad_shepherd(abs_theta_unit - reference)
  data.frame(
    trans_rho       = trans_rho,
    abs_theta_clock = abs_theta_clock,
    abs_theta_unit  = abs_theta_unit,
    rel_theta_unit  = rel_theta_unit,
    rel_x           = trans_rho * cos(rel_theta_unit),
    rel_y           = trans_rho * sin(rel_theta_unit)
  )
}
