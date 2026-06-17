# Circular plotting utilities for radiatR trajectories
#

# ---- display convention helpers ----------------------------------------------

#' Circular display convention specification
#'
#' Describes how unit-circle radian angles are rendered in plots and tables.
#' Pass to any display function as the `display` argument.
#'
#' @param zero UC angle (radians) that maps to display 0. Default `pi/2`
#'   (geographic North at top — standard compass/clock layout). Use `0` to
#'   put a stimulus that lies at East (positive rel_x) at the top.
#' @param clockwise Logical. `TRUE` (default) for clockwise-positive angles.
#' @param units `"degrees"` (default) or `"radians"` for table outputs and
#'   degree label annotations.
#' @return A `circ_display` list.
#' @export
circ_display <- function(zero = pi / 2,
                         clockwise = TRUE,
                         units = c("degrees", "radians")) {
  units <- match.arg(units)
  structure(list(zero = zero, clockwise = clockwise, units = units),
            class = "circ_display")
}

# Internal: rotate UC Cartesian point(s) (x, y) into display canvas space.
# zero=pi/2 CW (default) is identity — North is already up in standard ggplot.
# zero=0 CW reproduces the former .to_clock_display() for stimulus-at-East data.
.uc_to_display_coords <- function(x, y, display = circ_display()) {
  angle <- pi / 2 - display$zero
  cos_a <- cos(angle); sin_a <- sin(angle)
  x_rot <- cos_a * x - sin_a * y
  y_rot <- sin_a * x + cos_a * y
  if (!display$clockwise) x_rot <- -x_rot
  list(x = x_rot, y = y_rot)
}

# Internal: convert UC angle theta to display value.
# CW with zero=pi/2 → clock degrees (0=North, 90=East).
# CCW with zero=0   → UC degrees.
.uc_angle_to_display <- function(theta, display = circ_display()) {
  val <- if (display$clockwise) {
    (display$zero - theta) %% (2 * pi)
  } else {
    (theta - display$zero) %% (2 * pi)
  }
  if (display$units == "degrees") val * 180 / pi else val
}

# ---- annotation layers -------------------------------------------------------

#' Create evenly spaced radial tick marks.
#'
#' Generates a `geom_segment()` layer containing `n` evenly spaced tick marks
#' around the unit circle, each spanning a radial distance of `length`
#' straddling radius 1. The layer can be added to any ggplot.
#'
#' @param colour Tick colour. Default `"black"`.
#' @param linewidth Tick line width. Default `0.5`.
#' @param length Radial length of each tick, in data units. Default `0.1`.
#' @param n Number of evenly spaced ticks. Default `8L`.
#'
#' @return A `geom_segment()` layer.
#'
#' @examples
#' library(ggplot2)
#' ggplot() +
#'   coord_fixed() +
#'   add_ticks()
#' @export
add_ticks <- function(colour = "black", linewidth = 0.5, length = 0.1, n = 8L) {
  th    <- 2 * pi * (seq_len(n) - 1L) / n
  r_in  <- 1 - length / 2
  r_out <- 1 + length / 2
  tick_df <- data.frame(
    x = r_in * cos(th),   y = r_in * sin(th),
    xend = r_out * cos(th), yend = r_out * sin(th)
  )

  ggplot2::geom_segment(
    data    = tick_df,
    mapping = ggplot2::aes(
      x = .data$x,
      y = .data$y,
      xend = .data$xend,
      yend = .data$yend
    ),
    colour      = colour,
    linewidth   = linewidth,
    inherit.aes = FALSE
  )
}

#' Draw a circular guide.
#'
#' Creates a list of annotation layers that render a circle with the requested
#' radius and appearance. The returned list can be added directly to a ggplot.
#'
#' @param radius Radius of the circle, expressed in the same units as the plot coordinates.
#' @param circle_color Line colour for the circle.
#' @param circle_alpha Alpha transparency for the circle.
#' @param circle_size Line width for the circle.
#' @param linetype Line type for the circle.
#' @param colour ggplot-style alias for `circle_color`. If supplied, takes
#'   precedence over `circle_color`.
#' @param linewidth ggplot-style alias for `circle_size`. If supplied, takes
#'   precedence over `circle_size`.
#'
#' @return A list containing a single ggplot annotation layer.
#'
#' @importFrom ggplot2 annotate
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot() +
#'   coord_fixed() +
#'   add_circ(radius = 1)
add_circ <- function(radius = 1, circle_color = "grey60", circle_alpha = 1,
                     circle_size = 1, linetype = "solid",
                     colour = NULL, linewidth = NULL) {
  if (!is.null(colour))    circle_color <- colour      # ggplot-style aliases
  if (!is.null(linewidth)) circle_size  <- linewidth
  list(
    ggplot2::annotate(
      "path",
      x = radius * cos(seq(0, 2 * pi, length.out = 1000)),
      y = radius * sin(seq(0, 2 * pi, length.out = 1000)),
      color = circle_color, alpha = circle_alpha,
      linewidth = circle_size, linetype = linetype
    )
  )
}

#' Add multiple concentric circles to a ggplot object
#'
#' This function creates a list of layers for concentric circles with specified radii that can be added to a ggplot object.
#' The function takes optional arguments to customize the appearance of the circles.
#'
#' @param radii A vector of radii for the concentric circles (default is c(0.25, 0.5, 0.75))
#' @param circle_color The color of the circles (default is "grey40")
#' @param circle_alpha The transparency of the circles (default is 1)
#' @param circle_size The size of the circle lines (default is 1)
#' @return A list of layers for concentric circles
#'
#' @examples
#' library(ggplot2)
#' ggplot() + coord_fixed() + add_multiple_circles()
#'
#' @export
add_multiple_circles <- function(radii = c(0.25, 0.5, 0.75),
                                 circle_color = "grey20",
                                 circle_alpha = 1,
                                 circle_size = 0.5) {
  lapply(radii, function(radius) add_circ(radius, circle_color, circle_alpha, circle_size))
}

#' Add quadrant lines to a radial plot
#'
#' Draws two dashed lines through the centre of the unit circle -- one
#' horizontal (0degrees/180degrees) and one vertical (90degrees/270degrees) -- dividing the unit circle into
#' four quadrants. The lines extend to the circumference (unit circle).
#'
#' @param colour Line colour. Default `"grey60"`.
#' @param linewidth Line width. Default `0.5`.
#' @param linetype Line type. Default `"dashed"`.
#'
#' @return A `geom_segment()` layer.
#'
#' @examples
#' library(ggplot2)
#' ggplot() + coord_fixed() + add_quadrant_lines()
#' @export
add_quadrant_lines <- function(colour = "grey60", linewidth = 0.5, linetype = "dashed") {
  seg_df <- data.frame(
    x    = c(-1,  0),
    y    = c( 0, -1),
    xend = c( 1,  0),
    yend = c( 0,  1)
  )
  ggplot2::geom_segment(
    data        = seg_df,
    mapping     = ggplot2::aes(x = .data$x, y = .data$y,
                               xend = .data$xend, yend = .data$yend),
    colour      = colour,
    linewidth   = linewidth,
    linetype    = linetype,
    inherit.aes = FALSE
  )
}

#' Mark the centre of a radial plot
#'
#' Adds a single point at the origin `(0, 0)` -- a centre reference for sparse
#' themes where no crosshairs meet at the middle.
#'
#' @param colour Point colour. Default `"grey50"`.
#' @param size Point size. Default `1.5`.
#' @param shape Point shape. Default `16` (filled circle).
#' @param ... Further arguments passed to [ggplot2::geom_point()].
#' @return A `geom_point()` layer.
#' @examples
#' library(ggplot2)
#' ggplot() + coord_fixed() + add_circ() + add_origin_point()
#' @importFrom ggplot2 geom_point aes
#' @export
add_origin_point <- function(colour = "grey50", size = 1.5, shape = 16, ...) {
  ggplot2::geom_point(
    data        = data.frame(x = 0, y = 0),
    mapping     = ggplot2::aes(x = .data$x, y = .data$y),
    colour      = colour, size = size, shape = shape,
    inherit.aes = FALSE, ...
  )
}

# n radial lines from the origin to the rim, starting at `phase` (radians).
.radial_spokes <- function(n, phase = 0, colour = "grey60",
                           linewidth = 0.5, linetype = "solid") {
  if (is.null(n) || n < 1L) return(NULL)
  th <- phase + 2 * pi * (seq_len(n) - 1L) / n
  df <- data.frame(x = 0, y = 0, xend = cos(th), yend = sin(th))
  ggplot2::geom_segment(
    data    = df,
    mapping = ggplot2::aes(x = .data$x, y = .data$y,
                           xend = .data$xend, yend = .data$yend),
    colour  = colour, linewidth = linewidth, linetype = linetype,
    inherit.aes = FALSE
  )
}

#' Radial grid layers (the radial analogue of a Cartesian grid)
#'
#' Returns a list of layers -- an optional filled disc, concentric rings, and
#' radial spokes, in two weights (major/minor) -- to compose onto a radial
#' `radiate()` plot with `+`. The unit boundary (radius 1) is left to the
#' circumference ([add_circ()]); grid rings are interior only.
#'
#' @param rings_major,rings_minor Numeric radii (`<1`) of the major and minor
#'   rings. Defaults `0.5` and `c(0.25, 0.75)`.
#' @param spokes_major,spokes_minor Number of evenly spaced spokes for each
#'   weight. `4` major gives the quadrant crosshairs; `4` minor interleaves them
#'   as 45-degree diagonals. Minor spokes are offset from major by half the major
#'   spacing (`pi / spokes_major`), so their placement is defined relative to the
#'   major count.
#' @param colour,colour_minor Spoke/ring colour. `colour_minor` defaults to
#'   `colour`.
#' @param linewidth,linewidth_minor Line widths. `linewidth_minor` defaults to
#'   `0.5 * linewidth`.
#' @param linetype Line type for the spokes. Default `"solid"`.
#' @param disc_fill Fill colour for the background disc; `NA` (default) draws no
#'   disc.
#' @param origin Logical; add a centre dot ([add_origin_point()]). Default
#'   `FALSE`.
#' @param origin_colour,origin_size Centre-dot style.
#' @param n_pts Points used to approximate the disc outline. Default `200L`.
#' @return A list of ggplot2 layers.
#' @seealso [add_multiple_circles()], [add_quadrant_lines()], [add_origin_point()]
#' @examples
#' library(ggplot2)
#' ggplot() + coord_fixed() +
#'   add_radial_grid(disc_fill = "grey92", colour = "white") +
#'   add_circ()
#' @importFrom ggplot2 geom_polygon geom_segment aes
#' @export
add_radial_grid <- function(rings_major = 0.5, rings_minor = c(0.25, 0.75),
                            spokes_major = 4L, spokes_minor = 4L,
                            colour = "grey92", colour_minor = NULL,
                            linewidth = 0.5, linewidth_minor = NULL,
                            linetype = "solid", disc_fill = NA,
                            origin = FALSE, origin_colour = "grey50",
                            origin_size = 1.5, n_pts = 200L) {
  if (is.null(colour_minor))    colour_minor    <- colour
  if (is.null(linewidth_minor)) linewidth_minor <- 0.5 * linewidth
  layers <- list()

  # add_circ() returns list(layer) and add_multiple_circles() returns a
  # list-of-lists; flatten one level so `layers` is a flat list of layers.
  ring_layers <- function(radii, col, lw) {
    if (!length(radii)) return(list())
    do.call(c, add_multiple_circles(radii = radii, circle_color = col, circle_size = lw))
  }

  if (!is.null(disc_fill) && !is.na(disc_fill)) {
    th <- seq(0, 2 * pi, length.out = n_pts)
    layers <- c(layers, list(ggplot2::geom_polygon(
      data    = data.frame(x = cos(th), y = sin(th)),
      mapping = ggplot2::aes(x = .data$x, y = .data$y),
      fill = disc_fill, colour = NA, inherit.aes = FALSE)))
  }

  # minor first (drawn under major)
  layers <- c(layers, ring_layers(rings_minor, colour_minor, linewidth_minor))
  sp_min <- .radial_spokes(spokes_minor, phase = pi / max(spokes_major, 1L),
                           colour = colour_minor, linewidth = linewidth_minor,
                           linetype = linetype)
  if (!is.null(sp_min)) layers <- c(layers, list(sp_min))

  layers <- c(layers, ring_layers(rings_major, colour, linewidth))
  sp_maj <- .radial_spokes(spokes_major, phase = 0,
                           colour = colour, linewidth = linewidth,
                           linetype = linetype)
  if (!is.null(sp_maj)) layers <- c(layers, list(sp_maj))

  if (isTRUE(origin))
    layers <- c(layers, list(add_origin_point(colour = origin_colour, size = origin_size)))

  layers
}

#' Label the four diagonal directions.
#'
#' Provides a list of annotation layers that mark 45, 135, 225, and 315 degrees on a
#' unit circle.
#'
#' @param display A [`circ_display`] object. Default `circ_display()`. Supplies
#'   the label units when `units` is `NULL`.
#' @param colour Label colour. Default `"black"`.
#' @param units `"degrees"` (e.g. `45°`) or `"radians"` (e.g. `π/4`). When
#'   `NULL` (default) the units are taken from `display`.
#' @param size Label text size, in mm. Default `3.88` (ggplot2's default
#'   text size).
#' @param family Label font family. Default `""` (the device default).
#' @return A list of ggplot2 annotation layers.
#'
#' @examples
#' library(ggplot2)
#' ggplot() +
#'   coord_fixed() +
#'   degree_labs()
#' @export
degree_labs <- function(display = circ_display(), colour = "black",
                        units = NULL, size = 3.88, family = "") {
  if (is.null(units)) units <- display$units
  units <- match.arg(units, c("degrees", "radians"))
  diag_r      <- 0.85
  pos         <- list(c(diag_r,  diag_r), c(diag_r, -diag_r),
                      c(-diag_r, -diag_r), c(-diag_r,  diag_r))
  disp_angles <- c(45, 135, 225, 315)
  labels <- if (units == "radians") {
    vapply(disp_angles, .format_pi_deg, character(1))
  } else {
    paste0(disp_angles, "\U00B0")
  }
  mapply(function(p, lab) ggplot2::annotate("text", x = p[1], y = p[2],
                                            label = lab, colour = colour,
                                            size = size, family = family),
         pos, labels, SIMPLIFY = FALSE)
}

# Greatest common divisor, for reducing pi-fraction labels.
.gcd <- function(a, b) {
  a <- abs(a); b <- abs(b)
  while (b != 0) {
    t <- b
    b <- a %% b
    a <- t
  }
  a
}

# Format an angle given in degrees as a reduced fraction of pi using the Greek
# pi glyph, e.g. 45 -> "pi/4", 135 -> "3pi/4", 90 -> "pi/2", 180 -> "pi",
# 0 -> "0".
.format_pi_deg <- function(deg) {
  if (deg == 0) return("0")
  g   <- .gcd(deg, 180L)
  num <- deg / g
  den <- 180L / g
  pi_sym  <- "\u03C0"   # Greek small letter pi (ASCII-safe)
  numpart <- if (num == 1) pi_sym else paste0(num, pi_sym)
  if (den == 1) numpart else paste0(numpart, "/", den)
}

#' Make mean resultant length arrow
#'
#' Computes the circular mean direction and resultant length, returning a
#' `geom_segment()` layer that can be added to an existing ggplot.
#'
#' @param data Data frame containing the angle column.
#' @param angle_col Column containing angles in radians.
#' @param arrow_head_cm Length of the arrowhead in centimetres.
#' @param colour Colour of the arrow.
#' @param size Width of the arrow segment (applied to the geom's `linewidth`).
#' @return A `geom_segment()` layer.
#' @export
#' @importFrom ggplot2 geom_segment
#' @importFrom circular mean.circular rho.circular
#' @importFrom grid arrow unit
#' @importFrom rlang .data ensym as_string
#' @importFrom tibble tibble
#' @importFrom ggplot2 aes
directedness_arrow <- function(data, angle_col, arrow_head_cm = 0.2,
                               colour = "gray", size = 2) {
  if (missing(angle_col)) {
    stop("`angle_col` must be supplied and should reference a column of angles in radians.")
  }
  angle_sym <- rlang::ensym(angle_col)
  angles <- data[[rlang::as_string(angle_sym)]]
  if (is.null(angles) || length(angles) == 0) {
    warning("No data available to compute directedness arrow; skipping layer.", call. = FALSE)
    return(ggplot2::geom_blank())
  }
  if (inherits(angles, "circular")) {
    angles <- as.numeric(angles)
  }
  if (!is.numeric(angles)) {
    warning("`angle_col` must refer to a numeric column expressed in radians; skipping arrow layer.", call. = FALSE)
    return(ggplot2::geom_blank())
  }
  angles_circ <- .as_circ(angles)
  mean_angle <- as.numeric(circular::mean.circular(angles_circ))
  rho        <- as.numeric(circular::rho.circular(angles_circ))

  # When resultant length is zero the mean direction is undefined; keep tip at origin.
  if (!is.finite(rho) || !is.finite(mean_angle) || rho == 0) {
    xend <- 0; yend <- 0
  } else {
    xend <- rho * cos(mean_angle)
    yend <- rho * sin(mean_angle)
  }

  arrow_df <- tibble::tibble(x = 0, y = 0, xend = xend, yend = yend)

  ggplot2::geom_segment(
    data = arrow_df,
    mapping = ggplot2::aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend),
    arrow = grid::arrow(length = grid::unit(arrow_head_cm, "cm")),
    colour = colour,
    linewidth = size,
    inherit.aes = FALSE
  )
}

# ---- colour utilities --------------------------------------------------------

#' Cycle a bounded set of colour indices over the values of a key
#'
#' The order-stable primitive behind [assign_cycle_colours()] (and so
#' [radiate()]'s `colour_cycle`). Maps each value of `x` to an index in `1:n`,
#' numbering the distinct values by `levels` and wrapping back to `1` after every
#' `n`. Passing an explicit `levels` lets two data frames that share a key (for
#' example tracks and an overlay drawn on top of them) be coloured identically,
#' so a given key value gets the same colour in both.
#'
#' @param x A vector of key values (e.g. trajectory ids or a grouping column).
#' @param n Number of colours to cycle through (a positive integer).
#' @param levels Optional ordering of the distinct key values. Defaults to their
#'   order of first appearance in `x`. Supply a shared ordering to colour two
#'   frames consistently.
#'
#' @return A factor the same length as `x` with levels `"1"`..`"n"` giving the
#'   cycled colour index. `NA` in `x` is preserved as `NA`.
#'
#' @seealso [assign_cycle_colours()], [radiate()]
#' @examples
#' cycle_colours(c("a", "b", "c", "a"), n = 2)
#' @export
cycle_colours <- function(x, n, levels = NULL) {
  n_int <- if (is.character(n)) length(n) else as.integer(n)
  if (length(n_int) != 1L || is.na(n_int) || n_int < 1L)
    stop("`n` must be a positive integer or non-empty colour vector.")
  if (is.null(levels)) levels <- unique(x[!is.na(x)])
  idx <- ((match(x, levels) - 1L) %% n_int) + 1L
  factor(idx, levels = seq_len(n_int))
}

#' Assign a shared colour-key column to a TrajSet or data frame
#'
#' Writes a colour-key column (`into`, default `".colour"`) keyed on `by`, so the
#' tracks and any overlays drawn on top share one colour scale. Two modes are
#' chosen automatically by cardinality:
#' * **Cycled** -- `by = "trajectory"` (the trajectory id column), or any column
#'   with more than `n` levels: the key is a cycled `1:n` index
#'   ([cycle_colours()]), so a high-cardinality key stays legible.
#' * **Distinct** -- a column with `n` or fewer levels: the key holds the
#'   column's raw values (as a factor), so a legend is meaningful.
#'
#' Pass `reference` (another TrajSet or frame sharing the key) to borrow its level
#' order, so a given key value gets the same colour in both -- e.g. tracks and
#' their heading markers. If `by` names a column absent from `x` but present on
#' `reference`, it is borrowed by matching trajectory id.
#'
#' @param x A `TrajSet` or data frame to annotate.
#' @param by `"trajectory"` (the trajectory id column) or a grouping column name.
#' @param n Colour cap / cycle length (positive integer). Default 20.
#' @param reference Optional `TrajSet`/frame whose key order to reuse. Default
#'   `NULL` uses `x` itself.
#' @param into Name of the key column to add. Default `".colour"`.
#' @return `x` with the `into` column added.
#' @seealso [cycle_colours()]
#' @examples
#' ts <- simulate_tracks(n_points = 10, output = "trajset")
#' ts <- assign_colour_key(ts, by = "trajectory")
#' @export
assign_colour_key <- function(x, by, n = 20, reference = NULL, into = ".colour") {
  is_ts <- function(o) methods::is(o, "TrajSet")
  df_of <- function(o) if (is_ts(o)) as.data.frame(o) else o
  id_of <- function(o) if (is_ts(o)) o@cols$id else "id"

  ref    <- if (is.null(reference)) x else reference
  x_df   <- df_of(x)
  ref_df <- df_of(ref)

  if (identical(by, "trajectory")) {
    key_vals <- x_df[[id_of(x)]];  ref_vals <- ref_df[[id_of(ref)]]
  } else if (by %in% names(x_df)) {
    key_vals <- x_df[[by]];        ref_vals <- ref_df[[by]]
  } else {
    if (!by %in% names(ref_df))
      stop("colour key column '", by, "' not found in x or reference.")
    key_vals <- ref_df[[by]][match(x_df[[id_of(x)]], ref_df[[id_of(ref)]])]
    ref_vals <- ref_df[[by]]
  }

  lvls <- if (is.factor(ref_vals)) levels(ref_vals)
          else unique(ref_vals[!is.na(ref_vals)])
  distinct <- !identical(by, "trajectory") && length(lvls) <= n
  key <- if (distinct) factor(key_vals, levels = lvls)
         else cycle_colours(key_vals, n, levels = lvls)

  if (is_ts(x)) x@data[[into]] <- key else x[[into]] <- key
  x
}

#' Assign cycling colour indices to trajectories
#'
#' Creates a factor column that assigns each unique trajectory a colour index
#' in the range `1:n`, cycling back to 1 after every `n` trajectories. When
#' `panel_col` is supplied the cycle resets independently within each panel so
#' that trajectory 1 in every panel gets index 1.
#'
#' The resulting column can be passed to `colour_col` in [radiate()],
#' [add_heading_points()], or [add_heading_vectors()] to keep colours
#' consistent across layers.
#'
#' @param data A data frame containing at least `id_col` and, if supplied,
#'   `panel_col`.
#' @param id_col Name of the column identifying individual trajectories.
#' @param n Number of colours to cycle through (positive integer), or a
#'   character vector of colour values whose length determines `n`.
#' @param panel_col Optional column name. When set the cycle restarts for each
#'   unique value of this column.
#' @param out_col Name of the new factor column added to `data`. Default
#'   `"cycle_colour"`.
#'
#' @return `data` with an additional factor column named `out_col` (levels
#'   `"1"` through `"n"`).
#'
#' @examples
#' df <- data.frame(id = paste0("T", 1:12), panel = rep(c("A","B"), each = 6))
#' df <- assign_cycle_colours(df, id_col = "id", n = 4, panel_col = "panel")
#' table(df$panel, df$cycle_colour)
#'
#' @export
assign_cycle_colours <- function(data, id_col, n, panel_col = NULL,
                                 out_col = "cycle_colour") {
  if (!id_col %in% names(data))
    stop("`id_col` '", id_col, "' not found in data.")
  n_int <- if (is.character(n)) length(n) else as.integer(n)
  if (n_int < 1L) stop("`n` must be a positive integer or non-empty colour vector.")

  ids <- data[[id_col]]

  if (is.null(panel_col)) {
    data[[out_col]] <- cycle_colours(ids, n_int)
  } else {
    if (!panel_col %in% names(data))
      stop("`panel_col` '", panel_col, "' not found in data.")
    panels <- data[[panel_col]]
    out <- factor(rep(NA_integer_, length(ids)), levels = seq_len(n_int))
    for (p in unique(panels)) {
      # NA panel values form their own group; panels == NA would yield NA
      # subscripts, so match them explicitly. Each panel restarts the cycle.
      rows      <- if (is.na(p)) is.na(panels) else !is.na(panels) & panels == p
      out[rows] <- cycle_colours(ids[rows], n_int)
    }
    data[[out_col]] <- out
  }
  data
}

# ---- circular density overlay ------------------------------------------------

#' Internal: evaluate one density method for a numeric angle vector.
#' Returns a two-column data frame (theta, density) with NO radial scaling.
#' @noRd
.eval_circ_dens <- function(angles, method, n_theta, bins, bw,
                             boot_reps = 0L, boot_alpha = 0.05) {
  angles <- angles[is.finite(angles)]
  if (length(angles) < 3L) return(NULL)

  circ <- circular::circular(angles, units = "radians", template = "none",
                              modulo = "2pi", zero = 0, rotation = "counter")

  if (method == "vonmises") {
    fit      <- circular::mle.vonmises(circ)
    theta_sq <- seq(-pi, pi, length.out = n_theta + 1L)[-(n_theta + 1L)]
    ev       <- circular::circular(theta_sq, units = "radians", template = "none",
                                   modulo = "2pi", zero = 0, rotation = "counter")
    dens     <- as.numeric(circular::dvonmises(ev, mu = fit$mu, kappa = fit$kappa))
    df       <- data.frame(theta = theta_sq, density = dens)

    if (boot_reps > 0L) {
      n <- length(angles)
      boot_mat <- vapply(seq_len(boot_reps), function(.) {
        samp  <- circ[sample(n, n, replace = TRUE)]
        fit_b <- tryCatch(circular::mle.vonmises(samp), error = function(e) NULL)
        if (is.null(fit_b)) return(rep(NA_real_, n_theta))
        as.numeric(circular::dvonmises(ev, mu = fit_b$mu, kappa = fit_b$kappa))
      }, numeric(n_theta))
      lo <- boot_alpha / 2
      hi <- 1 - lo
      df$density_lower <- apply(boot_mat, 1L, stats::quantile, probs = lo, na.rm = TRUE)
      df$density_upper <- apply(boot_mat, 1L, stats::quantile, probs = hi, na.rm = TRUE)
    }
    return(df)

  } else if (method == "kernel") {
    bw_use   <- if (is.null(bw)) circular::bw.nrd.circular(circ) else bw
    dens_obj <- circular::density.circular(circ, bw = bw_use)
    theta_sq <- atan2(sin(as.numeric(dens_obj$x)), cos(as.numeric(dens_obj$x)))
    dens     <- as.numeric(dens_obj$y)
    ord      <- order(theta_sq)
    theta_sq <- theta_sq[ord]
    dens     <- dens[ord]

  } else {
    breaks   <- seq(-pi, pi, length.out = bins + 1L)
    counts   <- tabulate(cut(angles, breaks = breaks, include.lowest = TRUE),
                         nbins = bins)
    theta_sq <- (breaks[-1L] + breaks[-length(breaks)]) / 2
    dens     <- as.numeric(counts)
  }

  data.frame(theta = theta_sq, density = dens)
}

#' Compute a circular density data frame from heading observations
#'
#' Evaluates a directional density for a set of heading angles and returns a
#' tidy data frame of `(theta, density)` pairs. The result can be passed
#' directly to [add_circular_density()] for rendering, or inspected and
#' modified before plotting -- for example to replace the `density` column with
#' a Bayesian posterior predictive density obtained from `brms` or another
#' modelling package.
#'
#' Three built-in estimation methods are provided:
#'
#' * `"vonmises"` -- fit a von Mises distribution by MLE
#'   ([circular::mle.vonmises()]) and evaluate the fitted density on a regular
#'   grid of `n_theta` angles. Bootstrap confidence bands are available via
#'   `boot_reps`.
#' * `"kernel"` -- circular kernel density estimate
#'   ([circular::density.circular()]) with bandwidth chosen by
#'   [circular::bw.nrd.circular()] unless `bw` is supplied.
#' * `"histogram"` -- angular bin counts (a circular rose diagram); `bins`
#'   controls the number of bins.
#'
#' When `colour_col` is supplied the density is computed independently for each
#' group and the group label is preserved in the output, enabling per-panel use
#' with [radiate()]'s `panel_by`.
#'
#' When `boot_reps > 0` and `method = "vonmises"`, a non-parametric bootstrap
#' is run: `boot_reps` samples are drawn with replacement, a von Mises MLE is
#' fitted to each, and the density is evaluated on the same grid. The
#' `boot_alpha / 2` and `1 - boot_alpha / 2` quantiles across replicates are
#' returned as `density_lower` and `density_upper` columns. These can be
#' rendered as a confidence band by [add_circular_density()], or replaced with
#' interval values from a Bayesian model before plotting.
#'
#' @param headings_df Data frame containing heading angles.
#' @param heading_col Name of the heading column (radians). Default `"heading"`.
#' @param colour_col Optional grouping column. When set, one density is
#'   computed per group and the column is included in the output.
#' @param method Estimation method: `"vonmises"` (default), `"kernel"`, or
#'   `"histogram"`.
#' @param n_theta Number of angular evaluation points for smooth methods.
#'   Default `500`.
#' @param bins Number of angular bins for the histogram method. Default `36`
#'   (10degrees each).
#' @param bw Bandwidth passed to [circular::density.circular()]. `NULL`
#'   uses [circular::bw.nrd.circular()].
#' @param boot_reps Integer. Number of bootstrap replicates for a `"vonmises"`
#'   confidence band. `0` (default) skips the bootstrap. Ignored for `"kernel"`
#'   and `"histogram"`.
#' @param boot_alpha Significance level for the bootstrap band. Default `0.05`
#'   produces a 95\% interval.
#' @param axial Logical; when `TRUE`, mirror each observation to
#'   `heading_col + pi` before density estimation, producing a period-pi
#'   (bidirectional/axial) density. Default `FALSE`.
#'
#' @return A data frame with columns `theta` (radians, -pi to pi) and `density`
#'   (non-negative), plus `colour_col` if supplied. When `boot_reps > 0` and
#'   `method = "vonmises"`, also includes `density_lower` and `density_upper`.
#'   Suitable for passing to [add_circular_density()].
#'
#' @seealso [add_circular_density()], [add_heading_density()]
#' @importFrom circular circular mle.vonmises dvonmises density.circular bw.nrd.circular
#' @export
#'
#' @examples
#' hd <- data.frame(heading = c(0.2, 0.3, 0.4, 0.5, -0.1, 0.1, 0.6, 0.2))
#' dens_df <- compute_circular_density(hd)
#' head(dens_df)
#'
#' # Bootstrap CI band (vonmises only):
#' \dontrun{
#' dens_df <- compute_circular_density(hd, boot_reps = 999L)
#' # density_lower / density_upper can be replaced with Bayesian interval values
#' # before passing to add_circular_density()
#' }
#'
#' # Replace the density column with values from an external model before plotting:
#' # dens_df$density <- my_bayesian_density(dens_df$theta)
#' # ggplot() + coord_fixed() + add_circular_density(dens_df)
compute_circular_density <- function(headings_df,
                                     heading_col = "heading",
                                     colour_col  = NULL,
                                     method      = c("vonmises", "kernel", "histogram"),
                                     n_theta     = 500L,
                                     bins        = 36L,
                                     bw          = NULL,
                                     boot_reps   = 0L,
                                     boot_alpha  = 0.05,
                                     axial       = FALSE) {
  method     <- match.arg(method)
  use_colour <- !is.null(colour_col) && colour_col %in% names(headings_df)

  if (!heading_col %in% names(headings_df))
    stop("`heading_col` '", heading_col, "' not found in headings_df.")

  if (isTRUE(axial)) headings_df <- .mirror_axial(headings_df, heading_col)

  groups    <- if (use_colour) split(headings_df, headings_df[[colour_col]]) else list(headings_df)
  dens_list <- lapply(seq_along(groups), function(i) {
    d <- .eval_circ_dens(groups[[i]][[heading_col]], method, n_theta, bins, bw,
                         boot_reps = boot_reps, boot_alpha = boot_alpha)
    if (!is.null(d) && use_colour) d[[colour_col]] <- names(groups)[[i]]
    d
  })
  out <- do.call(rbind, Filter(Negate(is.null), dens_list))

  if (use_colour && is.factor(headings_df[[colour_col]]))
    out[[colour_col]] <- factor(out[[colour_col]],
                                levels = levels(headings_df[[colour_col]]))
  out
}

#' Wrap a pre-computed circular density around the unit circle
#'
#' Takes a data frame of `(theta, density)` pairs -- from any source: MLE,
#' kernel estimation, Bayesian posterior predictive, or hand-crafted -- and
#' renders it as a radial path (and optionally a filled polygon) around the
#' unit circle boundary. At each angle theta the plotted radius is
#' `1 + scale * density(theta) / max(density)`.
#'
#' Because this function only handles rendering, it is agnostic to how the
#' density was produced. To compute from raw headings use
#' [compute_circular_density()] first, or call the convenience wrapper
#' [add_heading_density()] which combines both steps.
#'
#' @details For an **axial** (bidirectional) density, do not mirror this
#'   pre-computed frame (that would double-count a full-circle density). Instead
#'   estimate it with [compute_circular_density()]`(..., axial = TRUE)`, which
#'   augments the raw sample before estimation, then pass the result here.
#'
#' @param density_df Data frame with columns named by `theta_col` and
#'   `density_col` (and, optionally, `colour_col`). Each row represents one
#'   evaluated angle.
#' @param theta_col Name of the angle column (radians, -pi to pi). Default
#'   `"theta"`.
#' @param density_col Name of the density/count column. Default `"density"`.
#' @param colour_col Optional grouping column. When set, separate paths are
#'   drawn per group, enabling ggplot2 faceting.
#' @param scale Maximum radial extension above the unit circle. Default `0.4`
#'   (peak at r = 1.4). Density is normalised within each group before scaling.
#' @param colour Fixed line colour used when `colour_col` is `NULL`. Default
#'   `"black"`.
#' @param fill Colour for the region between the unit circle and the density
#'   curve. `NA` (default) draws no fill.
#' @param alpha Alpha transparency for the filled polygon. Default `0.2`.
#' @param linewidth Width of the density path. Default `0.8`.
#' @param ci_fill Fill colour for the bootstrap confidence band. Only used when
#'   `density_df` contains `density_lower` and `density_upper` columns (produced
#'   by [compute_circular_density()] with `boot_reps > 0`, or supplied
#'   manually). Default `"grey70"`.
#' @param ci_alpha Alpha transparency for the confidence band polygon. Default
#'   `0.3`.
#'
#' @return A list of one, two, or three ggplot2 layers: optional CI band
#'   polygon, optional fill polygon between density and unit circle, and density
#'   path line.
#'
#' @seealso [compute_circular_density()], [add_heading_density()]
#' @importFrom ggplot2 geom_path geom_polygon aes
#' @importFrom rlang .data sym
#' @export
#'
#' @examples
#' library(ggplot2)
#' # From compute_circular_density:
#' hd <- data.frame(heading = c(0.2, 0.3, 0.4, 0.5, -0.1, 0.1, 0.6, 0.2))
#' dens_df <- compute_circular_density(hd)
#' ggplot() + coord_fixed() + add_circular_density(dens_df)
#'
#' # From an external model (e.g. brms posterior predictive):
#' theta_grid <- seq(-pi, pi, length.out = 200)
#' external_dens <- data.frame(
#'   theta   = theta_grid,
#'   density = exp(-2 * (1 - cos(theta_grid - 0.5)))  # von Mises-like
#' )
#' ggplot() + coord_fixed() + add_circular_density(external_dens, fill = "steelblue")
add_circular_density <- function(density_df,
                                 theta_col   = "theta",
                                 density_col = "density",
                                 colour_col  = NULL,
                                 scale       = 0.4,
                                 colour      = "black",
                                 fill        = NA,
                                 alpha       = 0.2,
                                 linewidth   = 0.8,
                                 ci_fill     = "grey70",
                                 ci_alpha    = 0.3) {
  use_colour <- !is.null(colour_col) && colour_col %in% names(density_df)
  has_ci     <- all(c("density_lower", "density_upper") %in% names(density_df))

  for (col in c(theta_col, density_col)) {
    if (!col %in% names(density_df))
      stop("`density_df` is missing column '", col, "'.")
  }

  disp_opts <- attr(density_df, "display", exact = TRUE) %||% circ_display()

  # Normalise density within each group and compute Cartesian coordinates.
  # CI bounds use the same max_d so the band is on the same scale as the curve.
  groups    <- if (use_colour) split(density_df, density_df[[colour_col]]) else list(density_df)
  dens_list <- lapply(groups, function(d) {
    theta <- d[[theta_col]]
    dens  <- d[[density_col]]
    max_d <- max(dens, na.rm = TRUE)
    scl   <- if (max_d > 0) scale / max_d else 0
    r     <- 1 + scl * dens
    d$.r  <- r
    xy    <- .uc_to_display_coords(r * cos(theta), r * sin(theta), disp_opts)
    d$.x  <- xy$x
    d$.y  <- xy$y
    d$.theta_raw <- theta
    if (has_ci) {
      d$.r_lower <- 1 + scl * d$density_lower
      d$.r_upper <- 1 + scl * d$density_upper
    }
    d
  })
  dens_df <- do.call(rbind, dens_list)

  layers <- list()

  # Bootstrap CI band: polygon from upper envelope forward + lower envelope reversed
  if (has_ci) {
    grp_ids <- if (use_colour) unique(dens_df[[colour_col]]) else list(NULL)
    ci_parts <- lapply(grp_ids, function(gid) {
      d  <- if (use_colour) dens_df[dens_df[[colour_col]] == gid, ] else dens_df
      th <- d$.theta_raw
      upper_xy <- .uc_to_display_coords(d$.r_upper * cos(th), d$.r_upper * sin(th), disp_opts)
      lower_xy <- .uc_to_display_coords(d$.r_lower * cos(th), d$.r_lower * sin(th), disp_opts)
      out <- data.frame(
        x = c(upper_xy$x, rev(lower_xy$x)),
        y = c(upper_xy$y, rev(lower_xy$y))
      )
      if (use_colour) out[[colour_col]] <- gid
      out
    })
    ci_df  <- do.call(rbind, ci_parts)
    ci_map <- ggplot2::aes(x = .data$x, y = .data$y)
    if (use_colour) ci_map[["group"]] <- rlang::sym(colour_col)
    ci_args <- list(data = ci_df, mapping = ci_map,
                    fill = ci_fill, colour = NA, alpha = ci_alpha, inherit.aes = FALSE)
    layers <- c(layers, list(do.call(ggplot2::geom_polygon, ci_args)))
  }

  if (!is.na(fill)) {
    grp_ids <- if (use_colour) unique(dens_df[[colour_col]]) else list(NULL)
    poly_parts <- lapply(grp_ids, function(gid) {
      d  <- if (use_colour) dens_df[dens_df[[colour_col]] == gid, ] else dens_df
      th <- d$.theta_raw
      inner_xy <- .uc_to_display_coords(cos(rev(th)), sin(rev(th)), disp_opts)
      out <- data.frame(x = c(d$.x, inner_xy$x), y = c(d$.y, inner_xy$y))
      if (use_colour) out[[colour_col]] <- gid
      out
    })
    poly_df  <- do.call(rbind, poly_parts)
    poly_map <- ggplot2::aes(x = .data$x, y = .data$y)
    if (use_colour) poly_map[["group"]] <- rlang::sym(colour_col)

    poly_args <- list(data = poly_df, mapping = poly_map,
                      fill = fill, colour = NA, alpha = alpha, inherit.aes = FALSE)
    layers <- c(layers, list(do.call(ggplot2::geom_polygon, poly_args)))
  }

  path_map <- ggplot2::aes(x = .data$.x, y = .data$.y)
  if (use_colour) {
    path_map[["colour"]] <- rlang::sym(colour_col)
    path_map[["group"]]  <- rlang::sym(colour_col)
  }
  path_args <- list(data = dens_df, mapping = path_map,
                    linewidth = linewidth, inherit.aes = FALSE)
  if (!use_colour) path_args$colour <- colour

  c(layers, list(do.call(ggplot2::geom_path, path_args)))
}

#' Compute a circular density and add it to a radial plot in one step
#'
#' Convenience wrapper that calls [compute_circular_density()] followed by
#' [add_circular_density()]. Equivalent to:
#' ```r
#' add_circular_density(
#'   compute_circular_density(headings_df, heading_col, colour_col, method, ...),
#'   colour_col = colour_col, scale = scale, ...
#' )
#' ```
#'
#' Use [compute_circular_density()] + [add_circular_density()] directly when you
#' need to inspect or replace the density values before plotting (e.g. to
#' substitute a Bayesian posterior predictive density from `brms`).
#'
#' @inheritParams compute_circular_density
#' @inheritParams add_circular_density
#'
#' @return A list of one or two ggplot2 layers.
#'
#' @seealso [compute_circular_density()], [add_circular_density()]
#' @importFrom circular circular mle.vonmises dvonmises density.circular bw.nrd.circular
#' @importFrom ggplot2 geom_path geom_polygon aes
#' @importFrom rlang .data sym
#' @export
#'
#' @examples
#' library(ggplot2)
#' hd <- data.frame(heading = c(0.2, 0.3, 0.4, 0.5, -0.1, 0.1, 0.6, 0.2))
#' ggplot() + coord_fixed() + add_heading_density(hd, fill = "steelblue", scale = 0.5)
add_heading_density <- function(headings_df,
                                heading_col = "heading",
                                colour_col  = NULL,
                                method      = c("vonmises", "kernel", "histogram"),
                                n_theta     = 500L,
                                bins        = 36L,
                                bw          = NULL,
                                boot_reps   = 0L,
                                boot_alpha  = 0.05,
                                scale       = 0.4,
                                colour      = "black",
                                fill        = NA,
                                alpha       = 0.2,
                                linewidth   = 0.8,
                                ci_fill     = "grey70",
                                ci_alpha    = 0.3,
                                axial       = FALSE) {
  method  <- match.arg(method)
  dens_df <- compute_circular_density(headings_df, heading_col = heading_col,
                                      colour_col = colour_col, method = method,
                                      n_theta = n_theta, bins = bins, bw = bw,
                                      boot_reps = boot_reps, boot_alpha = boot_alpha,
                                      axial = axial)
  add_circular_density(dens_df, colour_col = colour_col,
                       scale = scale, colour = colour,
                       fill = fill, alpha = alpha, linewidth = linewidth,
                       ci_fill = ci_fill, ci_alpha = ci_alpha)
}

# ---- display helpers ---------------------------------------------------------

.to_clock_display <- function(x, y) list(x = -y, y = x)

# ---- circular interval arc ---------------------------------------------------

#' @noRd
.compute_one_interval <- function(angles, stat, boot_reps, boot_alpha, axial = FALSE) {
  angles <- angles[is.finite(angles)]
  folded <- .fold_angles(angles, axial)
  n      <- length(folded)
  circ   <- circular::circular(folded, units = "radians", modulo = "2pi")

  mean_f <- if (n >= 1L) {
    mu <- as.numeric(circular::mean.circular(circ))
    atan2(sin(mu), cos(mu))
  } else NA_real_

  if (n < 3L) {
    mean_dir <- .unfold_mean(mean_f %% (2*pi), axial)
    return(data.frame(mean_dir = mean_dir, lower = NA_real_, upper = NA_real_,
                      wraps = FALSE, stringsAsFactors = FALSE))
  }

  if (stat == "sd") {
    sd_val <- as.numeric(circular::sd.circular(circ))
    lower_f <- mean_f - sd_val
    upper_f <- mean_f + sd_val
  } else {
    ci <- tryCatch(
      as.numeric(
        circular::mle.vonmises.bootstrap.ci(circ, alpha = boot_alpha,
                                            bias = TRUE, reps = boot_reps)$mu.ci
      ),
      error = function(e) c(NA_real_, NA_real_)
    )
    lower_f <- ci[1L]
    upper_f <- ci[2L]
  }

  if (isTRUE(axial)) {
    # unfold: axis is the halved mean; endpoints keep their (halved) deviation
    dl <- atan2(sin(lower_f - mean_f), cos(lower_f - mean_f))
    du <- atan2(sin(upper_f - mean_f), cos(upper_f - mean_f))
    mean_dir <- (mean_f / 2) %% pi
    lower <- mean_dir + dl / 2
    upper <- mean_dir + du / 2
  } else {
    mean_dir <- mean_f
    lower <- lower_f
    upper <- upper_f
  }

  lower <- atan2(sin(lower), cos(lower))
  upper <- atan2(sin(upper), cos(upper))
  wraps <- is.finite(lower) && is.finite(upper) && lower > upper

  data.frame(mean_dir = mean_dir, lower = lower, upper = upper,
             wraps = wraps, stringsAsFactors = FALSE)
}

#' Compute a circular interval arc from heading angles
#'
#' Returns a data frame of arc bounds centred on the circular mean direction.
#' Two built-in statistics are available: a bootstrap confidence interval for
#' the mean direction (`"bootstrap_ci"`, via
#' [circular::mle.vonmises.bootstrap.ci()]) and +/-1 circular SD (`"sd"`, via
#' [circular::sd.circular()]). The `lower` and `upper` columns of the output
#' can be replaced with Bayesian credible interval bounds from any model
#' before passing to [add_circ_interval()].
#'
#' @param headings_df Data frame containing heading angles.
#' @param heading_col Name of the heading column (radians). Default `"heading"`.
#' @param colour_col Optional grouping column. When set, one row is returned per
#'   group and the column is preserved in the output.
#' @param stat Statistic: `"bootstrap_ci"` (default) or `"sd"`.
#' @param boot_reps Integer. Bootstrap replicates for `stat = "bootstrap_ci"`.
#'   Default `1000L`. Ignored when `stat = "sd"`.
#' @param boot_alpha Significance level for the bootstrap CI. Default `0.05`
#'   produces a 95\% interval.
#' @param axial Logical. Treat the angles as axial (bidirectional, mod-pi)
#'   data: the interval is computed via the angle-doubling method and `mean_dir`
#'   is reported as an axis in `[0, pi)`, with the endpoints scaled accordingly.
#'   Default `FALSE` (ordinary directional data).
#'
#' @return A data frame with columns `mean_dir`, `lower`, `upper` (radians,
#'   `[-pi, pi]`), and `wraps` (logical, `TRUE` when the arc crosses the +/-pi
#'   discontinuity). `lower` and `upper` are `NA` when `n < 3`.
#'
#' @seealso [add_circ_interval()], [add_heading_interval()]
#' @importFrom circular circular mean.circular sd.circular mle.vonmises.bootstrap.ci
#' @export
compute_circ_interval <- function(headings_df,
                                  heading_col = "heading",
                                  colour_col  = NULL,
                                  stat        = c("bootstrap_ci", "sd"),
                                  boot_reps   = 1000L,
                                  boot_alpha  = 0.05,
                                  axial       = FALSE) {
  stat <- match.arg(stat)
  if (!heading_col %in% names(headings_df))
    stop("`heading_col` '", heading_col, "' not found in headings_df.")

  use_colour <- !is.null(colour_col) && colour_col %in% names(headings_df)
  groups     <- if (use_colour) split(headings_df, headings_df[[colour_col]]) else list(headings_df)

  out_list <- lapply(seq_along(groups), function(i) {
    angles <- groups[[i]][[heading_col]]
    row    <- .compute_one_interval(angles, stat, boot_reps, boot_alpha, axial)
    if (use_colour) row[[colour_col]] <- names(groups)[[i]]
    row
  })

  out <- do.call(rbind, out_list)
  if (use_colour && is.factor(headings_df[[colour_col]]))
    out[[colour_col]] <- factor(out[[colour_col]],
                                levels = levels(headings_df[[colour_col]]))
  out
}

#' Render a pre-computed circular interval arc on a radial plot
#'
#' Takes a data frame produced by [compute_circ_interval()] (or equivalent)
#' and renders it as a `geom_path()` arc at radius `radius`. Each row produces
#' one arc. Rows where `lower` or `upper` is `NA` are silently skipped.
#'
#' For the Bayesian extension, replace `lower` and `upper` in the output of
#' [compute_circ_interval()] with credible interval bounds from any model
#' before calling this function:
#' ```r
#' iv <- compute_circ_interval(hd)
#' iv$lower <- posterior_lower
#' iv$upper <- posterior_upper
#' ggplot() + coord_fixed() + add_circ_interval(iv)
#' ```
#'
#' @param interval_df Data frame with columns `mean_dir`, `lower`, `upper`
#'   (radians, `[-pi, pi]`), and optionally `wraps` (logical). Typically the
#'   output of [compute_circ_interval()].
#' @param colour_col Optional column name to map to the colour aesthetic.
#'   Ignored when `colour` is also supplied.
#' @param radius Radial position of the arc. Default `1.05`.
#' @param linewidth Line width. Default `1.5`.
#' @param colour Fixed colour. When `NULL` (default) and `colour_col` is set,
#'   colour is mapped from that column; when `NULL` and no `colour_col`, draws
#'   in `"black"`. Supplying any colour string always overrides `colour_col`.
#' @param linetype Line type. Default `"solid"`.
#' @param n_theta Number of points along the arc. Default `500L`.
#'
#' @return A `geom_path()` layer.
#'
#' @param axial Logical. Render the overlay for axial (bidirectional, mod-pi)
#'   data via the angle-doubling method: the CI is drawn at both poles of the
#'   axis. Default `FALSE`.
#' @seealso [compute_circ_interval()], [add_heading_interval()]
#' @importFrom ggplot2 geom_path aes
#' @importFrom rlang .data sym
#' @export
add_circ_interval <- function(interval_df,
                              colour_col = NULL,
                              radius     = 1.05,
                              linewidth  = 1.5,
                              colour     = NULL,
                              linetype   = "solid",
                              n_theta    = 500L,
                              axial      = FALSE) {
  for (col in c("lower", "upper")) {
    if (!col %in% names(interval_df))
      stop("`interval_df` is missing required column '", col, "'.")
  }

  has_group_col <- !is.null(colour_col) && colour_col %in% names(interval_df)
  map_colour    <- has_group_col && is.null(colour)
  has_wraps     <- "wraps" %in% names(interval_df)
  disp_opts     <- attr(interval_df, "display", exact = TRUE) %||% circ_display()

  valid_rows <- which(!is.na(interval_df$lower) & !is.na(interval_df$upper))

  if (!length(valid_rows)) {
    empty <- data.frame(.x = numeric(0), .y = numeric(0), .group_id = integer(0))
    return(ggplot2::geom_path(
      data    = empty,
      mapping = ggplot2::aes(x = .data$.x, y = .data$.y, group = .data$.group_id),
      inherit.aes = FALSE
    ))
  }

  one_arc <- function(i, lo, hi, gid) {
    wraps <- if (has_wraps) isTRUE(interval_df$wraps[i]) else lo > hi
    theta_seq <- if (wraps) seq(lo, hi + 2 * pi, length.out = n_theta)
                 else        seq(lo, hi, length.out = n_theta)
    xy <- .uc_to_display_coords(radius * cos(theta_seq),
                                radius * sin(theta_seq), disp_opts)
    d  <- data.frame(.x = xy$x, .y = xy$y, .group_id = gid)
    if (has_group_col) d[[colour_col]] <- interval_df[[colour_col]][i]
    d
  }
  parts <- lapply(valid_rows, function(i) {
    lo <- interval_df$lower[i]; hi <- interval_df$upper[i]
    if (isTRUE(axial)) {
      rbind(one_arc(i, lo, hi, i),
            one_arc(i, lo + pi, hi + pi, i + max(valid_rows)))
    } else {
      one_arc(i, lo, hi, i)
    }
  })
  arc_df <- do.call(rbind, parts)

  path_map <- ggplot2::aes(x = .data$.x, y = .data$.y, group = .data$.group_id)
  if (map_colour) path_map[["colour"]] <- rlang::sym(colour_col)

  path_args <- list(data = arc_df, mapping = path_map,
                    linewidth = linewidth, linetype = linetype,
                    inherit.aes = FALSE)
  if (!map_colour) path_args$colour <- if (is.null(colour)) "black" else colour
  do.call(ggplot2::geom_path, path_args)
}

#' Compute a circular interval arc and add it to a radial plot in one step
#'
#' Convenience wrapper that calls [compute_circ_interval()] followed by
#' [add_circ_interval()]. Use [compute_circ_interval()] + [add_circ_interval()]
#' directly when you need to replace `lower`/`upper` with Bayesian credible
#' interval bounds before rendering.
#'
#' @inheritParams compute_circ_interval
#' @inheritParams add_circ_interval
#' @param display A [`circ_display`] object. When `NULL` (default), read from
#'   `attr(headings_df, "display")`, falling back to `circ_display()`.
#'
#' @return A `geom_path()` layer.
#'
#' @param axial Logical. Render the overlay for axial (bidirectional, mod-pi)
#'   data via the angle-doubling method: the CI is drawn at both poles of the
#'   axis. Default `FALSE`.
#' @seealso [compute_circ_interval()], [add_circ_interval()]
#' @importFrom circular circular mean.circular sd.circular mle.vonmises.bootstrap.ci
#' @importFrom ggplot2 geom_path aes
#' @importFrom rlang .data sym
#' @export
add_heading_interval <- function(headings_df,
                                 heading_col = "heading",
                                 colour_col  = NULL,
                                 display     = NULL,
                                 stat        = c("bootstrap_ci", "sd"),
                                 boot_reps   = 1000L,
                                 boot_alpha  = 0.05,
                                 radius      = 1.05,
                                 linewidth   = 1.5,
                                 colour      = NULL,
                                 linetype    = "solid",
                                 n_theta     = 500L,
                                 axial       = FALSE) {
  if (is.null(display))
    display <- attr(headings_df, "display", exact = TRUE) %||% circ_display()
  stat <- match.arg(stat)
  iv   <- compute_circ_interval(headings_df, heading_col = heading_col,
                                colour_col = colour_col,
                                stat = stat,
                                boot_reps = boot_reps, boot_alpha = boot_alpha,
                                axial = axial)
  attr(iv, "display") <- display
  add_circ_interval(iv, colour_col = colour_col,
                    radius = radius, linewidth = linewidth,
                    colour = colour, linetype = linetype, n_theta = n_theta,
                    axial = axial)
}

# ---- circular mean arrow -----------------------------------------------------

#' Compute circular mean direction and resultant length from a headings data frame
#'
#' Computes the circular mean direction and resultant length (R) per group from
#' a headings data frame, typically the output of [derive_headings()].
#' `mean_dir` in the returned data frame is **always in unit-circle convention**
#' (0 = East, counterclockwise), regardless of the input convention, making it
#' suitable for direct use in [add_circ_mean()].
#'
#' @param headings_df Data frame with a column of heading angles in radians.
#'   [derive_headings()] sets `attr(headings_df, "angle_convention")` and
#'   `attr(headings_df, "coords")` automatically.
#' @param heading_col Name of the column containing heading angles. Default
#'   `"heading"`.
#' @param colour_col Optional. Name of a column to group by. One row is
#'   returned per group. The same column maps to colour in [add_circ_mean()].
#' @param axial Logical. Treat the angles as axial (bidirectional, mod-pi)
#'   data: `mean_dir` is the axis in `[0, pi)` and `resultant_R` is the axial
#'   resultant length, both via the angle-doubling method. Default `FALSE`.
#' @return A data frame with columns `mean_dir` (unit-circle radians, 0 to
#'   2pi), `resultant_R` (0--1), and `colour_col` when supplied. Both are `NA`
#'   when a group contains fewer than 2 finite angles.
#'
#' @seealso [add_circ_mean()], [add_heading_arrow()]
#' @importFrom circular circular mean.circular rho.circular
#' @export
compute_circ_mean <- function(headings_df,
                              heading_col = "heading",
                              colour_col  = NULL,
                              axial       = FALSE) {
  if (!heading_col %in% names(headings_df))
    stop("`heading_col` '", heading_col, "' not found in headings_df.")

  use_colour <- !is.null(colour_col) && colour_col %in% names(headings_df)
  groups     <- if (use_colour) split(headings_df, headings_df[[colour_col]]) else list(headings_df)

  out_list <- lapply(seq_along(groups), function(i) {
    angles <- groups[[i]][[heading_col]]
    angles <- angles[is.finite(angles)]

    if (length(angles) < 2L) {
      row <- data.frame(mean_dir = NA_real_, resultant_R = NA_real_,
                        stringsAsFactors = FALSE)
    } else {
      circ_obj <- circular::circular(.fold_angles(angles, axial),
                                     units = "radians", modulo = "2pi")
      mean_dir <- .unfold_mean(
        .wrap_to_2pi(as.numeric(circular::mean.circular(circ_obj, na.rm = TRUE))),
        axial)
      R        <- as.numeric(circular::rho.circular(circ_obj, na.rm = TRUE))
      row <- data.frame(mean_dir = mean_dir, resultant_R = R, stringsAsFactors = FALSE)
    }

    if (use_colour) row[[colour_col]] <- names(groups)[[i]]
    row
  })

  out <- do.call(rbind, out_list)
  if (use_colour && is.factor(headings_df[[colour_col]]))
    out[[colour_col]] <- factor(out[[colour_col]], levels = levels(headings_df[[colour_col]]))
  # Carry the input's display convention forward so add_circ_mean() orients the
  # arrow the same way as the rest of the figure (rbind drops attributes).
  attr(out, "display") <- attr(headings_df, "display", exact = TRUE)
  out
}

#' Render pre-computed circular mean arrows on a radial plot
#'
#' Takes a data frame produced by [compute_circ_mean()] and renders each row
#' as a `geom_segment()` arrow. `mean_dir` must be in unit-circle convention
#' (0 = East, CCW), as returned by [compute_circ_mean()]. Rows where
#' `mean_dir` or `resultant_R` is `NA` are silently skipped.
#'
#' @param summary_df Data frame with columns `mean_dir` (UC radians, 0 to 2pi)
#'   and `resultant_R` (0--1). Typically the output of [compute_circ_mean()].
#' @param colour_col Optional. Name of a column in `summary_df` to map to the
#'   colour aesthetic. Ignored when `colour` is also supplied.
#' @param linewidth Line width of the arrow segment. Default `1`.
#' @param colour Fixed colour. When `NULL` (default) and `colour_col` is set,
#'   colour is mapped from that column; when `NULL` and no `colour_col`, draws
#'   in `"black"`. Supplying any colour string always overrides `colour_col`.
#' @param arrow_length_cm Arrowhead length in cm. Default `0.2`.
#' @param ... Additional arguments forwarded to `geom_segment` (e.g.
#'   `linetype`, `alpha`, or a custom `arrow` spec that overrides the default).
#'
#' @return A `geom_segment()` layer.
#'
#' @param axial Logical. Render the overlay for axial (bidirectional, mod-pi)
#'   data: the mean is drawn as a double-headed axis through the centre. Default
#'   `FALSE`.
#' @seealso [compute_circ_mean()], [add_heading_arrow()]
#' @importFrom ggplot2 geom_segment aes
#' @importFrom rlang .data sym
#' @importFrom grid arrow unit
#' @export
add_circ_mean <- function(summary_df,
                          colour_col      = NULL,
                          linewidth       = 1,
                          colour          = NULL,
                          arrow_length_cm = 0.2,
                          axial           = FALSE,
                          ...) {
  for (col in c("mean_dir", "resultant_R")) {
    if (!col %in% names(summary_df))
      stop("`summary_df` is missing required column '", col, "'.")
  }

  use_colour <- is.null(colour) && !is.null(colour_col) && colour_col %in% names(summary_df)
  valid_rows <- which(!is.na(summary_df$mean_dir) & !is.na(summary_df$resultant_R))

  if (!length(valid_rows)) {
    empty <- data.frame(.x = numeric(0), .y = numeric(0),
                        .xend = numeric(0), .yend = numeric(0))
    return(ggplot2::geom_segment(
      data    = empty,
      mapping = ggplot2::aes(x = .data$.x, y = .data$.y,
                             xend = .data$.xend, yend = .data$.yend),
      inherit.aes = FALSE
    ))
  }

  summary_df <- summary_df[valid_rows, , drop = FALSE]

  disp <- attr(summary_df, "display", exact = TRUE) %||% circ_display()
  tip  <- .uc_to_display_coords(summary_df$resultant_R * cos(summary_df$mean_dir),
                                 summary_df$resultant_R * sin(summary_df$mean_dir),
                                 disp)
  if (isTRUE(axial)) {
    tail <- .uc_to_display_coords(-summary_df$resultant_R * cos(summary_df$mean_dir),
                                  -summary_df$resultant_R * sin(summary_df$mean_dir),
                                  disp)
    summary_df$.x <- tail$x
    summary_df$.y <- tail$y
  } else {
    summary_df$.x <- 0
    summary_df$.y <- 0
  }
  summary_df$.xend <- tip$x
  summary_df$.yend <- tip$y

  seg_map <- ggplot2::aes(x = .data$.x, y = .data$.y,
                          xend = .data$.xend, yend = .data$.yend)
  if (use_colour) seg_map[["colour"]] <- rlang::sym(colour_col)

  seg_args <- list(
    data        = summary_df,
    mapping     = seg_map,
    linewidth   = linewidth,
    arrow       = grid::arrow(length = grid::unit(arrow_length_cm, "cm"),
                              ends = if (isTRUE(axial)) "both" else "last"),
    inherit.aes = FALSE,
    ...
  )
  if (!use_colour) seg_args$colour <- if (is.null(colour)) "black" else colour

  do.call(ggplot2::geom_segment, seg_args)
}

#' Compute a circular mean arrow and add it to a radial plot in one step
#'
#' Convenience wrapper that calls [compute_circ_mean()] followed by
#' [add_circ_mean()]. Use the two-step form directly when you need to inspect
#' or modify the summary data frame before rendering.
#'
#' @inheritParams compute_circ_mean
#' @inheritParams add_circ_mean
#' @param display A [`circ_display`] object. When `NULL` (default), read from
#'   `attr(headings_df, "display")`, falling back to `circ_display()`.
#'
#' @return A `geom_segment()` layer.
#'
#' @param axial Logical. Render the overlay for axial (bidirectional, mod-pi)
#'   data: the mean is drawn as a double-headed axis through the centre. Default
#'   `FALSE`.
#' @seealso [compute_circ_mean()], [add_circ_mean()]
#' @importFrom circular circular mean.circular rho.circular
#' @importFrom ggplot2 geom_segment aes
#' @importFrom rlang .data sym
#' @importFrom grid arrow unit
#' @export
add_heading_arrow <- function(headings_df,
                              heading_col     = "heading",
                              colour_col      = NULL,
                              display         = NULL,
                              linewidth       = 1,
                              colour          = NULL,
                              arrow_length_cm = 0.2,
                              axial           = FALSE,
                              ...) {
  if (is.null(display))
    display <- attr(headings_df, "display", exact = TRUE) %||% circ_display()
  sm <- compute_circ_mean(headings_df, heading_col = heading_col,
                          colour_col = colour_col, axial = axial)
  attr(sm, "display") <- display
  add_circ_mean(sm, colour_col = colour_col,
                linewidth = linewidth, colour = colour,
                arrow_length_cm = arrow_length_cm, axial = axial, ...)
}

# ---- heading overlay layers --------------------------------------------------

# Row-bind the antipodal (theta + pi) copy of each observation for axial display.
# `angle_col` is the angle column (radians). `negate` names independent coordinate
# columns (e.g. x_inner/y_inner) that must be point-reflected through the origin on
# the mirrored copy; columns whose plotted position is derived from `angle_col` at
# render time need no entry here. All other columns are carried verbatim onto both
# copies so colour/group/weight mappings are unaffected.
.mirror_axial <- function(df, angle_col, negate = character()) {
  if (nrow(df) == 0L) return(df)
  mirrored <- df
  mirrored[[angle_col]] <- (df[[angle_col]] + pi) %% (2 * pi)
  for (col in negate)
    if (col %in% names(mirrored)) mirrored[[col]] <- -df[[col]]
  rbind(df, mirrored)
}

#' Add heading endpoint markers on the unit circle
#'
#' Draws a hollow circle at `(cos(heading), sin(heading))` for each row of a
#' headings data frame, placing one marker per trajectory on the unit-circle
#' boundary at the derived heading direction. The data frame is normally the
#' output of [derive_headings()].
#'
#' @param headings_df Data frame with a `heading` column (angles in radians).
#' @param colour_col Name of a column in `headings_df` to map to the colour
#'   aesthetic. When `NULL` (default), the value of
#'   `attr(headings_df, "colour_col")` is used if set -- so heading markers
#'   automatically inherit the colour mapping from the associated trajectory
#'   plot when that attribute is present. Ignored when `colour` is supplied.
#' @param colour Fixed colour string. Overrides `colour_col` when supplied;
#'   when `NULL` and no `colour_col` resolves, defaults to `"black"`.
#' @param size Point size passed to `geom_point`.
#' @param alpha Point alpha transparency.
#' @param axial Logical; when `TRUE`, draw each observation at both `heading`
#'   and `heading + pi` (bidirectional/axial display). Default `FALSE`.
#'
#' @return A `geom_point()` layer (shape = 1, hollow circle).
#'
#' @seealso [add_heading_vectors()], [derive_headings()]
#' @importFrom ggplot2 geom_point aes
#' @importFrom rlang .data sym
#' @export
#'
#' @examples
#' library(ggplot2)
#' # headings from a TrajSet via derive_headings(ts, rule = "crossing", ...)
#' hd <- data.frame(id = "A", time = 1, heading = pi / 4)
#' ggplot() + coord_fixed() + add_heading_points(hd)
#' ggplot() + coord_fixed() + add_heading_points(hd, colour = "steelblue")
add_heading_points <- function(headings_df, colour_col = NULL, colour = NULL,
                               size = 2, alpha = 1, axial = FALSE) {
  if (!("heading" %in% names(headings_df)))
    stop("`headings_df` must contain a `heading` column (radians).")
  if (is.null(colour_col)) colour_col <- attr(headings_df, "colour_col")
  disp <- attr(headings_df, "display", exact = TRUE) %||% circ_display()
  if (isTRUE(axial)) headings_df <- .mirror_axial(headings_df, "heading")

  xy <- .uc_to_display_coords(cos(headings_df$heading),
                               sin(headings_df$heading), disp)
  headings_df[[".x_head"]] <- xy$x
  headings_df[[".y_head"]] <- xy$y

  mapping <- ggplot2::aes(x = .data[[".x_head"]], y = .data[[".y_head"]])
  use_fixed_colour <- !is.null(colour) || is.null(colour_col) ||
                      !colour_col %in% names(headings_df)
  if (!use_fixed_colour) mapping[["colour"]] <- rlang::sym(colour_col)

  args <- list(data = headings_df, mapping = mapping,
               size = size, alpha = alpha, shape = 1, inherit.aes = FALSE)
  if (use_fixed_colour) args$colour <- if (is.null(colour)) "black" else colour
  do.call(ggplot2::geom_point, args)
}

#' Add heading vector segments from inner crossing to unit circle
#'
#' Draws a segment from the inner-radius crossing position to the heading
#' endpoint on the unit circle for each row of a headings data frame. This
#' visualises the extrapolated vector used to derive the heading and mirrors
#' the dotted-line display in the original millipede tracking workflow.
#'
#' Requires columns `heading`, `x_inner`, and `y_inner`, which are present
#' when [derive_headings()] is called with `rule = "crossing"` and
#' `return_coords = TRUE`.
#'
#' @param headings_df Data frame with columns `heading` (radians), `x_inner`,
#'   and `y_inner`.
#' @param colour_col Name of a column in `headings_df` to map to the colour
#'   aesthetic. When `NULL` (default), the value of
#'   `attr(headings_df, "colour_col")` is used if set -- so vectors
#'   automatically inherit the colour mapping from the associated trajectory
#'   plot when that attribute is present. Ignored when `colour` is supplied.
#' @param colour Fixed colour string. Overrides `colour_col` when supplied;
#'   when `NULL` and no `colour_col` resolves, defaults to `"black"`.
#' @param linetype Line type string or integer passed to `geom_segment`.
#' @param axial Logical; when `TRUE`, draw each vector at both `heading` and
#'   `heading + pi`, with the inner start point reflected through the origin.
#'   Default `FALSE`.
#'
#' @return A `geom_segment()` layer.
#'
#' @seealso [add_heading_points()], [derive_headings()]
#' @importFrom ggplot2 geom_segment aes
#' @importFrom rlang .data sym
#' @export
#'
#' @examples
#' library(ggplot2)
#' hd <- data.frame(id = "A", time = 1, heading = pi / 4,
#'                  x_inner = 0.15, y_inner = 0.15)
#' ggplot() + coord_fixed() + add_heading_vectors(hd)
add_heading_vectors <- function(headings_df, colour_col = NULL, colour = NULL,
                               linetype = "dotted", axial = FALSE) {
  if (is.null(colour_col)) colour_col <- attr(headings_df, "colour_col")
  required <- c("heading", "x_inner", "y_inner")
  missing_cols <- setdiff(required, names(headings_df))
  if (length(missing_cols)) {
    stop(sprintf(
      paste0("`headings_df` is missing columns: %s. ",
             "Call derive_headings(rule = 'crossing', return_coords = TRUE)."),
      paste(missing_cols, collapse = ", ")
    ))
  }

  disp <- attr(headings_df, "display", exact = TRUE) %||% circ_display()
  if (isTRUE(axial))
    headings_df <- .mirror_axial(headings_df, "heading",
                                 negate = c("x_inner", "y_inner"))

  end_xy   <- .uc_to_display_coords(cos(headings_df$heading),
                                     sin(headings_df$heading), disp)
  start_xy <- .uc_to_display_coords(headings_df$x_inner,
                                     headings_df$y_inner, disp)
  headings_df[[".x_head"]]  <- end_xy$x
  headings_df[[".y_head"]]  <- end_xy$y
  headings_df[[".x_inner"]] <- start_xy$x
  headings_df[[".y_inner"]] <- start_xy$y

  mapping <- ggplot2::aes(
    x    = .data[[".x_inner"]],
    y    = .data[[".y_inner"]],
    xend = .data[[".x_head"]],
    yend = .data[[".y_head"]]
  )
  use_fixed_colour <- !is.null(colour) || is.null(colour_col) || !colour_col %in% names(headings_df)
  if (!use_fixed_colour) mapping[["colour"]] <- rlang::sym(colour_col)

  args <- list(data = headings_df, mapping = mapping,
               linetype = linetype, inherit.aes = FALSE)
  if (use_fixed_colour) args$colour <- if (is.null(colour)) "black" else colour
  do.call(ggplot2::geom_segment, args)
}

#' Add stacked heading dots as a ggplot2 layer
#'
#' Places one point per observation at its heading angle, stacking coincident
#' angles radially to avoid overplotting. If \code{stack_r} is already a
#' column in \code{data} (from a prior call to \code{\link{stack_headings}}),
#' it is used as-is; otherwise stacking is computed internally.
#'
#' @param data A data frame with an angle column in radians, typically a
#'   \code{\link{headings_frame}}.
#' @param col Name of the angle column. Defaults to the \code{heading_col}
#'   attribute when \code{data} is a \code{headings_frame}.
#' @param step,start_sep,tol,direction,base_r Passed to
#'   \code{\link{stack_headings}} when \code{stack_r} is absent. See that
#'   function for details. \code{step} sets the gap between dots and
#'   \code{start_sep} offsets the first dot off the reference circle.
#' @param shade If \code{TRUE}, map \code{stack_n} to the alpha aesthetic
#'   (scaled 0.2--1 across the observed range). Overrides the fixed
#'   \code{alpha} argument.
#' @param shape Passed to \code{\link{stack_headings}} to request
#'   per-observation shape encoding. Shape is also applied when
#'   \code{shape_code} is already a column in \code{data}.
#'   Mapped to ggplot2 shape integers: 1 = hollow, 16 = filled,
#'   21 = filled with ring.
#' @param group Optional column name; stack within each group independently
#'   (e.g. one stacking per facet). Default \code{NULL}.
#' @param colour Fixed point colour (ignored when \code{colour_col} is set).
#' @param colour_col Optional column name to map to the colour aesthetic.
#' @param size Point size passed to \code{geom_point()}.
#' @param alpha Fixed alpha. Ignored when \code{shade = TRUE}.
#' @param ... Additional arguments passed to \code{ggplot2::geom_point()}.
#' @param axial Logical; when `TRUE`, mirror each observation to `col + pi`
#'   before stacking, so the figure reads as bidirectional. Stacking is computed
#'   after mirroring, so each antipodal cluster stacks within itself. Default
#'   `FALSE`.
#'
#' @return A \code{geom_point()} layer.
#'
#' @seealso \code{\link{headings_frame}}, \code{\link{stack_headings}},
#'   \code{\link{add_heading_points}}
#' @importFrom ggplot2 geom_point aes
#' @importFrom rlang .data sym
#' @export
add_stacked_headings <- function(data,
                                 col        = NULL,
                                 step       = 0.025,
                                 start_sep  = 0,
                                 tol        = NULL,
                                 direction  = "inward",
                                 base_r     = 1,
                                 shade      = FALSE,
                                 shape      = FALSE,
                                 group      = NULL,
                                 colour     = "black",
                                 colour_col = NULL,
                                 size       = 2,
                                 alpha      = 1,
                                 axial      = FALSE,
                                 ...) {
  if (is.null(col))
    col <- if (!is.null(attr(data, "heading_col"))) attr(data, "heading_col")
           else "heading"
  if (!col %in% names(data))
    stop(sprintf("column '%s' not found in data.", col))

  disp_opts <- attr(data, "display", exact = TRUE) %||% circ_display()

  if (isTRUE(axial)) data <- .mirror_axial(data, col)

  if (!"stack_r" %in% names(data))
    data <- stack_headings(data, col = col, step = step, start_sep = start_sep,
                           tol = tol, direction = direction, base_r = base_r,
                           shade = shade, shape = shape, group = group)
  xy <- .uc_to_display_coords(data$stack_r * cos(data[[col]]),
                               data$stack_r * sin(data[[col]]), disp_opts)
  data[[".x_stk"]] <- xy$x
  data[[".y_stk"]] <- xy$y

  mapping <- ggplot2::aes(x = .data[[".x_stk"]], y = .data[[".y_stk"]])

  use_fixed_colour <- is.null(colour_col) || !colour_col %in% names(data)
  if (!use_fixed_colour) mapping[["colour"]] <- rlang::sym(colour_col)

  use_fixed_shape <- TRUE
  if ("shape_code" %in% names(data)) {
    pch_map <- c(`1` = 1L, `2` = 16L, `3` = 21L)
    data[[".shape_pch"]] <- pch_map[as.character(data$shape_code)]
    mapping[["shape"]] <- rlang::sym(".shape_pch")
    use_fixed_shape <- FALSE
  }

  use_fixed_alpha <- TRUE
  if (shade && "stack_n" %in% names(data)) {
    mx <- max(data$stack_n, na.rm = TRUE)
    if (mx > 0) {
      data[[".alpha_v"]] <- 0.2 + 0.8 * data$stack_n / mx
      mapping[["alpha"]] <- rlang::sym(".alpha_v")
      use_fixed_alpha <- FALSE
    }
  }

  args <- list(data = data, mapping = mapping, size = size,
               inherit.aes = FALSE, ...)
  if (use_fixed_colour) args$colour <- colour
  if (use_fixed_alpha)  args$alpha  <- alpha
  if (use_fixed_shape)  args$shape  <- 16L

  do.call(ggplot2::geom_point, args)
}

# ---- themes ------------------------------------------------------------------

#' Themes for radial track plots, named for the ggplot2 base themes.
#'
#' Applies one of the standard ggplot2 themes as the look of a `radiate()`
#' plot, so the panel background, grid lines, and border match the familiar
#' `ggplot2::theme_*()` appearance. The Cartesian axis text and ticks are not
#' meaningful on a unit-circle plot and are removed by `radiate()` itself, so
#' this wrapper keeps only the panel-level styling that distinguishes the
#' themes from one another.
#'
#' @param name One of `"void"`, `"minimal"`, `"classic"`, `"bw"`, `"grey"`
#'   (or `"gray"`), `"light"`, `"dark"`, `"linedraw"` -- corresponding to the
#'   matching `ggplot2::theme_*()`. Default `"void"`.
#' @param base_size Base font size, passed to the underlying theme. Default
#'   `11`.
#'
#' @return A ggplot2 theme object.
#'
#' @examples
#' library(ggplot2)
#' ggplot() + coord_fixed() + add_circ() + radial_theme("bw")
#'
#' @importFrom ggplot2 theme_void theme_minimal theme_classic theme_bw
#' @importFrom ggplot2 theme_grey theme_light theme_dark theme_linedraw
#' @export
radial_theme <- function(name = "void", base_size = 11) {
  name <- match.arg(name, RADIAL_THEMES)
  switch(name,
    void     = ggplot2::theme_void(base_size = base_size),
    minimal  = ggplot2::theme_minimal(base_size = base_size),
    classic  = ggplot2::theme_classic(base_size = base_size),
    bw       = ggplot2::theme_bw(base_size = base_size),
    grey     = ,
    gray     = ggplot2::theme_grey(base_size = base_size),
    light    = ggplot2::theme_light(base_size = base_size),
    dark     = ggplot2::theme_dark(base_size = base_size),
    linedraw = ggplot2::theme_linedraw(base_size = base_size)
  )
}

# Accepted radial theme names (ggplot2 theme_* family). "gray" is an alias of
# "grey"; it is accepted as input but not offered as a separate choice.
RADIAL_THEMES <- c("void", "minimal", "classic", "bw", "grey", "gray",
                   "light", "dark", "linedraw")

# TRUE if a colour reads as "dark" (relative luminance < 0.5).
.is_dark <- function(col) {
  if (is.null(col) || is.na(col)) return(FALSE)
  rgb <- tryCatch(grDevices::col2rgb(col)[, 1], error = function(e) c(255, 255, 255))
  (0.299 * rgb[1] + 0.587 * rgb[2] + 0.114 * rgb[3]) / 255 < 0.5
}

# Keep an overlay colour legible against the disc fill: a dark colour on a dark
# disc is replaced with a light ink.
.legible <- function(colour, fill) {
  if (is.null(colour) || is.na(colour)) return(colour)
  if (.is_dark(fill) && .is_dark(colour)) "grey85" else colour
}

# Resolve the theme's axis-like styling (axis.line / axis.ticks / axis.text) plus
# the disc fill and a legibility-guarded "ink" fallback colour. radiatR draws its
# axis chrome ON the disc, so colour is legibility-adjusted; size/family/presence
# are taken verbatim.
.theme_axis_style <- function(name) {
  th <- radial_theme(name)
  line_el <- function(elname) {
    el <- ggplot2::calc_element(elname, th)
    if (inherits(el, "element_line") && !is.null(el$colour) && !is.na(el$colour)) {
      lw <- if (!is.null(el$linewidth) && !is.na(el$linewidth)) el$linewidth else 0.5
      list(colour = el$colour, linewidth = lw, present = TRUE)
    } else {
      list(colour = NA_character_, linewidth = 0.5, present = FALSE)
    }
  }
  text_el <- function(elname) {
    el <- ggplot2::calc_element(elname, th)
    if (inherits(el, "element_text") && !is.null(el$colour) && !is.na(el$colour)) {
      sz  <- if (!is.null(el$size) && !is.na(el$size)) el$size else 11
      fam <- if (is.null(el$family) || identical(el$family, "")) "" else el$family
      list(colour = el$colour, size = sz, family = fam, present = TRUE)
    } else {
      list(colour = NA_character_, size = 11, family = "", present = FALSE)
    }
  }
  line  <- line_el("axis.line")
  ticks <- line_el("axis.ticks")
  text  <- text_el("axis.text")

  bg   <- ggplot2::calc_element("panel.background", th)
  fill <- if (inherits(bg, "element_rect") && !is.null(bg$fill) && !is.na(bg$fill))
    bg$fill else NA_character_

  cand <- c(line$colour, ticks$colour, text$colour)
  cand <- cand[!is.na(cand)]
  raw_ink <- if (length(cand)) cand[[1]] else "black"

  list(line = line, ticks = ticks, text = text, fill = fill,
       ink = .legible(raw_ink, fill))
}

# Grid-like styling (colour, linewidth) for the radial guide overlays
# (quadrant lines and guide rings), pulled from the chosen theme's own
# `panel.grid` element so they match whatever grid that theme draws. Themes
# that draw no grid (void, classic) fall back to a subtle grey.
.theme_grid_style <- function(name) {
  th <- radial_theme(name)
  line_style <- function(elname) {
    el <- ggplot2::calc_element(elname, th)
    if (inherits(el, "element_line") &&
        !is.null(el$colour) && !is.na(el$colour)) {
      lw <- if (!is.null(el$linewidth) && !is.na(el$linewidth)) el$linewidth else 0.5
      list(colour = el$colour, linewidth = lw)
    } else {
      list(colour = "grey60", linewidth = 0.5)
    }
  }
  major <- line_style("panel.grid.major")
  minor <- line_style("panel.grid.minor")

  gmaj <- ggplot2::calc_element("panel.grid.major", th)
  has_grid <- inherits(gmaj, "element_line") &&
    !is.null(gmaj$colour) && !is.na(gmaj$colour)

  bg   <- ggplot2::calc_element("panel.background", th)
  fill <- if (inherits(bg, "element_rect") &&
              !is.null(bg$fill) && !is.na(bg$fill)) bg$fill else NA_character_

  list(colour = major$colour, linewidth = major$linewidth,  # flat back-compat aliases
       major = major, minor = minor, fill = fill, has_grid = has_grid)
}

# ---- trajectory plotting -----------------------------------------------------

#' Plot trajectories from a TrajSet (overlay or faceted)
#'
#' @param x TrajSet
#' @param color,linetype,alpha,size Optional column names (strings) mapped to aesthetics
#' @param panel_by NULL, a single string, or a character vector of columns to facet by
#' @param coord "polar" (unit circle) or "cartesian"
#' @param geom "path" or "point" (or both, as c("path","point"))
#' @param thin Keep every n-th point per id (for very long tables)
#' @param ncol Number of facet columns (when faceting)
#' @return ggplot object
#' @importFrom stats ave
#' @export
setGeneric("gg_traj", function(x, color = NULL, linetype = NULL, alpha = NULL, size = 0.6,
                               panel_by = NULL, coord = c("polar", "cartesian"),
                               geom = c("path"), thin = 1L, ncol = NULL) standardGeneric("gg_traj"))

#' @rdname gg_traj
#' @export
setMethod("gg_traj", "TrajSet",
  function(x, color = NULL, linetype = NULL, alpha = NULL, size = 0.6,
           panel_by = NULL, coord = c("polar", "cartesian"),
           geom = c("path"), thin = 1L, ncol = NULL) {

    coord <- match.arg(coord)
    geom  <- match.arg(geom, several.ok = TRUE)

    d   <- as.data.frame(x)
    idc <- x@cols$id
    xc  <- x@cols$x
    yc  <- x@cols$y
    th  <- x@cols$angle

    if (is.null(xc) || is.null(yc)) {
      d$`.ux` <- cos(d[[th]])
      d$`.uy` <- sin(d[[th]])
      xc <- ".ux"; yc <- ".uy"
    }

    if (thin > 1L) {
      keep_idx <- unlist(lapply(split(seq_len(nrow(d)), d[[idc]]), function(idx) {
        idx[seq(1, length(idx), by = thin)]
      }), use.names = FALSE)
      d <- d[keep_idx, , drop = FALSE]
    }

    p <- ggplot2::ggplot(
      d,
      ggplot2::aes(x = .data[[xc]], y = .data[[yc]], group = .data[[idc]])
    )

    if (!is.null(color)    && color    %in% names(d)) p <- p + ggplot2::aes(color    = .data[[color]])
    if (!is.null(linetype) && linetype %in% names(d)) p <- p + ggplot2::aes(linetype = .data[[linetype]])
    if (!is.null(alpha)    && alpha    %in% names(d)) p <- p + ggplot2::aes(alpha    = .data[[alpha]])

    if ("path"  %in% geom) p <- p + ggplot2::geom_path(lineend = "round", linewidth = size, na.rm = TRUE)
    if ("point" %in% geom) p <- p + ggplot2::geom_point(size = size * 1.5, stroke = 0, na.rm = TRUE)

    if (!is.null(panel_by)) {
      if (is.character(panel_by)) {
        missing <- setdiff(panel_by, names(d))
        if (length(missing)) stop("panel_by column(s) not found: ", paste(missing, collapse = ", "))
        p <- p + ggplot2::facet_wrap(stats::as.formula(paste("~", paste(panel_by, collapse = "+"))),
                                     scales = "free", ncol = ncol)
      } else stop("panel_by must be NULL or character vector of column names")
    }

    if (coord == "polar") {
      p <- p + ggplot2::coord_equal() +
        ggplot2::labs(x = NULL, y = NULL)
    } else {
      p <- p + ggplot2::coord_equal() + ggplot2::labs(x = xc, y = yc)
    }

    p + ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
})

#' Create geom layers for Cartesian track coordinates
#'
#' @param data Data frame that will be plotted.
#' @param x_col Name of the column mapped to the x aesthetic.
#' @param y_col Name of the column mapped to the y aesthetic.
#' @param geom Either a character vector (`"path"`/`"point"`) or a ggplot2 geom
#'   function (e.g. [ggplot2::geom_path]).
#' @param mapping Optional aesthetics created with [ggplot2::aes()].
#' @param ... Additional arguments passed on to the geom function.
#'
#' @return A list containing a single ggplot2 layer.
#'
#' @export
draw_tracks <- function(data, x_col, y_col, geom = "path", mapping = NULL, ...) {
  if (missing(x_col) || missing(y_col)) {
    stop("Both `x_col` and `y_col` must be specified.")
  }

  geom_fun <- resolve_geom(geom)
  x_sym <- rlang::sym(x_col)
  y_sym <- rlang::sym(y_col)

  if (!is.null(mapping)) {
    if (!inherits(mapping, "uneval")) {
      stop("`mapping` must be created with ggplot2::aes().")
    }
    mapping$x <- x_sym
    mapping$y <- y_sym
    base_mapping <- mapping
  } else {
    base_mapping <- ggplot2::aes(x = !!x_sym, y = !!y_sym)
  }

  layer <- rlang::exec(
    geom_fun,
    data = data,
    mapping = base_mapping,
    ...
  )

  list(layer)
}

resolve_geom <- function(geom) {
  if (is.character(geom)) {
    geom <- tolower(geom)
    geom <- match.arg(geom, c("path", "point", "line"))
    switch(
      geom,
      path = ggplot2::geom_path,
      line = ggplot2::geom_path,
      point = ggplot2::geom_point
    )
  } else if (is.function(geom)) {
    geom
  } else {
    stop("`geom` must be either a character shortcut or a ggplot2 geom function.")
  }
}

# ---- geometry helpers --------------------------------------------------------

#' Find the intercept of a line with the unit circle
#'
#' @param x0,y0 Starting coordinates of the vector.
#' @param x1,y1 Ending coordinates of the vector.
#' @return A tibble with the intersection point closest to `(x1, y1)`.
#' @export
line_circle_intercept <- function(x0, y0, x1, y1) {
  h <- 0; k <- 0; radius <- 1
  a <- (x1 - x0)^2 + (y1 - y0)^2
  b <- (2 * (x1 - x0) * (x0 - h)) + (2 * (y1 - y0) * (y0 - k))
  c <- (x0 - h)^2 + (y0 - k)^2 - radius^2
  D <- (b^2) - (4 * a * c)
  if (a <= 0) warning("Vector length is not positive!")
  if (D <= 0) warning("Discriminant is not positive with x0=", x0, " y0=", y0, " x1=", x1, " y1=", y1)
  if (c >= 0) warning("Starting point is outside radius!")

  t <- c(
    (-b + sqrt(D)) / (2 * a),
    (-b - sqrt(D)) / (2 * a)
  )

  ints <- rbind(
    c(t[1] * (x1 - x0) + x0, t[1] * (y1 - y0) + y0),
    c(t[2] * (x1 - x0) + x0, t[2] * (y1 - y0) + y0)
  )

  d <- c(sqrt(ints[1, 1]^2 + ints[1, 2]^2), sqrt(ints[2, 1]^2 + ints[2, 2]^2))
  if (!isTRUE(all.equal(d[1], d[2]))) warning("Intersect is off unit circle!")

  dist2track <- c(
    sqrt((ints[1, 1] - x1)^2 + (ints[1, 2] - y1)^2),
    sqrt((ints[2, 1] - x1)^2 + (ints[2, 2] - y1)^2)
  )
  xy <- ints[which.min(dist2track), ]

  tibble::tibble(x_int = xy[1], y_int = xy[2])
}

#' Intersection helper using track rows
#'
#' Convenience wrapper that accepts a data frame (or `TrajSet@data`) containing
#' coordinates and evaluates the intersection between two rows.
#'
#' @param df Data frame with `x` and `y` columns.
#' @param row_in,row_out Row indices (or names) identifying the start and end of the vector.
#' @return Tibble with `x_int`/`y_int`.
#' @export
line_circle_intercept_df <- function(df, row_in, row_out) {
  if (!all(c("x", "y") %in% names(df))) {
    stop("`df` must contain x/y columns.")
  }
  start <- df[row_in, , drop = TRUE]
  end <- df[row_out, , drop = TRUE]
  line_circle_intercept(start[["x"]], start[["y"]], end[["x"]], end[["y"]])
}

#' Intersection helper for TrajSet trajectories
#'
#' Extracts the first/last rows for a given id/time range and computes the
#' intersection with the unit circle.
#'
#' @param traj TrajSet
#' @param id Identifier of the trajectory
#' @param range Numeric index vector (e.g., rows within a trial)
#' @return Tibble with `x_int`/`y_int`
#' @export
line_circle_intercept_traj <- function(traj, id, range) {
  df <- traj@data
  id_col <- traj@cols$id
  x_col <- traj@cols$x
  y_col <- traj@cols$y
  if (is.null(x_col) || is.null(y_col)) {
    stop("TrajSet must encode x/y coordinates for intersection.")
  }
  subset <- df[df[[id_col]] == id, , drop = FALSE][range, , drop = FALSE]
  line_circle_intercept_df(
    data.frame(x = subset[[x_col]], y = subset[[y_col]]),
    1, nrow(subset)
  )
}

# Theme-derived radial styling resolved once, shared by the chrome helpers below
# and by radiate.default / radiate.headings_frame.
.radial_style <- function(theme, grid = "radial", grid_colour = NULL) {
  gs <- .theme_grid_style(theme)
  ax <- .theme_axis_style(theme)
  guide_col <- if (is.null(grid_colour)) gs$major$colour else grid_colour
  minor_col <- if (is.null(grid_colour)) gs$minor$colour else grid_colour
  radial_on        <- identical(grid, "radial")
  draw_radial_grid <- radial_on && isTRUE(gs$has_grid)
  col_of <- function(part)
    if (isTRUE(part$present)) .legible(part$colour, ax$fill) else ax$ink
  list(gs = gs, ax = ax, guide_col = guide_col, minor_col = minor_col,
       radial_on = radial_on, draw_radial_grid = draw_radial_grid, col_of = col_of)
}

# Background chrome: the radial grid disc and origin point, drawn UNDER the data
# (tracks or markers) so the data sits on top of the guides.
.radial_chrome_background <- function(g, style, origin = FALSE) {
  if (style$draw_radial_grid) {
    g <- g + add_radial_grid(
      rings_major     = c(0.5, 1),                 # outermost ring marks the boundary
      colour          = style$guide_col,
      colour_minor    = style$minor_col,
      linewidth       = style$gs$major$linewidth,
      linewidth_minor = style$gs$minor$linewidth,
      disc_fill       = style$gs$fill,
      origin          = FALSE
    )
  }
  if (isTRUE(origin)) g <- g + add_origin_point(colour = style$ax$ink)
  g
}

# Foreground chrome: base theme, a-la-carte rings/quadrants, circumference
# fallback, ticks and degree labels, drawn OVER the data.
.radial_chrome_foreground <- function(g, style, theme, grid, angle_labels,
                                      display, ticks = TRUE, quadrants = FALSE,
                                      rings = FALSE, circumference = TRUE) {
  g <- g + radial_theme(theme)
  if (style$draw_radial_grid) {
    g <- g + ggplot2::theme(
      panel.grid       = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      panel.border     = ggplot2::element_blank()
    )
  } else if (identical(grid, "none")) {
    g <- g + ggplot2::theme(panel.grid = ggplot2::element_blank())
  }
  if (!style$draw_radial_grid) {                # a-la-carte overlays only when no radial grid
    if (rings)
      g <- g + add_multiple_circles(circle_color = style$guide_col,
                                    circle_size  = style$gs$major$linewidth)
    if (quadrants)
      g <- g + add_quadrant_lines(colour    = style$guide_col,
                                  linewidth = style$gs$major$linewidth)
  }
  # Circumference: fallback boundary only when the grid doesn't already mark it.
  if (isTRUE(circumference) && !style$draw_radial_grid) {
    circ_lw <- if (isTRUE(style$ax$line$present)) style$ax$line$linewidth else 0.5
    g <- g + add_circ(circle_color = style$col_of(style$ax$line), circle_size = circ_lw)
  }
  if (isTRUE(ticks))
    g <- g + add_ticks(colour = style$col_of(style$ax$ticks),
                       linewidth = style$ax$ticks$linewidth)
  if (angle_labels != "none")
    g <- g + degree_labs(display = display, units = angle_labels,
                         colour = style$col_of(style$ax$text),
                         size   = style$ax$text$size / ggplot2::.pt,
                         family = style$ax$text$family)
  g
}

# ---- high-level plotting -----------------------------------------------------

#' Make ggplot object of tracks radiating from circle centre.
#'
#' Accepts either a precomputed data frame of polar/cartesian coordinates or a
#' `TrajSet`. When a `TrajSet` is supplied, column mappings are inferred from
#' the object and handed off to the plotting helpers.
#'
#' @param data Data frame or `TrajSet`.
#' @param geom Geom specification passed to [draw_tracks()].
#' @param group_col Optional column for grouping aesthetics.
#' @param colour_col Optional column for colour aesthetics. Mutually exclusive
#'   with `colour_cycle`.
#' @param colour_cycle Optional cycling colour specification. Either a positive
#'   integer `n` (trajectories are assigned colours 1--n, cycling back to 1 after
#'   every `n` trajectories) or a character vector of colour values (e.g.
#'   `c("red","blue","green")`). When `panel_by` is set the cycle restarts
#'   independently within each panel. Mutually exclusive with `colour_col`.
#' @param theme Plot appearance, named for the ggplot2 base themes: one of
#'   `"void"` (default), `"minimal"`, `"classic"`, `"bw"`, `"grey"`, `"light"`,
#'   `"dark"`, or `"linedraw"`. See [radial_theme()].
#' @param grid One of `"radial"` (default), `"cartesian"`, or `"none"`. `"radial"`
#'   replaces the theme's Cartesian grid with theme-styled radial guides
#'   (circular disc + major/minor crosshairs and rings) for grid-bearing themes,
#'   and draws nothing for grid-less themes (`void`, `classic`). `"cartesian"`
#'   keeps the theme's square grid; `"none"` removes all gridlines.
#' @param grid_colour Optional colour overriding the theme-derived guide colour.
#' @param origin Logical; draw a centre point. Default `FALSE`. When drawn it
#'   takes the theme's axis ink colour.
#' @param circumference Logical; draw the unit-circle circumference as a fallback
#'   boundary when no radial grid marks it (grid-less themes, `grid = "none"`).
#'   Default `TRUE`. Styled from the theme's `axis.line` (else the axis ink). On
#'   grid-bearing themes the grid's outer ring marks the boundary instead, so this
#'   has no effect there.
#' @param quadrants Logical; draw the two dashed lines through the origin that
#'   demarcate the quadrants. Default `FALSE`. Their colour and width follow the
#'   chosen `theme`'s grid lines (see [radial_theme()]).
#' @param rings Logical; draw concentric guide rings (the radial analogue of a
#'   grid). Default `FALSE`. Their colour and width follow the chosen `theme`'s
#'   grid lines.
#' @param x_col Name of the x-coordinate column.  Default \code{"rel_x"}.
#' @param y_col Name of the y-coordinate column.  Default \code{"rel_y"}.
#' @param show_labels Whether to place labels at the perimeter.
#' @param label_col Column containing label values.
#' @param label_size Text size for perimeter labels.
#' @param label_padding Multiplier applied to the unit circle when placing labels.
#' @param label_use_repel Use `ggrepel::geom_text_repel()` when available.
#' @param show_tracks Whether to draw the trajectory paths. Default `TRUE`.
#'   Set to `FALSE` to render the unit circle and any overlays (arrow, circle, ticks)
#'   without the track geometry.
#' @param show_arrow Whether to draw a mean resultant arrow from the centre.
#' @param arrow_angle_col Column containing angles (radians) to summarise for the arrow.
#' @param arrow_colour Arrow colour (a single fixed colour). Ignored when
#'   `arrow_colour_col` is set.
#' @param arrow_colour_col Optional grouping column. When supplied, one mean
#'   resultant arrow is drawn per level of this column (within each panel, if
#'   `panel_by` is also set) and mapped to the colour aesthetic, so the arrow can
#'   follow a colour grouping independently of faceting. Default `NULL` draws a
#'   single arrow in `arrow_colour`.
#' @param arrow_size Arrow linewidth.
#' @param panel_by NULL, a column name, or a character vector of column names
#'   to facet by (via [ggplot2::facet_wrap()]). The named column(s) must be
#'   present in the data.
#' @param ncol Number of columns passed to [ggplot2::facet_wrap()] when
#'   `panel_by` is set.
#' @param strip_labels Logical or `NULL`. Whether to show a label identifying
#'   the panel variable value on each panel. Defaults to `TRUE` when `panel_by`
#'   is set, `FALSE` otherwise. Ignored when `panel_by` is `NULL`.
#' @param strip_position Position of the panel label. One of `"top"` (default),
#'   `"bottom"`, `"left"`, `"right"` (ggplot2 strip positions), or `"inside"`
#'   (places a text annotation inside the plot area, centred below the unit
#'   circle at y = -1.25).
#' @param strip_label_size Font size for strip labels. Applies to both strip
#'   text and the in-panel `"inside"` annotation.
#' @param display A [`circ_display`] object controlling how angles are rendered.
#'   Default `circ_display()` puts North at top with clockwise-positive degrees.
#'   Use `circ_display(zero = 0)` when the reference direction lies at East in
#'   unit-circle coordinates (e.g. the `cpunctatus` dataset).
#' @param ticks,degrees,legend,title,xlab,ylab,axes Additional styling options.
#'   `degrees` is retained for back-compatibility; `degrees = FALSE` is
#'   equivalent to `angle_labels = "none"`. Tick styling (colour, width, length)
#'   follows the chosen `theme`'s `axis.ticks`.
#' @param angle_labels One of `"degrees"` (default; e.g. `45°`), `"none"`, or
#'   `"radians"` (e.g. `π/4`) -- the diagonal angle labels around the circle.
#'   Label styling (colour, size, family) follows the chosen `theme`'s `axis.text`.
#' @param ... Additional arguments forwarded to [draw_tracks()].
#' @return A `ggplot2` object.
#' @examples
#' tracks_demo <- simulate_tracks(conditions = data.frame(n_trials = 1L),
#'                                n_points = 200, seed = 1)
#' radiate(tracks_demo, x_col = "rel_x", y_col = "rel_y", group_col = "trial_id")

#' @export
radiate <- function(data, ...) UseMethod("radiate")

#' @rdname radiate
#' @exportS3Method
radiate.TrajSet <- function(data, ...) radiate.default(data, ...)

#' @rdname radiate
#' @exportS3Method
radiate.default <-
function(
  data,
  x_col = "rel_x", y_col = "rel_y",
  geom = "path",
  group_col = NULL,
  colour_col = NULL,
  colour_cycle = NULL,
  panel_by = NULL,
  ncol = NULL,
  strip_labels = NULL,
  strip_position = c("top", "bottom", "left", "right", "inside"),
  strip_label_size = 11,
  ticks = NULL,
  degrees = NULL, legend = NULL, title = NULL,
  xlab = NULL, ylab = NULL, axes = NULL,
  angle_labels = c("degrees", "none", "radians"),
  theme = c("void", "minimal", "classic", "bw", "grey", "gray",
            "light", "dark", "linedraw"),
  quadrants = FALSE,
  rings = FALSE,
  grid = c("radial", "cartesian", "none"),
  grid_colour = NULL,
  origin = FALSE,
  circumference = TRUE,
  show_labels = NULL,
  label_col = NULL,
  label_size = 3,
  label_padding = 1.08,
  label_use_repel = TRUE,
  show_tracks = TRUE,
  show_arrow = NULL,
  arrow_angle_col = NULL,
  arrow_colour = "black",
  arrow_colour_col = NULL,
  arrow_size = 2,
  display = circ_display(),
  ...){
  if (is.null(ticks)) {ticks = TRUE}
  if (is.null(legend)) {legend = FALSE}
  if (is.null(axes)) {axes = FALSE}
  angle_labels <- match.arg(angle_labels)
  grid <- match.arg(grid)
  # Back-compat: an explicit `degrees = FALSE` hides the angle labels.
  if (isFALSE(degrees)) angle_labels <- "none"
  theme <- match.arg(theme)
  if (is.null(show_labels)) {
    show_labels <- TRUE
  }

  # Coerce non-TrajSet inputs to TrajSet to keep a single canonical path
  if (!inherits(data, "TrajSet")) {
    if (!missing(x_col) && !missing(y_col) && all(c(x_col, y_col) %in% names(data))) {
      # Prefer explicit mapping if provided
      guessed_id <- if (!is.null(group_col) && group_col %in% names(data)) group_col else {
        cand <- intersect(c("trial_id","id","track","trajectory"), names(data)); if (length(cand)) cand[1] else stop("Could not infer id column; supply `group_col`.")
      }
      guessed_time <- {
        cand <- intersect(c("frame","time","t"), names(data)); if (length(cand)) cand[1] else stop("Could not infer time column (e.g., 'frame').")
      }
      angle_col_guess <- if ("rel_theta" %in% names(data)) "rel_theta" else if ("abs_theta" %in% names(data)) "abs_theta" else NULL
      ts <- TrajSet(
        data,
        id = guessed_id,
        time = guessed_time,
        angle = angle_col_guess,
        x = x_col,
        y = y_col,
        angle_unit = "radians",
        normalize_xy = FALSE
      )
    } else {
      # Fall back to heuristic coercion
      ts <- methods::as(data, "TrajSet")
    }
  } else {
    ts <- data
  }

  data <- as.data.frame(ts)
  if (!is.null(ts@meta$plot_x_col) && identical(x_col, "rel_x")) {
    x_col <- ts@meta$plot_x_col
    if (!is.null(ts@meta$plot_y_col) && identical(y_col, "rel_y"))
      y_col <- ts@meta$plot_y_col
  } else if (!is.null(ts@cols$x) && identical(x_col, "rel_x")) {
    x_col <- ts@cols$x
    if (!is.null(ts@cols$y) && identical(y_col, "rel_y")) y_col <- ts@cols$y
  }
  xy_disp <- .uc_to_display_coords(data[[x_col]], data[[y_col]], display)
  data[[".disp_x"]] <- xy_disp$x
  data[[".disp_y"]] <- xy_disp$y
  x_col <- ".disp_x"
  y_col <- ".disp_y"
  if (is.null(group_col)) group_col <- ts@cols$id
  if (is.null(arrow_angle_col)) arrow_angle_col <- ts@cols$angle

  if (!is.null(colour_cycle)) {
    if (!is.null(colour_col))
      stop("`colour_col` and `colour_cycle` cannot both be set.")
    n_cycle <- if (is.character(colour_cycle)) length(colour_cycle) else as.integer(colour_cycle)
    data <- assign_cycle_colours(data, id_col = group_col, n = n_cycle,
                                 panel_col = panel_by, out_col = ".cycle_colour")
    colour_col <- ".cycle_colour"
  }

  x_sym <- rlang::sym(x_col)
  y_sym <- rlang::sym(y_col)

  g <- ggplot2::ggplot(data = data,
                       mapping = ggplot2::aes(x = !!x_sym, y = !!y_sym)) +
    ggplot2::coord_fixed()

  layer_mapping <- NULL
  if (!is.null(group_col) || !is.null(colour_col)) {
    mapping_list <- list()
    if (!is.null(group_col)) mapping_list$group <- rlang::sym(group_col)
    if (!is.null(colour_col)) mapping_list$colour <- rlang::sym(colour_col)
    layer_mapping <- do.call(ggplot2::aes, mapping_list)
  }

  style <- .radial_style(theme, grid, grid_colour)
  g <- .radial_chrome_background(g, style, origin = origin)

  if (show_tracks) {
    g <- g + draw_tracks(
      data = data, x_col = x_col, y_col = y_col,
      geom = geom, mapping = layer_mapping, ...
    )
  }

  g <- .radial_chrome_foreground(
    g, style, theme = theme, grid = grid, angle_labels = angle_labels,
    display = display, ticks = ticks, quadrants = quadrants, rings = rings,
    circumference = circumference
  )

  label_col <- resolve_label_column(data, label_col, group_col)
  if (show_labels && !is.null(label_col)) {
    label_data <- build_label_data(
      data, label_col, x_col = x_col, y_col = y_col,
      colour_col = colour_col, padding = label_padding
    )
    if (!is.null(label_data)) {
      label_aes <- ggplot2::aes(x = .data$label_x, y = .data$label_y, label = .data$label_value)
      if (!is.null(colour_col) && "colour_value" %in% names(label_data)) {
        label_aes$colour <- quote(.data$colour_value)
      }
      if (label_use_repel && requireNamespace("ggrepel", quietly = TRUE)) {
        g <- g + ggrepel::geom_text_repel(
          data = label_data,
          mapping = label_aes,
          size = label_size,
          inherit.aes = FALSE,
          max.overlaps = Inf
        )
      } else {
        g <- g + ggplot2::geom_text(
          data = label_data,
          mapping = label_aes,
          size = label_size,
          inherit.aes = FALSE
        )
      }
    }
  }

  if (is.null(show_arrow)) {
    show_arrow <- TRUE
  }
  if (show_arrow) {
    angle_col <- resolve_angle_column(data)
    if (!is.null(arrow_angle_col)) angle_col <- arrow_angle_col
    if (!is.null(angle_col) &&
        angle_col %in% names(data) &&
        is.numeric(data[[angle_col]]) &&
        any(!is.na(data[[angle_col]]))) {
      # Per-trial mean angles (avoids timepoint-count bias).
      if (!is.null(group_col) && group_col %in% names(data)) {
        trial_means <- vapply(split(data[[angle_col]], data[[group_col]]),
          function(a) {
            a <- a[!is.na(a)]
            if (length(a) == 0L) return(NA_real_)
            atan2(mean(sin(a)), mean(cos(a)))
          }, numeric(1L))
        trial_df <- data.frame(.key = names(trial_means),
                               .mean = as.numeric(trial_means),
                               stringsAsFactors = FALSE)
        names(trial_df)[1L] <- group_col
      } else {
        trial_df <- data.frame(.key = seq_along(data[[angle_col]]),
                               .mean = data[[angle_col]],
                               stringsAsFactors = FALSE)
        names(trial_df)[1L] <- group_col %||% ".key"
      }

      .circ_mean_seg <- function(a) {
        a <- a[!is.na(a)]
        if (length(a) == 0L) return(NULL)
        ma <- atan2(mean(sin(a)), mean(cos(a)))
        r  <- sqrt(mean(sin(a))^2 + mean(cos(a))^2)
        data.frame(x = 0, y = 0, xend = r * cos(ma), yend = r * sin(ma))
      }

      # Group the arrow by the panel column (so each arrow routes to its facet)
      # and/or the colour column (so it follows a colour grouping). Each grouping
      # column is carried onto the arrow rows for faceting / the colour scale.
      seg_cols <- unique(c(
        if (!is.null(panel_by) && length(panel_by) == 1L &&
            panel_by %in% names(data)) panel_by,
        if (!is.null(arrow_colour_col) &&
            arrow_colour_col %in% names(data)) arrow_colour_col
      ))
      seg_cols <- seg_cols[!is.na(seg_cols)]

      if (length(seg_cols) && !is.null(group_col) && group_col %in% names(data)) {
        key_map  <- unique(data[, c(group_col, seg_cols), drop = FALSE])
        trial_df <- merge(trial_df, key_map, by = group_col, all.x = TRUE)
        # split by the combination of grouping columns, preserving their classes
        grp_key  <- interaction(trial_df[seg_cols], drop = TRUE, sep = "\r")
        arrow_df <- do.call(rbind, lapply(split(seq_len(nrow(trial_df)), grp_key),
          function(ix) {
            seg <- .circ_mean_seg(trial_df$.mean[ix])
            if (is.null(seg)) return(NULL)
            for (sc in seg_cols) seg[[sc]] <- trial_df[[sc]][ix[1L]]  # constant in group
            seg
          }))
      } else {
        arrow_df <- .circ_mean_seg(trial_df$.mean)
      }

      if (!is.null(arrow_df) && nrow(arrow_df) > 0L) {
        xy_a <- .uc_to_display_coords(arrow_df$xend, arrow_df$yend, display)
        arrow_df$xend <- xy_a$x
        arrow_df$yend <- xy_a$y
      }

      if (!is.null(arrow_df) && nrow(arrow_df) > 0L) {
        use_arrow_colour_col <- !is.null(arrow_colour_col) &&
                                arrow_colour_col %in% names(arrow_df)
        arrow_map <- ggplot2::aes(x = x, y = y, xend = xend, yend = yend)
        if (use_arrow_colour_col)
          arrow_map[["colour"]] <- rlang::sym(arrow_colour_col)
        seg_args <- list(
          data        = arrow_df,
          mapping     = arrow_map,
          linewidth   = arrow_size,
          arrow       = grid::arrow(length = grid::unit(0.2, "cm")),
          inherit.aes = FALSE
        )
        if (!use_arrow_colour_col) seg_args$colour <- arrow_colour
        g <- g + do.call(ggplot2::geom_segment, seg_args)
      }
    } else if (!is.null(arrow_angle_col)) {
      warning("Arrow requested but column `", arrow_angle_col,
              "` is unavailable or not numeric; skipping arrow layer.",
              call. = FALSE)
    }
  }

  if (is.character(colour_cycle)) {
    scale_vals <- stats::setNames(colour_cycle, as.character(seq_along(colour_cycle)))
    g <- g + ggplot2::scale_colour_manual(values = scale_vals)
  }

  if (!is.null(panel_by)) {
    if (!is.character(panel_by)) stop("`panel_by` must be a character vector of column names.")
    missing_pby <- setdiff(panel_by, names(data))
    if (length(missing_pby)) stop("panel_by column(s) not found in data: ", paste(missing_pby, collapse = ", "))

    strip_position <- match.arg(strip_position)
    show_strip <- if (is.null(strip_labels)) TRUE else isTRUE(strip_labels)

    fw_pos <- if (strip_position == "inside") "top" else strip_position
    g <- g + ggplot2::facet_wrap(
      stats::as.formula(paste("~", paste(panel_by, collapse = "+"))),
      ncol = ncol,
      strip.position = fw_pos
    )

    if (show_strip && strip_position != "inside") {
      g <- g + ggplot2::theme(
        strip.text = ggplot2::element_text(size = strip_label_size)
      )
    } else {
      # Hide the facet strip when labels are suppressed (strip_labels = FALSE)
      # or drawn manually inside the panel (strip_position = "inside"). The
      # base ggplot2 theme would otherwise show its default strip.
      g <- g + ggplot2::theme(
        strip.text       = ggplot2::element_blank(),
        strip.background = ggplot2::element_blank()
      )
    }

    if (show_strip && strip_position == "inside") {
      panel_col <- panel_by[[1]]
      label_df <- unique(data[, c(panel_by), drop = FALSE])
      label_df[[".x_lab"]] <- 0
      label_df[[".y_lab"]] <- -1.25
      label_df[[".label"]] <- as.character(label_df[[panel_col]])
      g <- g + ggplot2::geom_text(
        data        = label_df,
        mapping     = ggplot2::aes(x = .data[[".x_lab"]], y = .data[[".y_lab"]],
                                   label = .data[[".label"]]),
        size        = strip_label_size / ggplot2::.pt,
        inherit.aes = FALSE
      )
    }
  }

  if (legend == FALSE) {
    g <- g + ggplot2::theme(
      legend.position = "none",
      legend.background = ggplot2::element_rect(fill = "transparent"),
      legend.box.background = ggplot2::element_rect(fill = "transparent"))
  }
  if (axes == FALSE) {
    g <- g + ggplot2::theme(
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank())
  }
  if (is.null(xlab)) g <- g + ggplot2::xlab('') else g <- g + ggplot2::xlab(as.character(xlab))
  if (is.null(ylab)) g <- g + ggplot2::ylab('') else g <- g + ggplot2::ylab(as.character(ylab))

  if (is.null(title)) {
    g <- g + ggplot2::theme(plot.title = ggplot2::element_blank())
  } else {
    g <- g + ggplot2::ggtitle(as.character(title)) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 14, hjust = 0.5))
  }

  g
}

#' @rdname radiate
#' @param col Name of the angle column in \code{data}. Defaults to the
#'   \code{heading_col} attribute when \code{data} is a
#'   \code{\link{headings_frame}}.
#' @param step,tol,direction,base_r,shade,shape Passed to
#'   \code{\link{add_stacked_headings}}. See that function for details.
#' @param show_markers When \code{TRUE} (default) the stacked-dot markers are
#'   drawn; \code{FALSE} returns the themed radial frame only, for callers
#'   that layer their own marker and statistic overlays.
#' @exportS3Method
radiate.headings_frame <- function(
  data,
  col       = NULL,
  step      = 0.025,
  tol       = NULL,
  direction = "inward",
  base_r    = 1,
  shade     = FALSE,
  shape     = FALSE,
  panel_by  = NULL,
  ncol      = NULL,
  ticks     = TRUE,
  degrees   = TRUE,
  angle_labels = c("degrees", "none", "radians"),
  title     = NULL,
  theme     = c("void", "minimal", "classic", "bw", "grey", "gray",
                "light", "dark", "linedraw"),
  quadrants = FALSE,
  rings     = FALSE,
  grid      = c("radial", "cartesian", "none"),
  grid_colour   = NULL,
  circumference = TRUE,
  origin        = FALSE,
  colour_col    = NULL,
  legend        = FALSE,
  display       = circ_display(),
  show_markers  = TRUE,
  ...) {

  theme        <- match.arg(theme)
  grid         <- match.arg(grid)
  angle_labels <- match.arg(angle_labels)
  if (isFALSE(degrees)) angle_labels <- "none"

  style <- .radial_style(theme, grid, grid_colour)

  g <- ggplot2::ggplot() + ggplot2::coord_fixed()
  g <- .radial_chrome_background(g, style, origin = origin)

  if (isTRUE(show_markers)) {
    attr(data, "display") <- display
    g <- g + add_stacked_headings(
      data, col = col, step = step, tol = tol, direction = direction,
      base_r = base_r, shade = shade, shape = shape,
      colour_col = colour_col, ...
    )
  }

  g <- .radial_chrome_foreground(
    g, style, theme = theme, grid = grid, angle_labels = angle_labels,
    display = display, ticks = ticks, quadrants = quadrants, rings = rings,
    circumference = circumference
  )

  if (!is.null(panel_by)) {
    if (!is.character(panel_by))
      stop("`panel_by` must be a character string.")
    missing_pby <- setdiff(panel_by, names(data))
    if (length(missing_pby))
      stop("panel_by column(s) not found in data: ",
           paste(missing_pby, collapse = ", "))
    g <- g + ggplot2::facet_wrap(
      stats::as.formula(paste("~", paste(panel_by, collapse = "+"))),
      ncol = ncol
    )
  }

  if (is.null(title)) {
    g <- g + ggplot2::theme(plot.title = ggplot2::element_blank())
  } else {
    g <- g + ggplot2::ggtitle(as.character(title)) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 14, hjust = 0.5))
  }

  if (!isTRUE(legend)) g <- g + ggplot2::theme(legend.position = "none")

  g
}

resolve_label_column <- function(data, label_col, group_col) {
  if (!is.null(label_col) && label_col %in% names(data)) {
    return(label_col)
  }
  if (!is.null(group_col) && group_col %in% names(data)) {
    return(group_col)
  }
  fallback_cols <- c("trial_num", "id", "vid_ord", "group")
  available <- fallback_cols[fallback_cols %in% names(data)]
  if (length(available) > 0) {
    return(available[[1]])
  }
  NULL
}

resolve_angle_column <- function(data) {
  candidates <- c("rel_theta", "abs_theta")
  available <- candidates[candidates %in% names(data)]
  if (length(available) > 0) available[[1]] else NULL
}

build_label_data <- function(data, label_col, x_col, y_col, colour_col = NULL, padding = 1.08) {
  if (!(label_col %in% names(data))) {
    return(NULL)
  }
  label_vals <- data[[label_col]]
  if (all(is.na(label_vals))) {
    return(NULL)
  }
  valid <- !is.na(label_vals)
  if (!any(valid)) {
    return(NULL)
  }

  data <- data[valid, , drop = FALSE]
  label_vals <- label_vals[valid]
  split_data <- split(data, label_vals, drop = TRUE)

  label_list <- lapply(split_data, function(df) {
    radii <- df[[x_col]]^2 + df[[y_col]]^2
    idx <- which.max(radii)
    if (length(idx) == 0 || length(idx) > nrow(df)) {
      return(NULL)
    }
    row <- df[idx, , drop = FALSE]
    r <- sqrt(row[[x_col]]^2 + row[[y_col]]^2)
    if (!is.finite(r) || r == 0) {
      return(NULL)
    }
    edge_x <- row[[x_col]] / r
    edge_y <- row[[y_col]] / r
    data.frame(
      label_value = row[[label_col]],
      edge_x = edge_x,
      edge_y = edge_y,
      label_x = edge_x * padding,
      label_y = edge_y * padding,
      colour_value = if (!is.null(colour_col) && colour_col %in% names(row)) row[[colour_col]] else NA,
      stringsAsFactors = FALSE
    )
  })

  non_null <- !vapply(label_list, is.null, logical(1))
  if (!any(non_null)) {
    return(NULL)
  }
  label_data <- do.call(rbind, label_list[non_null])
  if (is.null(label_data) || nrow(label_data) == 0) {
    return(NULL)
  }
  if (!is.null(colour_col) && "colour_value" %in% names(label_data) &&
      all(is.na(label_data$colour_value))) {
    label_data$colour_value <- NULL
  }
  label_data
}

# ---- add_angle_rose ----------------------------------------------------------

#' Add a rose diagram of heading angles to a radiate plot
#'
#' Draws angular frequency as filled wedge polygons in the Cartesian coordinate
#' space used by \code{\link{radiate}}.  Each wedge spans one angular bin; its
#' outer radius is proportional to the proportion (or count) of frames in that
#' bin.  The layer can be faceted by passing the same column used in the parent
#' \code{radiate(panel_by = ...)} call.
#'
#' @param hd Data frame of headings, e.g. from \code{\link{pose_to_headings}}
#'   or \code{derive_headings(\ldots, frame_select = "all")}.
#' @param bins Integer; number of equal angular sectors.  Default \code{12}.
#' @param angle_col Column containing headings in radians.  Default
#'   \code{"heading"}.
#' @param group_col Column used for faceting; must match the \code{panel_by}
#'   argument of the parent \code{radiate()} call.
#' @param scale Maximum outer radius of the tallest wedge as a fraction of the
#'   unit circle radius.  Default \code{0.4}.
#' @param inner_r Inner radius of wedges.  Default \code{0}; set \code{> 0}
#'   for a donut style.
#' @param normalize \code{TRUE} (default) scales wedges by proportion;
#'   \code{FALSE} uses raw counts.
#' @param fill Wedge fill colour.  Default \code{"steelblue"}.
#' @param colour Wedge border colour.  Default \code{NA} (no border).
#' @param alpha Opacity.  Default \code{0.5}.
#' @param arc_pts Points used to approximate each wedge arc.  Default \code{20L}.
#' @param display A `circ_display()` controlling rotation, matching the parent
#'   `radiate()` plot. Default `NULL` uses the input's `display` attribute when
#'   present, otherwise the identity display.
#' @param axial Logical; when `TRUE`, mirror each observation to `angle_col + pi`
#'   before estimation, yielding a period-pi (bidirectional) result. Default
#'   `FALSE`.
#' @return A \code{geom_polygon} layer that can be added to a \code{radiate()}
#'   plot with \code{+}.
#' @export
add_angle_rose <- function(hd, bins = 12L, angle_col = "heading",
                            group_col = NULL, scale = 0.4, inner_r = 0,
                            normalize = TRUE, fill = "steelblue",
                            colour = NA, alpha = 0.5, arc_pts = 20L,
                            display = NULL, axial = FALSE) {
  stopifnot(is.data.frame(hd), angle_col %in% names(hd))
  disp  <- display %||% attr(hd, "display", exact = TRUE) %||% circ_display()
  if (isTRUE(axial)) hd <- .mirror_axial(hd, angle_col)
  ss    <- sector_summary(hd, sectors = bins, group_col = group_col,
                          angle_col = angle_col)
  y_col <- if (normalize) "proportion" else "count"
  y_max <- max(ss[[y_col]], na.rm = TRUE)
  if (y_max <= 0) y_max <- 1
  hw    <- pi / bins   # half sector width in radians

  .wedge <- function(mid, val, grp_label) {
    r_out  <- inner_r + scale * val / y_max
    thetas <- seq(mid - hw, mid + hw, length.out = arc_pts)
    xs <- c(inner_r * cos(mid - hw), r_out * cos(thetas),
            inner_r * cos(mid + hw), inner_r * cos(mid - hw))
    ys <- c(inner_r * sin(mid - hw), r_out * sin(thetas),
            inner_r * sin(mid + hw), inner_r * sin(mid - hw))
    xy <- .uc_to_display_coords(xs, ys, disp)
    data.frame(x = xy$x, y = xy$y,
               .rose_grp = paste0(grp_label, "_", round(mid, 4)),
               stringsAsFactors = FALSE)
  }

  if (!is.null(group_col)) {
    polys <- do.call(rbind, lapply(seq_len(nrow(ss)), function(i) {
      w <- .wedge(ss$mid_angle[i], ss[[y_col]][i],
                  as.character(ss[[group_col]][i]))
      w[[group_col]] <- ss[[group_col]][i]
      w
    }))
  } else {
    polys <- do.call(rbind, lapply(seq_len(nrow(ss)), function(i) {
      .wedge(ss$mid_angle[i], ss[[y_col]][i], as.character(i))
    }))
  }

  ggplot2::geom_polygon(
    data    = polys,
    mapping = ggplot2::aes(x = x, y = y, group = .rose_grp),
    fill = fill, colour = colour, alpha = alpha,
    inherit.aes = FALSE
  )
}

# ---- add_vonmises_density ----------------------------------------------------

#' Overlay a fitted von Mises density curve on a radiate plot
#'
#' Evaluates the von Mises probability density on a fine angular grid and
#' draws it as a closed polygon in the same Cartesian coordinate space used
#' by \code{\link{radiate}} and \code{\link{add_angle_rose}}.  The curve
#' peaks at \code{scale} so the two layers align when given matching
#' \code{scale} values.
#'
#' @param fit Data frame from \code{\link{vonmises_fit}}, containing at least
#'   \code{mu} and \code{kappa} columns.
#' @param scale Maximum outer radius as a fraction of the unit circle.
#'   Default \code{0.4} matches \code{add_angle_rose}.
#' @param inner_r Inner radius; default \code{0}.
#' @param group_col Column in \code{fit} for faceting; must match the
#'   \code{panel_by} argument of the parent \code{radiate()} call.
#' @param n_pts Angular evaluation points.  Default \code{360L}.
#' @param colour Outline colour.  Default \code{"steelblue"}.
#' @param linewidth Outline width.  Default \code{0.8}.
#' @param fill Fill colour.  Default \code{NA} (outline only).
#' @param alpha Opacity.  Default \code{0.8}.
#' @param display A `circ_display()` controlling rotation, matching the parent
#'   `radiate()` plot. Default `NULL` uses the input's `display` attribute when
#'   present, otherwise the identity display.
#' @param axial Logical; when `TRUE`, draw the axial (bidirectional) density of an
#'   axial fit (from `vonmises_fit(axial = TRUE)`): the curve is evaluated on
#'   doubled angles, giving two equal peaks at `mu` and `mu + pi`. Default
#'   `FALSE`.
#' @return A \code{geom_polygon} layer, or \code{NULL} if \code{fit} is all
#'   \code{NA}.
#' @export
add_vonmises_density <- function(fit, scale = 0.4, inner_r = 0,
                                  group_col = NULL, n_pts = 360L,
                                  colour = "steelblue", linewidth = 0.8,
                                  fill = NA, alpha = 0.8, display = NULL,
                                  axial = FALSE) {
  stopifnot(is.data.frame(fit))
  miss <- setdiff(c("mu", "kappa"), names(fit))
  if (length(miss))
    stop("add_vonmises_density: fit must contain columns: ",
         paste(miss, collapse = ", "),
         " -- generate with vonmises_fit()")

  thetas <- seq(-pi, pi, length.out = n_pts + 1L)[seq_len(n_pts)]
  disp   <- display %||% attr(fit, "display", exact = TRUE) %||% circ_display()

  .ring <- function(mu, kappa, grp) {
    if (is.na(mu) || is.na(kappa) || kappa < 0) return(NULL)
    th_eval <- if (isTRUE(axial)) 2 * thetas else thetas
    mu_eval <- if (isTRUE(axial)) 2 * mu     else mu
    mu_c <- circular::circular(mu_eval, units = "radians", type = "angles")
    th_c <- circular::circular(th_eval, units = "radians", type = "angles")
    d     <- as.numeric(circular::dvonmises(th_c, mu = mu_c, kappa = kappa))
    d_max <- max(d, na.rm = TRUE)
    if (!is.finite(d_max) || d_max <= 0) return(NULL)
    r <- inner_r + scale * d / d_max
    xy <- .uc_to_display_coords(r * cos(thetas), r * sin(thetas), disp)
    data.frame(x = xy$x, y = xy$y,
               .vm_grp = grp, stringsAsFactors = FALSE)
  }

  if (!is.null(group_col)) {
    if (!group_col %in% names(fit))
      stop("add_vonmises_density: '", group_col, "' not found in fit")
    polys <- do.call(rbind, lapply(seq_len(nrow(fit)), function(i) {
      w <- .ring(fit$mu[i], fit$kappa[i],
                 paste0(as.character(fit[[group_col]][i]), "_vm"))
      if (is.null(w)) return(NULL)
      w[[group_col]] <- fit[[group_col]][i]
      w
    }))
  } else {
    polys <- .ring(fit$mu[1L], fit$kappa[1L], "vm")
  }

  if (is.null(polys)) return(NULL)

  ggplot2::geom_polygon(
    data    = polys,
    mapping = ggplot2::aes(x = x, y = y, group = .vm_grp),
    colour      = colour,
    linewidth   = linewidth,
    fill        = fill,
    alpha       = alpha,
    inherit.aes = FALSE
  )
}

# ---- add_circular_kde --------------------------------------------------------

#' Overlay a non-parametric circular kernel density estimate on a radiate plot
#'
#' Estimates the circular density using \code{\link[circular]{density.circular}}
#' (no distributional assumptions) and draws it as a closed polygon in the same
#' Cartesian coordinate space used by \code{\link{radiate}} and
#' \code{\link{add_angle_rose}}.  Unlike \code{\link{add_vonmises_density}},
#' this makes no assumption about the shape of the distribution and handles
#' multimodal data naturally.
#'
#' The \code{bw} parameter is a \emph{concentration} parameter (analogous to
#' \eqn{\kappa} of the von Mises kernel) -- larger values produce a sharper,
#' data-following estimate; smaller values over-smooth towards uniform.
#' \code{NULL} (default) selects bandwidth automatically via
#' \code{\link[circular]{bw.nrd.circular}}.
#'
#' @param hd Data frame with a heading column in radians.
#' @param angle_col Column name.  Default \code{"heading"}.
#' @param group_col Column for faceting; must match \code{panel_by} in the
#'   parent \code{radiate()} call.
#' @param bw Bandwidth (concentration).  \code{NULL} uses
#'   \code{bw.nrd.circular} automatic selection.
#' @param scale Maximum outer radius as a fraction of the unit circle.
#'   Default \code{0.4}.
#' @param inner_r Inner radius.  Default \code{0}.
#' @param n_pts Number of evaluation points.  Default \code{512L}.
#' @param kernel Kernel name passed to \code{density.circular}.  Default
#'   \code{"vonmises"} (the kernel shape, not a model assumption).
#' @param colour Outline colour.  Default \code{"tomato"}.
#' @param linewidth Outline width.  Default \code{0.8}.
#' @param fill Fill colour.  Default \code{NA} (outline only).
#' @param alpha Opacity.  Default \code{0.8}.
#' @param display A `circ_display()` controlling rotation, matching the parent
#'   `radiate()` plot. Default `NULL` uses the input's `display` attribute when
#'   present, otherwise the identity display.
#' @param axial Logical; when `TRUE`, mirror each observation to `angle_col + pi`
#'   before estimation, yielding a period-pi (bidirectional) result. Default
#'   `FALSE`.
#' @return A \code{geom_polygon} layer, or \code{NULL} if estimation fails.
#' @export
add_circular_kde <- function(hd, angle_col = "heading", group_col = NULL,
                              bw = NULL, scale = 0.4, inner_r = 0,
                              n_pts = 512L, kernel = "vonmises",
                              colour = "tomato", linewidth = 0.8,
                              fill = NA, alpha = 0.8, display = NULL,
                              axial = FALSE) {
  stopifnot(is.data.frame(hd))
  if (!angle_col %in% names(hd))
    stop("add_circular_kde: column '", angle_col, "' not found")

  disp <- display %||% attr(hd, "display", exact = TRUE) %||% circ_display()
  if (isTRUE(axial)) hd <- .mirror_axial(hd, angle_col)

  .kde_ring <- function(sub, grp) {
    a <- as.numeric(sub[[angle_col]])
    a <- a[is.finite(a)]
    if (length(a) < 2L) return(NULL)
    a_circ <- circular::circular(a, units = "radians", type = "angles")
    bw_use <- if (is.null(bw)) {
      tryCatch(
        circular::bw.nrd.circular(a_circ),
        error = function(e) 5   # conservative fallback
      )
    } else {
      bw
    }
    kde <- tryCatch(
      circular::density.circular(a_circ, bw = bw_use,
                                  n = n_pts, kernel = kernel),
      error = function(e) NULL
    )
    if (is.null(kde)) return(NULL)
    thetas <- as.numeric(kde$x)
    d      <- as.numeric(kde$y)
    d_max  <- max(d, na.rm = TRUE)
    if (!is.finite(d_max) || d_max <= 0) return(NULL)
    r <- inner_r + scale * d / d_max
    xy <- .uc_to_display_coords(r * cos(thetas), r * sin(thetas), disp)
    data.frame(x = xy$x, y = xy$y,
               .kde_grp = grp, stringsAsFactors = FALSE)
  }

  if (!is.null(group_col)) {
    if (!group_col %in% names(hd))
      stop("add_circular_kde: '", group_col, "' not found in hd")
    groups <- unique(hd[[group_col]])
    polys  <- do.call(rbind, lapply(groups, function(g) {
      w <- .kde_ring(hd[hd[[group_col]] == g, , drop = FALSE],
                     paste0(as.character(g), "_kde"))
      if (is.null(w)) return(NULL)
      w[[group_col]] <- g
      w
    }))
  } else {
    polys <- .kde_ring(hd, "kde")
  }

  if (is.null(polys)) return(NULL)

  ggplot2::geom_polygon(
    data    = polys,
    mapping = ggplot2::aes(x = x, y = y, group = .kde_grp),
    colour      = colour,
    linewidth   = linewidth,
    fill        = fill,
    alpha       = alpha,
    inherit.aes = FALSE
  )
}

# ---- add_wrappedcauchy_density -----------------------------------------------

#' Overlay a fitted wrapped Cauchy density curve on a radiate plot
#'
#' Evaluates \code{\link[circular]{dwrappedcauchy}} on a fine angular grid and
#' draws it as a closed polygon in the same Cartesian space as
#' \code{\link{radiate}}.  Intended as a visual companion to
#' \code{\link{add_vonmises_density}}: overlaying both curves shows whether the
#' data favour the lighter-tailed von Mises or the heavier-tailed wrapped
#' Cauchy.
#'
#' Default colour is \code{"darkorange"} to distinguish from
#' \code{add_vonmises_density} (\code{"steelblue"}) and
#' \code{add_circular_kde} (\code{"tomato"}).
#'
#' @param fit Data frame from \code{\link{wrappedcauchy_fit}}, containing at
#'   least \code{mu} and \code{rho} columns.
#' @param scale Maximum outer radius as a fraction of the unit circle.
#'   Default \code{0.4}.
#' @param inner_r Inner radius.  Default \code{0}.
#' @param group_col Column for faceting; must match \code{panel_by} in the
#'   parent \code{radiate()} call.
#' @param n_pts Angular evaluation points.  Default \code{360L}.
#' @param colour Outline colour.  Default \code{"darkorange"}.
#' @param linewidth Outline width.  Default \code{0.8}.
#' @param fill Fill colour.  Default \code{NA} (outline only).
#' @param alpha Opacity.  Default \code{0.8}.
#' @param display A `circ_display()` controlling rotation, matching the parent
#'   `radiate()` plot. Default `NULL` uses the input's `display` attribute when
#'   present, otherwise the identity display.
#' @return A \code{geom_polygon} layer, or \code{NULL} if estimation fails.
#' @seealso \code{\link{add_vonmises_density}}, \code{\link{add_circular_kde}}
#' @export
add_wrappedcauchy_density <- function(fit, scale = 0.4, inner_r = 0,
                                       group_col = NULL, n_pts = 360L,
                                       colour = "darkorange", linewidth = 0.8,
                                       fill = NA, alpha = 0.8, display = NULL) {
  stopifnot(is.data.frame(fit))
  miss <- setdiff(c("mu", "rho"), names(fit))
  if (length(miss))
    stop("add_wrappedcauchy_density: fit must contain columns: ",
         paste(miss, collapse = ", "),
         " -- generate with wrappedcauchy_fit()")

  thetas <- seq(-pi, pi, length.out = n_pts + 1L)[seq_len(n_pts)]
  disp   <- display %||% attr(fit, "display", exact = TRUE) %||% circ_display()

  .ring <- function(mu, rho, grp) {
    if (is.na(mu) || is.na(rho) || rho < 0 || rho >= 1) return(NULL)
    mu_c  <- circular::circular(mu,     units = "radians", type = "angles")
    th_c  <- circular::circular(thetas, units = "radians", type = "angles")
    d     <- as.numeric(circular::dwrappedcauchy(th_c, mu = mu_c, rho = rho))
    d_max <- max(d, na.rm = TRUE)
    if (!is.finite(d_max) || d_max <= 0) return(NULL)
    r <- inner_r + scale * d / d_max
    xy <- .uc_to_display_coords(r * cos(thetas), r * sin(thetas), disp)
    data.frame(x = xy$x, y = xy$y,
               .wc_grp = grp, stringsAsFactors = FALSE)
  }

  if (!is.null(group_col)) {
    if (!group_col %in% names(fit))
      stop("add_wrappedcauchy_density: '", group_col, "' not found in fit")
    polys <- do.call(rbind, lapply(seq_len(nrow(fit)), function(i) {
      w <- .ring(fit$mu[i], fit$rho[i],
                 paste0(as.character(fit[[group_col]][i]), "_wc"))
      if (is.null(w)) return(NULL)
      w[[group_col]] <- fit[[group_col]][i]
      w
    }))
  } else {
    polys <- .ring(fit$mu[1L], fit$rho[1L], "wc")
  }

  if (is.null(polys)) return(NULL)

  ggplot2::geom_polygon(
    data    = polys,
    mapping = ggplot2::aes(x = x, y = y, group = .wc_grp),
    colour      = colour,
    linewidth   = linewidth,
    fill        = fill,
    alpha       = alpha,
    inherit.aes = FALSE
  )
}

# ---- add_critical_r ----------------------------------------------------------

#' Add a critical resultant-length circle to a radiate plot
#'
#' Draws an inner reference circle at the \emph{critical mean resultant length}
#' -- the smallest \eqn{\bar R} at which a circular significance test reaches
#' \code{alpha} for the given sample size.  If a group's mean-direction arrow
#' (\code{\link{add_heading_arrow}}) extends beyond this circle, that group's
#' headings are significantly directed at the \code{alpha} level.  This is the
#' convention used by Oriana and similar circular-statistics software.
#'
#' Two tests are supported:
#' \describe{
#'   \item{\code{"rayleigh"} (default)}{Tests against uniformity with no
#'     hypothesised direction.  Critical value
#'     \eqn{\bar R_{crit} = \sqrt{-\log(\alpha) / n}} (asymptotic; accurate
#'     for \eqn{n \ge 10}).}
#'   \item{\code{"vtest"}}{Tests against a specific direction.  Critical value
#'     \eqn{\bar R_{crit} = z_\alpha / \sqrt{2n}} at its most powerful (when the
#'     observed mean direction equals the hypothesised \eqn{\mu_0}); the
#'     effective threshold rises as the observed direction departs from
#'     \eqn{\mu_0}, so this circle is a lower bound.}
#' }
#'
#' Sample size \code{n} is taken per group from \code{hd}.  When
#' \code{group_col} matches the parent \code{radiate(panel_by = ...)} argument,
#' one circle is drawn per panel at the radius appropriate to that panel's
#' \code{n}.  For groups overlaid in a single panel, set \code{per_group = TRUE}
#' to draw one circle per group (colour-matched), or \code{per_group = FALSE}
#' (default) to draw a single conservative circle using the smallest \code{n}
#' (largest critical radius).
#'
#' @param hd Data frame of headings with a heading column (radians).
#' @param alpha Significance level.  Default \code{0.05}.
#' @param test \code{"rayleigh"} (default) or \code{"vtest"}.
#' @param angle_col Heading column name.  Default \code{"heading"}.
#' @param group_col Column identifying groups.  \code{NULL} pools all rows.
#' @param per_group Logical.  When \code{group_col} is set but the plot is not
#'   faceted, draw one circle per group (\code{TRUE}) or a single conservative
#'   circle (\code{FALSE}, default).  Ignored when faceting (always per panel).
#' @param colour Circle colour.  Default \code{"firebrick"}.  When
#'   \code{per_group = TRUE} and \code{colour_by_group = TRUE} this is overridden
#'   by the colour scale.
#' @param colour_by_group Logical.  When \code{per_group = TRUE}, map each
#'   circle's colour to its group (\code{TRUE}, default) or draw every circle in
#'   the fixed \code{colour} while still attaching the group column so the
#'   circles facet (\code{FALSE}).  Use \code{FALSE} to keep per-panel circles a
#'   single colour without injecting the grouping levels into the parent plot's
#'   colour scale.  Ignored unless \code{per_group = TRUE}.
#' @param linetype Line type.  Default \code{"dashed"}.
#' @param linewidth Line width.  Default \code{0.6}.
#' @param n_pts Points used to approximate each circle.  Default \code{200L}.
#' @return A \code{geom_path} layer, or \code{NULL} if no group has \code{n >= 2}.
#' @seealso \code{\link{add_heading_arrow}}, \code{\link{add_circ}}
#' @export
add_critical_r <- function(hd, alpha = 0.05,
                            test = c("rayleigh", "vtest"),
                            angle_col = "heading", group_col = NULL,
                            per_group = FALSE, colour = "firebrick",
                            colour_by_group = TRUE,
                            linetype = "dashed", linewidth = 0.6,
                            n_pts = 200L) {
  test <- match.arg(test)
  stopifnot(is.data.frame(hd), alpha > 0, alpha < 1)
  if (!angle_col %in% names(hd))
    stop("add_critical_r: column '", angle_col, "' not found")

  .r_crit <- function(n) {
    if (n < 2L) return(NA_real_)
    switch(test,
      rayleigh = sqrt(-log(alpha) / n),
      vtest    = stats::qnorm(1 - alpha) / sqrt(2 * n)
    )
  }

  .circle_df <- function(r, grp) {
    th <- seq(0, 2 * pi, length.out = n_pts)
    data.frame(x = r * cos(th), y = r * sin(th),
               .cr_grp = grp, stringsAsFactors = FALSE)
  }

  # ---- no grouping: single circle from pooled n ----
  if (is.null(group_col)) {
    n <- sum(is.finite(hd[[angle_col]]))
    r <- .r_crit(n)
    if (is.na(r)) return(NULL)
    df <- .circle_df(r, "all")
    return(ggplot2::geom_path(
      data = df, mapping = ggplot2::aes(x = x, y = y, group = .cr_grp),
      colour = colour, linetype = linetype, linewidth = linewidth,
      inherit.aes = FALSE
    ))
  }

  if (!group_col %in% names(hd))
    stop("add_critical_r: '", group_col, "' not found")

  groups <- unique(hd[[group_col]])
  ns <- vapply(groups, function(g)
    sum(is.finite(hd[[angle_col]][hd[[group_col]] == g])), integer(1L))

  # ---- single conservative circle (overlaid, per_group = FALSE) ----
  if (!per_group) {
    n_min <- min(ns[ns >= 2L], na.rm = TRUE)
    if (!is.finite(n_min)) return(NULL)
    df <- .circle_df(.r_crit(n_min), "conservative")
    return(ggplot2::geom_path(
      data = df, mapping = ggplot2::aes(x = x, y = y, group = .cr_grp),
      colour = colour, linetype = linetype, linewidth = linewidth,
      inherit.aes = FALSE
    ))
  }

  # ---- one circle per group (faceted, or overlaid colour-matched) ----
  parts <- lapply(seq_along(groups), function(i) {
    r <- .r_crit(ns[i])
    if (is.na(r)) return(NULL)
    df <- .circle_df(r, as.character(groups[i]))
    df[[group_col]] <- groups[i]
    df
  })
  parts <- parts[!vapply(parts, is.null, logical(1L))]
  if (!length(parts)) return(NULL)
  circ_df <- do.call(rbind, parts)

  # When colour_by_group is FALSE the group column is retained (so the circles
  # still facet alongside the parent plot) but colour is drawn as a fixed value
  # rather than mapped -- this avoids injecting the grouping levels into the
  # parent plot's colour scale (e.g. when the panels are already coloured by a
  # different variable).
  if (!colour_by_group)
    return(ggplot2::geom_path(
      data = circ_df,
      mapping = ggplot2::aes(x = x, y = y, group = .cr_grp),
      colour = colour, linetype = linetype, linewidth = linewidth,
      inherit.aes = FALSE
    ))

  ggplot2::geom_path(
    data = circ_df,
    mapping = ggplot2::aes(x = x, y = y,
                           group = .cr_grp,
                           colour = .data[[group_col]]),
    linetype = linetype, linewidth = linewidth, inherit.aes = FALSE
  )
}

# ---- add_critical_v_line -----------------------------------------------------

#' Add a V-test significance boundary to a radiate plot
#'
#' Draws the decision boundary for the Rayleigh V test against a specified
#' direction \code{mu0}.  Unlike the Rayleigh test (a circle, see
#' \code{\link{add_critical_r}}), the V test privileges one direction, so its
#' boundary is a \emph{straight line} perpendicular to \code{mu0} at distance
#' \eqn{c = z_\alpha / \sqrt{2n}} from the centre.  A mean-direction arrow
#' (\code{\link{add_heading_arrow}}) is V-significant if and only if its tip
#' falls on the far side of this line -- equivalently, if its projection onto
#' \code{mu0} exceeds \eqn{c}.
#'
#' The line is clipped to the unit circle (drawn as a chord).  With
#' \code{show_region = TRUE} the circular segment beyond the line -- the
#' rejection region -- is shaded.
#'
#' Sample size \code{n} is taken per group from \code{hd}, with the same
#' options as \code{\link{add_critical_r}}: per-panel when faceting,
#' per-group when \code{per_group = TRUE}, or a single conservative boundary
#' (smallest \code{n}, largest \eqn{c}) otherwise.
#'
#' @param hd Data frame of headings with a heading column (radians).
#' @param mu0 Hypothesised direction in radians (unit-circle convention).
#' @param alpha Significance level.  Default \code{0.05}.
#' @param angle_col Heading column name.  Default \code{"heading"}.
#' @param group_col Column identifying groups.  \code{NULL} pools all rows.
#' @param per_group Logical.  Draw one boundary per group (\code{TRUE}) or a
#'   single conservative boundary (\code{FALSE}, default).  Ignored when
#'   faceting, where each panel gets its own boundary.
#' @param show_region Logical; shade the rejection segment.  Default
#'   \code{FALSE}.
#' @param colour Line colour.  Default \code{"firebrick"}.
#' @param linetype Line type.  Default \code{"dashed"}.
#' @param linewidth Line width.  Default \code{0.6}.
#' @param region_fill Fill colour for the rejection region.  Default
#'   \code{"firebrick"}.
#' @param region_alpha Fill opacity.  Default \code{0.08}.
#' @param axial Logical. Render the V-test boundary for axial (bidirectional,
#'   mod-pi) data: the decision chord is mirrored to both poles. Default `FALSE`.
#' @param n_pts Points approximating the rejection arc.  Default \code{100L}.
#' @return A list of ggplot2 layers, or \code{NULL} if no group has a boundary
#'   inside the unit circle.
#' @seealso \code{\link{add_critical_r}}, \code{\link{add_heading_arrow}}
#' @export
add_critical_v_line <- function(hd, mu0, alpha = 0.05,
                                 angle_col = "heading", group_col = NULL,
                                 per_group = FALSE, show_region = FALSE,
                                 colour = "firebrick", linetype = "dashed",
                                 linewidth = 0.6, region_fill = "firebrick",
                                 region_alpha = 0.08, axial = FALSE, n_pts = 100L) {
  stopifnot(is.data.frame(hd), alpha > 0, alpha < 1)
  if (missing(mu0)) stop("add_critical_v_line: 'mu0' (hypothesised direction) is required")
  if (!angle_col %in% names(hd))
    stop("add_critical_v_line: column '", angle_col, "' not found")

  z <- stats::qnorm(1 - alpha)
  .c_of_n <- function(n) if (n < 2L) NA_real_ else z / sqrt(2 * n)

  # Geometry for one boundary at perpendicular distance c along mu0.
  # Returns list(chord = data.frame(x,y,xend,yend), region = data.frame(x,y)).
  .one_pole <- function(cc, grp, mu) {
    half <- sqrt(1 - cc^2)
    ux <- cos(mu); uy <- sin(mu)
    px <- -uy;     py <- ux
    foot <- c(cc * ux, cc * uy)
    e1 <- foot + half * c(px, py)
    e2 <- foot - half * c(px, py)
    chord <- data.frame(x = e1[1], y = e1[2], xend = e2[1], yend = e2[2],
                        .v_grp = grp, stringsAsFactors = FALSE)
    region <- NULL
    if (show_region) {
      span <- acos(cc)
      phis <- seq(mu - span, mu + span, length.out = n_pts)
      region <- data.frame(
        x = c(cos(phis), e2[1], e1[1]),
        y = c(sin(phis), e2[2], e1[2]),
        .v_grp = grp, stringsAsFactors = FALSE
      )
    }
    list(chord = chord, region = region)
  }
  .geom <- function(cc, grp) {
    if (is.na(cc) || cc >= 1) return(NULL)        # line outside the disc
    poles <- list(.one_pole(cc, grp, mu0))
    if (isTRUE(axial))
      poles <- c(poles, list(.one_pole(cc, paste0(grp, ".pi"), mu0 + pi)))
    list(
      chord  = do.call(rbind, lapply(poles, `[[`, "chord")),
      region = {
        rg <- lapply(poles, `[[`, "region")
        rg <- rg[!vapply(rg, is.null, logical(1L))]
        if (length(rg)) do.call(rbind, rg) else NULL
      }
    )
  }

  # Collect (c, group-key) pairs to draw
  if (is.null(group_col)) {
    keys <- list(list(cc = .c_of_n(sum(is.finite(hd[[angle_col]]))),
                      g = "all", facet = NULL))
  } else {
    if (!group_col %in% names(hd))
      stop("add_critical_v_line: '", group_col, "' not found")
    groups <- unique(hd[[group_col]])
    ns <- vapply(groups, function(g)
      sum(is.finite(hd[[angle_col]][hd[[group_col]] == g])), integer(1L))
    if (per_group) {
      keys <- lapply(seq_along(groups), function(i)
        list(cc = .c_of_n(ns[i]), g = as.character(groups[i]),
             facet = groups[i]))
    } else {
      n_min <- min(ns[ns >= 2L], na.rm = TRUE)
      if (!is.finite(n_min)) return(NULL)
      keys <- list(list(cc = .c_of_n(n_min), g = "conservative", facet = NULL))
    }
  }

  geoms <- lapply(keys, function(k) {
    g <- .geom(k$cc, k$g)
    if (is.null(g)) return(NULL)
    if (!is.null(k$facet)) {
      g$chord[[group_col]] <- k$facet
      if (!is.null(g$region)) g$region[[group_col]] <- k$facet
    }
    g
  })
  geoms <- geoms[!vapply(geoms, is.null, logical(1L))]
  if (!length(geoms)) return(NULL)

  chords  <- do.call(rbind, lapply(geoms, `[[`, "chord"))
  regions <- do.call(rbind, lapply(geoms, `[[`, "region"))

  layers <- list()
  if (!is.null(regions) && nrow(regions)) {
    layers[[length(layers) + 1L]] <- ggplot2::geom_polygon(
      data = regions,
      mapping = ggplot2::aes(x = x, y = y, group = .v_grp),
      fill = region_fill, alpha = region_alpha, colour = NA,
      inherit.aes = FALSE
    )
  }
  layers[[length(layers) + 1L]] <- ggplot2::geom_segment(
    data = chords,
    mapping = ggplot2::aes(x = x, y = y, xend = xend, yend = yend,
                           group = .v_grp),
    colour = colour, linetype = linetype, linewidth = linewidth,
    inherit.aes = FALSE
  )
  layers
}
