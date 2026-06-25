# Circular boxplot (Buttarazzi, Pandolfo & Porzio 2018, Biometrics;
# doi:10.1111/biom.12889). Algorithm reimplemented from their bpDir package
# (GPL) in radiatR style: work in signed offset-from-median coordinates
# rel in (-pi, pi], so the antimedian sits at the +/-pi branch cut and the
# "fences must not pass the antimedian" rule is a clamp to +/-pi.

# Closed-form fence multiplier: at the sample's estimated von Mises
# concentration, the factor c such that Q3 + c*IQR lands on the 99.65% quantile.
.circ_box_constant <- function(tc) {
  conc <- as.numeric(circular::A1inv(circular::rho.circular(tc)))
  # As kappa -> Inf the von Mises quantile ratio converges to the Gaussian
  # Tukey value (~1.5). Cap concentration at a large finite value so tightly
  # clustered data (rho rounding to 1, A1inv = Inf) does not yield NaN. The cap
  # is 1e3 rather than 1e4 because circular::qvonmises loses accuracy past ~5e3
  # (its quantiles saturate, collapsing the constant to ~0); at 1e3 the constant
  # is a clean ~1.4997.
  if (!is.finite(conc)) conc <- 1e3
  conc <- min(conc, 1e3)
  qk <- function(p) {
    q <- as.numeric(circular::qvonmises(p, mu = circular::circular(0), kappa = conc))
    ((q + pi) %% (2 * pi)) - pi               # signed arc from 0
  }
  iqr <- qk(0.75) - qk(0.25)
  list(constant = (qk(0.9965) - qk(0.75)) / iqr, kappa = conc)
}

.circ_boxplot_core <- function(theta, axial) {
  n <- length(theta)
  na_spec <- function(reason, drawable = FALSE)
    list(median = NA_real_, antimedian = NA_real_, hinges = c(NA_real_, NA_real_),
         box_arc = c(NA_real_, NA_real_), constant = NA_real_, kappa = NA_real_,
         fences = c(NA_real_, NA_real_), whiskers = c(NA_real_, NA_real_),
         far_out = numeric(0), n = n, axial = axial,
         drawable = drawable, reason = reason)

  if (n < 4L) return(na_spec("fewer than 4 observations; boxplot not drawn"))

  tc <- circular::circular(theta %% (2 * pi), units = "radians",
                           modulo = "2pi", zero = 0, rotation = "counter")
  M  <- suppressWarnings(as.numeric(circular::median.circular(tc)))
  if (!is.finite(M)) return(na_spec("circular median is not unique; boxplot not drawn"))
  M  <- M %% (2 * pi)

  # signed offsets from the median; drop ties exactly at the antimedian (|rel|=pi)
  rel  <- ((theta - M + pi) %% (2 * pi)) - pi
  rel  <- sort(rel[abs(abs(rel) - pi) > 1e-9])
  m    <- length(rel)
  if (m < 4L) return(na_spec("fewer than 4 usable observations; boxplot not drawn"))

  # Tukey outer-inward depth: rank from both ends (the antimedian) toward 0.
  dm <- round((1 + m) / 2 - 0.1)        # depth of the median (bpDir)
  dq <- (1 + dm) / 2                    # depth of the quartiles (may be x.5)
  lo <- function(k) rel[k]              # clockwise hinge side (rel < 0)
  hi <- function(k) rel[m + 1 - k]      # anticlockwise hinge side (rel > 0)
  if (dq %% 1 == 0) {
    h_lo <- lo(dq); h_hi <- hi(dq)
  } else {
    h_lo <- mean(c(lo(dq - 0.5), lo(dq + 0.5)))
    h_hi <- mean(c(hi(dq - 0.5), hi(dq + 0.5)))
  }
  d <- h_hi - h_lo                      # IQR arc length

  cc      <- .circ_box_constant(tc)
  f_hi    <- min(h_hi + cc$constant * d,  pi)   # clamp at the antimedian
  f_lo    <- max(h_lo - cc$constant * d, -pi)

  inside_hi <- rel[rel >= h_hi & rel <= f_hi]
  inside_lo <- rel[rel <= h_lo & rel >= f_lo]
  w_hi <- if (length(inside_hi)) max(inside_hi) else h_hi
  w_lo <- if (length(inside_lo)) min(inside_lo) else h_lo
  far  <- rel[rel > w_hi | rel < w_lo]

  abs_of <- function(x) (M + x) %% (2 * pi)

  # advisory: near-uniform / antipodally symmetric (box ~ half-circle, c ~ 0.5)
  reason <- NA_character_
  if (is.finite(cc$constant) && (d >= 0.95 * pi || cc$constant <= 0.55))
    reason <- "near-uniform or antipodal: box spans ~half the circle; interpretation not recommended"

  list(median     = M,
       antimedian = (M + pi) %% (2 * pi),
       hinges     = abs_of(c(h_lo, h_hi)),
       box_arc    = abs_of(c(h_lo, h_hi)),
       constant   = cc$constant,
       kappa      = cc$kappa,
       fences     = abs_of(c(f_lo, f_hi)),
       whiskers   = abs_of(c(w_lo, w_hi)),
       far_out    = if (length(far)) abs_of(far) else numeric(0),
       n = m, axial = axial, drawable = TRUE, reason = reason)
}

# Axial boxplot: double the axes (mod pi -> 2pi), run the core where the
# distribution is unimodal (so the constant/kappa are taken there, per the
# paper's 4.4), then halve every location back to [0, pi).
.circ_boxplot_axial <- function(theta) {
  s <- .circ_boxplot_core((2 * theta) %% (2 * pi), axial = TRUE)
  if (!isTRUE(s$drawable)) return(s)
  halve <- function(x) (x / 2) %% pi
  s$median     <- halve(s$median)
  s$antimedian <- halve(s$antimedian)
  s$hinges     <- halve(s$hinges)
  s$box_arc    <- halve(s$box_arc)
  s$fences     <- halve(s$fences)
  s$whiskers   <- halve(s$whiskers)
  s$far_out    <- if (length(s$far_out)) halve(s$far_out) else numeric(0)
  s
}

#' Circular boxplot statistics (Tukey-like, for circular and axial data)
#'
#' Computes the five-number summary and fences of a circular boxplot following
#' Buttarazzi, Pandolfo & Porzio (2018): observations are depth-ranked outward
#' from the antimedian, the box spans the central ~50% (hinge to hinge through
#' the median), and the fence multiplier is obtained in closed form from the
#' von Mises quantiles at the sample's estimated concentration (so ~0.7% of
#' observations are flagged as far-out under that reference). Pairs with
#' [add_circular_boxplot()] for rendering.
#'
#' @param hd A data frame with a column of heading angles in radians
#'   (unit-circle convention), or a numeric vector of angles.
#' @param angle_col Name of the angle column when `hd` is a data frame.
#'   Default `"heading"`.
#' @param axial Logical. Treat the angles as axial (bidirectional, mod-pi):
#'   angles are doubled, the boxplot computed, and locations halved back to
#'   `[0, pi)`; the fence multiplier is taken on the doubled data. Default
#'   `FALSE`.
#' @return A list with `median`, `antimedian`, `hinges`, `box_arc`,
#'   `constant`, `kappa`, `fences`, `whiskers`, `far_out` (all radians,
#'   unit-circle convention), `n`, `axial`, `drawable`, and `reason`. When
#'   `drawable` is `FALSE` (non-unique median or `n < 4`) the location fields
#'   are `NA`; `reason` may also carry a non-fatal advisory while `drawable`
#'   remains `TRUE` (near-uniform data).
#' @references Buttarazzi, D., Pandolfo, G. & Porzio, G. C. (2018). A boxplot
#'   for circular data. \emph{Biometrics} 74(4), 1492--1501.
#'   \doi{10.1111/biom.12889}
#' @source Algorithm reimplemented from the \pkg{bpDir} package.
#' @seealso [add_circular_boxplot()], [circ_summarise()], [vonmises_fit()]
#' @importFrom circular circular median.circular A1inv rho.circular qvonmises
#' @export
circ_boxplot_stats <- function(hd, angle_col = "heading", axial = FALSE) {
  theta <- if (is.data.frame(hd)) {
    if (!angle_col %in% names(hd))
      stop("`angle_col` '", angle_col, "' not found in `hd`.")
    hd[[angle_col]]
  } else hd
  theta <- as.numeric(theta)
  theta <- theta[is.finite(theta)]
  if (isTRUE(axial)) return(.circ_boxplot_axial(theta))
  .circ_boxplot_core(theta, axial = FALSE)
}

#' Add a circular boxplot layer to a radial plot
#'
#' Renders the [circ_boxplot_stats()] summary onto a [radiate()] polar plot in
#' the Buttarazzi et al. (2018) style: a filled box band between the hinges,
#' whisker arcs, short crossbars at the median and hinges, far-out points, and
#' an optional median arrow. For `axial = TRUE` every element is drawn at both
#' poles of the axis (offsets are computed with mod-pi arithmetic so the box is
#' a short band even when the axis sits on the 0/pi seam). Composable with `+`.
#'
#' @inheritParams circ_boxplot_stats
#' @param radius Perimeter radius for the box/whiskers. Default `1.1` (just
#'   outside the unit circle).
#' @param width Radial thickness of the box band. Default `0.05`.
#' @param colour,color Outline colour for box, whiskers, crossbars, far-out.
#'   Default `"black"`. `color` is the American-spelling alias.
#' @param box_fill Fill colour of the box band. Default `"grey90"`.
#' @param farout_shape Point shape for far-out values. Default `8` (star).
#' @param show_median_arrow Draw a centre-to-edge arrow at the median direction
#'   (a median pointer, not a rho resultant). Default `FALSE`.
#' @param linewidth Line width for arcs/crossbars. Default `0.8`.
#' @param n_theta Points per arc. Default `200`.
#' @param display A [circ_display()] object; when `NULL` (default), taken from
#'   `attr(hd, "display")`, falling back to `circ_display()`.
#' @param panel_by Optional name of a column in `hd` identifying facet panels
#'   (the same column passed to `radiate(panel_by = )`). When set, a separate
#'   boxplot is computed and drawn per level so each facet shows its own
#'   summary; the layer data is tagged with this column so it faces correctly. A
#'   level that is not drawable (fewer than 4 usable observations or a non-unique
#'   median) is skipped with a warning while the others still draw. Default
#'   `NULL` draws a single boxplot from all of `hd`.
#' @param theme Optional radiate theme name (e.g. "void", "minimal", "dark").
#'   When set, the box, whiskers and crossbars take that theme's chrome colour
#'   (matching the circle and ticks), overriding \code{colour}.
#' @return A list of ggplot2 layers, or `NULL` when the boxplot is not drawable
#'   (a `warning()` is emitted with the reason).
#' @references Buttarazzi, D., Pandolfo, G. & Porzio, G. C. (2018). A boxplot
#'   for circular data. \emph{Biometrics} 74(4), 1492--1501.
#'   \doi{10.1111/biom.12889}
#' @seealso [circ_boxplot_stats()], [radiate()]
#' @importFrom ggplot2 geom_polygon geom_path geom_segment geom_point aes
#' @importFrom rlang .data
#' @importFrom grid arrow unit
#' @export
add_circular_boxplot <- function(hd, angle_col = "heading", axial = FALSE,
                                 radius = 1.1, width = 0.05,
                                 colour = "black", box_fill = "grey90",
                                 farout_shape = 8, show_median_arrow = FALSE,
                                 linewidth = 0.8, n_theta = 200L, display = NULL,
                                 panel_by = NULL, theme = NULL, color = NULL) {
  .apply_spelling_aliases()
  if (!is.null(theme)) {
    st     <- .radial_style(theme)
    colour <- st$col_of(st$ax$line)
  }
  if (is.null(display))
    display <- (if (is.data.frame(hd)) attr(hd, "display", exact = TRUE) else NULL) %||% circ_display()

  r_in  <- radius - width / 2
  r_out <- radius + width / 2

  arc_xy <- function(a0, a1, r, gid) {
    th <- seq(a0, a1, length.out = n_theta)
    xy <- .uc_to_display_coords(r * cos(th), r * sin(th), display)
    data.frame(.x = xy$x, .y = xy$y, .g = gid)
  }
  radial_xy <- function(a, r0, r1) {
    p0 <- .uc_to_display_coords(r0 * cos(a), r0 * sin(a), display)
    p1 <- .uc_to_display_coords(r1 * cos(a), r1 * sin(a), display)
    data.frame(.x = p0$x, .y = p0$y, .xend = p1$x, .yend = p1$y)
  }

  # Build the five geometry frames (box/whiskers/crossbars/arrow/far-out) for one
  # boxplot summary `s`. Signed offset from the median: for axial data the
  # locations live on a mod-pi circle, so offsets are taken mod pi (range
  # (-pi/2, pi/2]) and mirrored at the axis and its antipode (+pi); mod-2pi would
  # draw arcs the long way round the 0/pi seam.
  build_geom <- function(s) {
    M   <- s$median
    off <- if (axial) function(a) ((a - M + pi/2) %% pi)     - pi/2
           else        function(a) ((a - M + pi)   %% (2*pi)) - pi
    h   <- sort(off(s$hinges)); w <- sort(off(s$whiskers))
    fo  <- off(s$far_out)
    offs <- if (axial) c(0, pi) else 0
    box_df <- whisk_df <- cross_df <- arrow_df <- fo_df <- NULL
    for (k in seq_along(offs)) {
      base <- M + offs[k]
      # box band: outer arc forward + inner arc back -> one closed ring per pole
      outer <- arc_xy(base + h[1], base + h[2], r_out, k)
      inner <- arc_xy(base + h[2], base + h[1], r_in,  k)
      box_df <- rbind(box_df, outer, inner)
      # whiskers: hinge -> whisker end on each side, along the perimeter
      whisk_df <- rbind(whisk_df,
        arc_xy(base + h[2], base + w[2], radius, 2*k - 1),
        arc_xy(base + w[1], base + h[1], radius, 2*k))
      # crossbars at median + hinges (short radial segments)
      cross_df <- rbind(cross_df,
        radial_xy(base + 0,    r_in, r_out),
        radial_xy(base + h[1], r_in, r_out),
        radial_xy(base + h[2], r_in, r_out))
      if (show_median_arrow) arrow_df <- rbind(arrow_df, radial_xy(base, 0, radius))
      if (length(fo)) {
        xy <- .uc_to_display_coords(radius * cos(base + fo), radius * sin(base + fo), display)
        fo_df <- rbind(fo_df, data.frame(.x = xy$x, .y = xy$y))
      }
    }
    list(box = box_df, whisk = whisk_df, cross = cross_df,
         arrow = arrow_df, fo = fo_df)
  }

  # One boxplot, or one per panel level when panel_by names a column of hd.
  faceted <- !is.null(panel_by) && is.data.frame(hd) && panel_by %in% names(hd)
  if (!faceted) {
    s <- circ_boxplot_stats(hd, angle_col = angle_col, axial = axial)
    if (!isTRUE(s$drawable)) {
      warning("add_circular_boxplot: ", s$reason, call. = FALSE)
      return(NULL)
    }
    if (!is.na(s$reason)) warning("add_circular_boxplot: ", s$reason, call. = FALSE)
    geom <- build_geom(s)
  } else {
    groups <- split(seq_len(nrow(hd)), hd[[panel_by]])
    geom   <- list(box = NULL, whisk = NULL, cross = NULL, arrow = NULL, fo = NULL)
    for (lv in names(groups)) {
      s <- circ_boxplot_stats(hd[groups[[lv]], , drop = FALSE],
                              angle_col = angle_col, axial = axial)
      if (!isTRUE(s$drawable)) {
        warning("add_circular_boxplot [", lv, "]: ", s$reason, call. = FALSE)
        next
      }
      if (!is.na(s$reason))
        warning("add_circular_boxplot [", lv, "]: ", s$reason, call. = FALSE)
      g <- build_geom(s)
      for (nm in names(geom)) {
        if (!is.null(g[[nm]])) {
          g[[nm]][[panel_by]] <- lv
          geom[[nm]] <- rbind(geom[[nm]], g[[nm]])
        }
      }
    }
    if (is.null(geom$box)) return(NULL)   # no level was drawable
  }

  box_df   <- geom$box
  whisk_df <- geom$whisk
  cross_df <- geom$cross
  arrow_df <- geom$arrow
  fo_df    <- geom$fo

  layers <- list(
    ggplot2::geom_polygon(data = box_df,
      ggplot2::aes(x = .data$.x, y = .data$.y, group = .data$.g),
      fill = box_fill, colour = colour, linewidth = linewidth,
      alpha = 0.6, inherit.aes = FALSE),
    ggplot2::geom_path(data = whisk_df,
      ggplot2::aes(x = .data$.x, y = .data$.y, group = .data$.g),
      colour = colour, linewidth = linewidth, inherit.aes = FALSE),
    ggplot2::geom_segment(data = cross_df,
      ggplot2::aes(x = .data$.x, y = .data$.y, xend = .data$.xend, yend = .data$.yend),
      colour = colour, linewidth = linewidth, inherit.aes = FALSE)
  )
  if (show_median_arrow)
    layers <- c(layers, list(ggplot2::geom_segment(data = arrow_df,
      ggplot2::aes(x = .data$.x, y = .data$.y, xend = .data$.xend, yend = .data$.yend),
      colour = colour, linewidth = linewidth,
      arrow = grid::arrow(length = grid::unit(0.2, "cm")), inherit.aes = FALSE)))
  if (!is.null(fo_df))
    layers <- c(layers, list(ggplot2::geom_point(data = fo_df,
      ggplot2::aes(x = .data$.x, y = .data$.y),
      shape = farout_shape, colour = colour, inherit.aes = FALSE)))
  layers
}
