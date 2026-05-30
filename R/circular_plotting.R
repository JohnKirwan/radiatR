# Circular plotting utilities for radiatR trajectories
#

# ---- annotation layers -------------------------------------------------------

#' Create tick marks at the cardinal directions.
#'
#' Generates a `geom_segment()` layer containing small tick marks at north,
#' south, east, and west. The layer can be added to any ggplot.
#'
#' @return A `geom_segment()` layer.
#'
#' @examples
#' library(ggplot2)
#' ggplot() +
#'   coord_fixed() +
#'   add_ticks()
#' @export
add_ticks <- function() {
  tick_df <- data.frame(
    x = c(0, .66, .95, .66, 0, -.66, -.95, -.66),
    y = c(.95, .66, 0, -.66, -.95, -.66, 0, .66),
    xend = c(0, .74, 1.05, .74, 0, -.74, -1.05, -.74),
    yend = c(1.05, .74, 0, -.74, -1.05, -.74, 0, .74)
  )

  ggplot2::geom_segment(
    data = tick_df,
    mapping = ggplot2::aes(
      x = .data$x,
      y = .data$y,
      xend = .data$xend,
      yend = .data$yend
    ),
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
add_circ <- function(
  radius = 1, circle_color = "grey60", circle_alpha = 1, circle_size = 1) {

  list(
    ggplot2::annotate(
      "path",
      x = radius * cos(seq(0, 2 * pi, length.out = 1000)),
      y = radius * sin(seq(0, 2 * pi, length.out = 1000)),
      color = circle_color, alpha = circle_alpha, linewidth = circle_size
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
#' Draws two dashed lines through the centre of the unit circle — one
#' horizontal (0°/180°) and one vertical (90°/270°) — dividing the arena into
#' four quadrants. The lines extend to the arena boundary (unit circle).
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

#' Label the four diagonal directions.
#'
#' Provides a list of annotation layers that mark 45, 135, 225, and 315 degrees on a
#' unit circle.
#'
#' @return A list of ggplot2 annotation layers.
#'
#' @examples
#' library(ggplot2)
#' ggplot() +
#'   coord_fixed() +
#'   degree_labs()
#' @export
degree_labs <- function() {
  list(
    ggplot2::annotate("text", x =  .85, y =  .85, label = paste0("45", "\U00B0")),
    ggplot2::annotate("text", x =  .85, y = -.85, label = paste0("135", "\U00B0")),
    ggplot2::annotate("text", x = -.85, y = -.85, label = paste0("225", "\U00B0")),
    ggplot2::annotate("text", x = -.85, y =  .85, label = paste0("315", "\U00B0"))
  )
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
  mean_angle <- as.numeric(circular::mean.circular(angles))
  rho        <- as.numeric(circular::rho.circular(angles))

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

  idx <- if (is.null(panel_col)) {
    unique_ids <- unique(ids)
    ((match(ids, unique_ids) - 1L) %% n_int) + 1L
  } else {
    if (!panel_col %in% names(data))
      stop("`panel_col` '", panel_col, "' not found in data.")
    panels <- data[[panel_col]]
    out <- integer(length(ids))
    for (p in unique(panels)) {
      rows        <- panels == p
      unique_ids_p <- unique(ids[rows])
      out[rows]   <- ((match(ids[rows], unique_ids_p) - 1L) %% n_int) + 1L
    }
    out
  }

  data[[out_col]] <- factor(idx, levels = seq_len(n_int))
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
#' modified before plotting — for example to replace the `density` column with
#' a Bayesian posterior predictive density obtained from `brms` or another
#' modelling package.
#'
#' Three built-in estimation methods are provided:
#'
#' * `"vonmises"` — fit a von Mises distribution by MLE
#'   ([circular::mle.vonmises()]) and evaluate the fitted density on a regular
#'   grid of `n_theta` angles. Bootstrap confidence bands are available via
#'   `boot_reps`.
#' * `"kernel"` — circular kernel density estimate
#'   ([circular::density.circular()]) with bandwidth chosen by
#'   [circular::bw.nrd.circular()] unless `bw` is supplied.
#' * `"histogram"` — angular bin counts (a circular rose diagram); `bins`
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
#'   (10° each).
#' @param bw Bandwidth passed to [circular::density.circular()]. `NULL`
#'   uses [circular::bw.nrd.circular()].
#' @param boot_reps Integer. Number of bootstrap replicates for a `"vonmises"`
#'   confidence band. `0` (default) skips the bootstrap. Ignored for `"kernel"`
#'   and `"histogram"`.
#' @param boot_alpha Significance level for the bootstrap band. Default `0.05`
#'   produces a 95\% interval.
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
                                     boot_alpha  = 0.05) {
  method     <- match.arg(method)
  use_colour <- !is.null(colour_col) && colour_col %in% names(headings_df)

  if (!heading_col %in% names(headings_df))
    stop("`heading_col` '", heading_col, "' not found in headings_df.")

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
#' Takes a data frame of `(theta, density)` pairs — from any source: MLE,
#' kernel estimation, Bayesian posterior predictive, or hand-crafted — and
#' renders it as a radial path (and optionally a filled polygon) around the
#' unit circle boundary. At each angle θ the plotted radius is
#' `1 + scale * density(θ) / max(density)`.
#'
#' Because this function only handles rendering, it is agnostic to how the
#' density was produced. To compute from raw headings use
#' [compute_circular_density()] first, or call the convenience wrapper
#' [add_heading_density()] which combines both steps.
#'
#' @param density_df Data frame with columns named by `theta_col` and
#'   `density_col` (and, optionally, `colour_col`). Each row represents one
#'   evaluated angle.
#' @param theta_col Name of the angle column (radians, −π to π). Default
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
    d$.x  <- r * cos(theta)
    d$.y  <- r * sin(theta)
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
      out <- data.frame(
        x = c(d$.r_upper * cos(th),       rev(d$.r_lower * cos(th))),
        y = c(d$.r_upper * sin(th),       rev(d$.r_lower * sin(th)))
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
      out <- data.frame(x = c(d$.x, cos(rev(th))),
                        y = c(d$.y, sin(rev(th))))
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
                                ci_alpha    = 0.3) {
  method  <- match.arg(method)
  dens_df <- compute_circular_density(headings_df, heading_col = heading_col,
                                      colour_col = colour_col, method = method,
                                      n_theta = n_theta, bins = bins, bw = bw,
                                      boot_reps = boot_reps, boot_alpha = boot_alpha)
  add_circular_density(dens_df, colour_col = colour_col,
                       scale = scale, colour = colour,
                       fill = fill, alpha = alpha, linewidth = linewidth,
                       ci_fill = ci_fill, ci_alpha = ci_alpha)
}

# ---- display helpers ---------------------------------------------------------

.to_clock_display <- function(x, y) list(x = -y, y = x)

# ---- circular interval arc ---------------------------------------------------

#' @noRd
.compute_one_interval <- function(angles, stat, boot_reps, boot_alpha) {
  angles <- angles[is.finite(angles)]
  n      <- length(angles)
  circ   <- circular::circular(angles, units = "radians", modulo = "2pi")

  mean_dir <- if (n >= 1L) {
    mu <- as.numeric(circular::mean.circular(circ))
    atan2(sin(mu), cos(mu))
  } else NA_real_

  if (n < 3L) {
    return(data.frame(mean_dir = mean_dir, lower = NA_real_, upper = NA_real_,
                      wraps = FALSE, stringsAsFactors = FALSE))
  }

  if (stat == "sd") {
    sd_val <- as.numeric(circular::sd.circular(circ))
    lower  <- mean_dir - sd_val
    upper  <- mean_dir + sd_val
  } else {
    ci <- tryCatch(
      as.numeric(
        circular::mle.vonmises.bootstrap.ci(circ, alpha = boot_alpha,
                                            bias = TRUE, reps = boot_reps)$mu.ci
      ),
      error = function(e) c(NA_real_, NA_real_)
    )
    lower <- ci[1L]
    upper <- ci[2L]
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
#' @param angle_convention Angle convention of the heading column: `"unit_circle"`
#'   (default, 0 = East CCW) or `"clock"` (0 = North CW). If `NULL`, read from
#'   `attr(headings_df, "angle_convention")`.
#' @param coords Coordinate system used when `angle_convention = "clock"`:
#'   `"relative"` or `"absolute"`. If `NULL`, read from
#'   `attr(headings_df, "coords")`.
#' @param stat Statistic: `"bootstrap_ci"` (default) or `"sd"`.
#' @param boot_reps Integer. Bootstrap replicates for `stat = "bootstrap_ci"`.
#'   Default `1000L`. Ignored when `stat = "sd"`.
#' @param boot_alpha Significance level for the bootstrap CI. Default `0.05`
#'   produces a 95\% interval.
#'
#' @return A data frame with columns `mean_dir`, `lower`, `upper` (radians,
#'   `[-pi, pi]`), and `wraps` (logical, `TRUE` when the arc crosses the +/-pi
#'   discontinuity). `lower` and `upper` are `NA` when `n < 3`.
#'
#' @seealso [add_circ_interval()], [add_heading_interval()]
#' @importFrom circular circular mean.circular sd.circular mle.vonmises.bootstrap.ci
#' @export
compute_circ_interval <- function(headings_df,
                                  heading_col      = "heading",
                                  colour_col       = NULL,
                                  angle_convention = NULL,
                                  coords           = NULL,
                                  stat             = c("bootstrap_ci", "sd"),
                                  boot_reps        = 1000L,
                                  boot_alpha       = 0.05) {
  stat <- match.arg(stat)
  if (!heading_col %in% names(headings_df))
    stop("`heading_col` '", heading_col, "' not found in headings_df.")

  if (is.null(angle_convention)) {
    angle_convention <- attr(headings_df, "angle_convention")
    if (is.null(angle_convention)) angle_convention <- "unit_circle"
  }
  angle_convention <- match.arg(angle_convention, c("clock", "unit_circle"))

  if (is.null(coords)) {
    coords <- attr(headings_df, "coords")
    if (is.null(coords)) coords <- "absolute"
  }
  coords <- match.arg(coords, c("relative", "absolute"))

  use_colour <- !is.null(colour_col) && colour_col %in% names(headings_df)
  groups     <- if (use_colour) split(headings_df, headings_df[[colour_col]]) else list(headings_df)

  out_list <- lapply(seq_along(groups), function(i) {
    angles <- groups[[i]][[heading_col]]
    angles <- if (angle_convention == "clock") .clock_to_uc(angles, coords) else angles
    row    <- .compute_one_interval(angles, stat, boot_reps, boot_alpha)
    if (use_colour) row[[colour_col]] <- names(groups)[[i]]
    row
  })

  out <- do.call(rbind, out_list)
  if (use_colour && is.factor(headings_df[[colour_col]]))
    out[[colour_col]] <- factor(out[[colour_col]],
                                levels = levels(headings_df[[colour_col]]))
  attr(out, "display_convention") <- attr(headings_df, "display_convention")
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
#'   (radians, `[-π, π]`), and optionally `wraps` (logical). Typically the
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
                              n_theta    = 500L) {
  for (col in c("lower", "upper")) {
    if (!col %in% names(interval_df))
      stop("`interval_df` is missing required column '", col, "'.")
  }

  has_group_col <- !is.null(colour_col) && colour_col %in% names(interval_df)
  map_colour    <- has_group_col && is.null(colour)
  has_wraps     <- "wraps" %in% names(interval_df)
  use_clock     <- identical(attr(interval_df, "display_convention"), "clock")

  valid_rows <- which(!is.na(interval_df$lower) & !is.na(interval_df$upper))

  if (!length(valid_rows)) {
    empty <- data.frame(.x = numeric(0), .y = numeric(0), .group_id = integer(0))
    return(ggplot2::geom_path(
      data    = empty,
      mapping = ggplot2::aes(x = .data$.x, y = .data$.y, group = .data$.group_id),
      inherit.aes = FALSE
    ))
  }

  parts <- lapply(valid_rows, function(i) {
    lower <- interval_df$lower[i]
    upper <- interval_df$upper[i]
    wraps <- if (has_wraps) isTRUE(interval_df$wraps[i]) else lower > upper
    theta_seq <- if (wraps) {
      seq(lower, upper + 2 * pi, length.out = n_theta)
    } else {
      seq(lower, upper, length.out = n_theta)
    }
    cos_vals <- radius * cos(theta_seq)
    sin_vals <- radius * sin(theta_seq)
    if (use_clock) {
      disp     <- .to_clock_display(cos_vals, sin_vals)
      cos_vals <- disp$x
      sin_vals <- disp$y
    }
    d <- data.frame(
      .x        = cos_vals,
      .y        = sin_vals,
      .group_id = i
    )
    if (has_group_col) d[[colour_col]] <- interval_df[[colour_col]][i]
    d
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
#'
#' @return A `geom_path()` layer.
#'
#' @seealso [compute_circ_interval()], [add_circ_interval()]
#' @importFrom circular circular mean.circular sd.circular mle.vonmises.bootstrap.ci
#' @importFrom ggplot2 geom_path aes
#' @importFrom rlang .data sym
#' @export
add_heading_interval <- function(headings_df,
                                 heading_col      = "heading",
                                 colour_col       = NULL,
                                 angle_convention = NULL,
                                 coords           = NULL,
                                 stat             = c("bootstrap_ci", "sd"),
                                 boot_reps        = 1000L,
                                 boot_alpha       = 0.05,
                                 radius           = 1.05,
                                 linewidth        = 1.5,
                                 colour           = NULL,
                                 linetype         = "solid",
                                 n_theta          = 500L) {
  stat <- match.arg(stat)
  iv   <- compute_circ_interval(headings_df, heading_col = heading_col,
                                colour_col = colour_col,
                                angle_convention = angle_convention,
                                coords = coords,
                                stat = stat,
                                boot_reps = boot_reps, boot_alpha = boot_alpha)
  add_circ_interval(iv, colour_col = colour_col,
                    radius = radius, linewidth = linewidth,
                    colour = colour, linetype = linetype, n_theta = n_theta)
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
#' @param angle_convention Convention for angles in `heading_col`: `"clock"`
#'   (0 = North, clockwise) or `"unit_circle"` (0 = East, CCW). If `NULL`
#'   (default), read from `attr(headings_df, "angle_convention")`; defaults to
#'   `"unit_circle"` with a message if the attribute is also absent.
#' @param coords Coordinate system: `"relative"` or `"absolute"`. If `NULL`
#'   (default), read from `attr(headings_df, "coords")`; defaults to
#'   `"absolute"` with a message if absent.
#'
#' @return A data frame with columns `mean_dir` (unit-circle radians, 0 to
#'   2π), `resultant_R` (0–1), and `colour_col` when supplied. Both are `NA`
#'   when a group contains fewer than 2 finite angles.
#'
#' @seealso [add_circ_mean()], [add_heading_arrow()]
#' @importFrom circular circular mean.circular rho.circular
#' @export
compute_circ_mean <- function(headings_df,
                              heading_col      = "heading",
                              colour_col       = NULL,
                              angle_convention = NULL,
                              coords           = NULL) {
  if (!heading_col %in% names(headings_df))
    stop("`heading_col` '", heading_col, "' not found in headings_df.")

  if (is.null(angle_convention)) {
    angle_convention <- attr(headings_df, "angle_convention")
    if (is.null(angle_convention)) {
      message("`angle_convention` not found in headings_df attributes; defaulting to 'unit_circle'.")
      angle_convention <- "unit_circle"
    }
  }
  angle_convention <- match.arg(angle_convention, c("clock", "unit_circle"))

  if (is.null(coords)) {
    coords <- attr(headings_df, "coords")
    if (is.null(coords)) {
      message("`coords` not found in headings_df attributes; defaulting to 'absolute'.")
      coords <- "absolute"
    }
  }
  coords <- match.arg(coords, c("relative", "absolute"))

  use_colour <- !is.null(colour_col) && colour_col %in% names(headings_df)
  groups     <- if (use_colour) split(headings_df, headings_df[[colour_col]]) else list(headings_df)

  out_list <- lapply(seq_along(groups), function(i) {
    angles <- groups[[i]][[heading_col]]
    angles <- angles[is.finite(angles)]

    if (length(angles) < 2L) {
      row <- data.frame(mean_dir = NA_real_, resultant_R = NA_real_,
                        stringsAsFactors = FALSE)
    } else {
      uc_angles <- if (angle_convention == "clock") .clock_to_uc(angles, coords) else angles
      circ_obj  <- circular::circular(uc_angles, units = "radians", modulo = "2pi")
      mean_dir  <- .wrap_to_2pi(as.numeric(circular::mean.circular(circ_obj, na.rm = TRUE)))
      R         <- as.numeric(circular::rho.circular(circ_obj, na.rm = TRUE))
      row <- data.frame(mean_dir = mean_dir, resultant_R = R, stringsAsFactors = FALSE)
    }

    if (use_colour) row[[colour_col]] <- names(groups)[[i]]
    row
  })

  out <- do.call(rbind, out_list)
  if (use_colour && is.factor(headings_df[[colour_col]]))
    out[[colour_col]] <- factor(out[[colour_col]], levels = levels(headings_df[[colour_col]]))
  attr(out, "display_convention") <- attr(headings_df, "display_convention")
  out
}

#' Render pre-computed circular mean arrows on a radial plot
#'
#' Takes a data frame produced by [compute_circ_mean()] and renders each row
#' as a `geom_segment()` arrow. `mean_dir` must be in unit-circle convention
#' (0 = East, CCW), as returned by [compute_circ_mean()]. Rows where
#' `mean_dir` or `resultant_R` is `NA` are silently skipped.
#'
#' @param summary_df Data frame with columns `mean_dir` (UC radians, 0 to 2π)
#'   and `resultant_R` (0–1). Typically the output of [compute_circ_mean()].
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

  summary_df$.x <- 0
  summary_df$.y <- 0
  if (identical(attr(summary_df, "display_convention"), "clock")) {
    summary_df$.xend <- -summary_df$resultant_R * sin(summary_df$mean_dir)
    summary_df$.yend <-  summary_df$resultant_R * cos(summary_df$mean_dir)
  } else {
    summary_df$.xend <- summary_df$resultant_R * cos(summary_df$mean_dir)
    summary_df$.yend <- summary_df$resultant_R * sin(summary_df$mean_dir)
  }

  seg_map <- ggplot2::aes(x = .data$.x, y = .data$.y,
                          xend = .data$.xend, yend = .data$.yend)
  if (use_colour) seg_map[["colour"]] <- rlang::sym(colour_col)

  seg_args <- list(
    data        = summary_df,
    mapping     = seg_map,
    linewidth   = linewidth,
    arrow       = grid::arrow(length = grid::unit(arrow_length_cm, "cm")),
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
#'
#' @return A `geom_segment()` layer.
#'
#' @seealso [compute_circ_mean()], [add_circ_mean()]
#' @importFrom circular circular mean.circular rho.circular
#' @importFrom ggplot2 geom_segment aes
#' @importFrom rlang .data sym
#' @importFrom grid arrow unit
#' @export
add_heading_arrow <- function(headings_df,
                              heading_col      = "heading",
                              colour_col       = NULL,
                              angle_convention = NULL,
                              coords           = NULL,
                              linewidth        = 1,
                              colour           = NULL,
                              arrow_length_cm  = 0.2,
                              ...) {
  sm <- compute_circ_mean(headings_df, heading_col = heading_col,
                          colour_col = colour_col,
                          angle_convention = angle_convention, coords = coords)
  add_circ_mean(sm, colour_col = colour_col,
                linewidth = linewidth, colour = colour,
                arrow_length_cm = arrow_length_cm, ...)
}

# ---- heading overlay layers --------------------------------------------------

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
#'   `attr(headings_df, "colour_col")` is used if set — so heading markers
#'   automatically inherit the colour mapping from the associated trajectory
#'   plot when that attribute is present. Ignored when `colour` is supplied.
#' @param colour Fixed colour string. Overrides `colour_col` when supplied;
#'   when `NULL` and no `colour_col` resolves, defaults to `"black"`.
#' @param size Point size passed to `geom_point`.
#' @param alpha Point alpha transparency.
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
                              size = 2, alpha = 1) {
  if (!("heading" %in% names(headings_df))) {
    stop("`headings_df` must contain a `heading` column (radians).")
  }
  if (is.null(colour_col)) colour_col <- attr(headings_df, "colour_col")
  if (identical(attr(headings_df, "display_convention"), "clock")) {
    disp <- .to_clock_display(cos(headings_df$heading), sin(headings_df$heading))
    headings_df[[".x_head"]] <- disp$x
    headings_df[[".y_head"]] <- disp$y
  } else {
    headings_df[[".x_head"]] <- cos(headings_df$heading)
    headings_df[[".y_head"]] <- sin(headings_df$heading)
  }

  mapping <- ggplot2::aes(x = .data[[".x_head"]], y = .data[[".y_head"]])
  use_fixed_colour <- !is.null(colour) || is.null(colour_col) || !colour_col %in% names(headings_df)
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
#' the dotted-line display in the original P. lividus tracking workflow.
#'
#' Requires columns `heading`, `x_inner`, and `y_inner`, which are present
#' when [derive_headings()] is called with `rule = "crossing"` and
#' `return_coords = TRUE`.
#'
#' @param headings_df Data frame with columns `heading` (radians), `x_inner`,
#'   and `y_inner`.
#' @param colour_col Name of a column in `headings_df` to map to the colour
#'   aesthetic. When `NULL` (default), the value of
#'   `attr(headings_df, "colour_col")` is used if set — so vectors
#'   automatically inherit the colour mapping from the associated trajectory
#'   plot when that attribute is present. Ignored when `colour` is supplied.
#' @param colour Fixed colour string. Overrides `colour_col` when supplied;
#'   when `NULL` and no `colour_col` resolves, defaults to `"black"`.
#' @param linetype Line type string or integer passed to `geom_segment`.
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
                               linetype = "dotted") {
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

  headings_df[[".x_head"]] <- cos(headings_df$heading)
  headings_df[[".y_head"]] <- sin(headings_df$heading)

  if (identical(attr(headings_df, "display_convention"), "clock")) {
    disp_end   <- .to_clock_display(headings_df[[".x_head"]], headings_df[[".y_head"]])
    disp_start <- .to_clock_display(headings_df$x_inner,     headings_df$y_inner)
    headings_df[[".x_head"]]  <- disp_end$x
    headings_df[[".y_head"]]  <- disp_end$y
    headings_df[[".x_inner"]] <- disp_start$x
    headings_df[[".y_inner"]] <- disp_start$y
  } else {
    headings_df[[".x_inner"]] <- headings_df$x_inner
    headings_df[[".y_inner"]] <- headings_df$y_inner
  }

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
#' @param step,tol,direction,base_r Passed to \code{\link{stack_headings}}
#'   when \code{stack_r} is absent. See that function for details.
#' @param shade If \code{TRUE}, map \code{stack_n} to the alpha aesthetic
#'   (scaled 0.2–1 across the observed range). Overrides the fixed
#'   \code{alpha} argument.
#' @param shape Passed to \code{\link{stack_headings}} to request
#'   per-observation shape encoding. Shape is also applied when
#'   \code{shape_code} is already a column in \code{data}.
#'   Mapped to ggplot2 shape integers: 1 = hollow, 16 = filled,
#'   21 = filled with ring.
#' @param colour Fixed point colour (ignored when \code{colour_col} is set).
#' @param colour_col Optional column name to map to the colour aesthetic.
#' @param size Point size passed to \code{geom_point()}.
#' @param alpha Fixed alpha. Ignored when \code{shade = TRUE}.
#' @param ... Additional arguments passed to \code{ggplot2::geom_point()}.
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
                                 tol        = NULL,
                                 direction  = "inward",
                                 base_r     = 1,
                                 shade      = FALSE,
                                 shape      = FALSE,
                                 colour     = "black",
                                 colour_col = NULL,
                                 size       = 2,
                                 alpha      = 1,
                                 ...) {
  if (is.null(col))
    col <- if (!is.null(attr(data, "heading_col"))) attr(data, "heading_col")
           else "heading"
  if (!col %in% names(data))
    stop(sprintf("column '%s' not found in data.", col))

  if (!"stack_r" %in% names(data))
    data <- stack_headings(data, col = col, step = step, tol = tol,
                           direction = direction, base_r = base_r,
                           shade = shade, shape = shape)

  if (identical(attr(data, "display_convention"), "clock")) {
    disp <- .to_clock_display(data$stack_r * cos(data[[col]]),
                               data$stack_r * sin(data[[col]]))
    data[[".x_stk"]] <- disp$x
    data[[".y_stk"]] <- disp$y
  } else {
    data[[".x_stk"]] <- data$stack_r * cos(data[[col]])
    data[[".y_stk"]] <- data$stack_r * sin(data[[col]])
  }

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

#' Sparse overlay theme for radial plots.
#'
#' Removes axes, grid lines, and strip decorations to keep attention on the
#' radial geometry. Useful when adding concentric guides and track overlays.
#'
#' @param base_theme Base theme to start from. Defaults to
#'   [ggplot2::theme_classic()].
#'
#' @return A ggplot2 theme object.
#'
#' @importFrom ggplot2 theme theme_classic element_blank element_rect element_text
#' @importFrom grid unit
#' @export
sparse_theme <- function(base_theme = ggplot2::theme_classic()) {
  base_theme +
    ggplot2::theme(
      axis.title        = ggplot2::element_blank(),
      axis.text         = ggplot2::element_blank(),
      axis.ticks        = ggplot2::element_blank(),
      axis.line         = ggplot2::element_blank(),
      panel.spacing     = grid::unit(0.5, "cm"),
      plot.title        = ggplot2::element_text(size = 14, hjust = 0.5),
      strip.background  = ggplot2::element_blank(),
      strip.text        = ggplot2::element_blank(),
      panel.background  = ggplot2::element_rect(fill = "transparent"),
      plot.background   = ggplot2::element_rect(fill = "transparent", color = NA),
      panel.grid.major  = ggplot2::element_blank(),
      panel.grid.minor  = ggplot2::element_blank(),
      legend.background = ggplot2::element_rect(fill = "transparent", color = NA),
      legend.box.background = ggplot2::element_rect(fill = "transparent", color = NA)
    )
}

#' Minimalist theme for radial track plots.
#'
#' Wrapper around [ggplot2::theme_grey()] that strips panel adornments to
#' emphasise the radial geometry used by the package.
#'
#' @param ... Additional arguments passed to [ggplot2::theme_grey()].
#'
#' @return A ggplot2 theme object.
#'
#' @importFrom ggplot2 theme element_blank element_rect %+replace%
#' @export
spartan_theme <- function(...) {
  ggplot2::theme_grey(...) %+replace%
    ggplot2::theme(
      strip.background = ggplot2::element_blank(),
      strip.text       = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "transparent"),
      plot.background  = ggplot2::element_rect(fill = "transparent", color = NA),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
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

    d   <- x@data
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
#' @inheritParams ggplot2::geom_path
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

# ---- high-level plotting -----------------------------------------------------

#' Make ggplot object of tracks radiating from circle centre.
#'
#' Accepts either a precomputed data frame of polar/cartesian coordinates or a
#' `TrajSet`. When a `TrajSet` is supplied, column mappings are inferred from
#' the object and handed off to the plotting helpers.
#'
#' @inheritParams draw_tracks
#' @inheritParams directedness_arrow
#' @inheritParams add_ticks
#' @inheritParams sparse_theme
#' @param data Data frame or `TrajSet`.
#' @param geom Geom specification passed to [draw_tracks()].
#' @param group_col Optional column for grouping aesthetics.
#' @param colour_col Optional column for colour aesthetics. Mutually exclusive
#'   with `colour_cycle`.
#' @param colour_cycle Optional cycling colour specification. Either a positive
#'   integer `n` (trajectories are assigned colours 1–n, cycling back to 1 after
#'   every `n` trajectories) or a character vector of colour values (e.g.
#'   `c("red","blue","green")`). When `panel_by` is set the cycle restarts
#'   independently within each panel. Mutually exclusive with `colour_col`.
#' @param style Either `"classic"` (default) or `"minimal"`.
#' @param show_labels Whether to place labels at the perimeter.
#' @param label_col Column containing label values.
#' @param label_padding Multiplier applied to the unit circle when placing labels.
#' @param label_use_repel Use `ggrepel::geom_text_repel()` when available.
#' @param show_arrow Whether to draw a mean resultant arrow from the centre.
#' @param arrow_angle_col Column containing angles (radians) to summarise for the arrow.
#' @param arrow_colour Arrow colour.
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
#'   circle at y = −1.25).
#' @param strip_label_size Font size for strip labels. Applies to both strip
#'   text and the in-panel `"inside"` annotation.
#' @param ticks,degrees,legend,title,xlab,ylab,axes Additional styling options.
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
  style = c("classic", "minimal"),
  show_labels = NULL,
  label_col = NULL,
  label_size = 3,
  label_padding = 1.08,
  label_use_repel = TRUE,
  show_arrow = NULL,
  arrow_angle_col = NULL,
  arrow_colour = "black",
  arrow_size = 2,
  ...){
  if (is.null(ticks)) {ticks = TRUE}
  if (is.null(degrees)) {degrees = TRUE}
  if (is.null(legend)) {legend = FALSE}
  if (is.null(axes)) {axes = FALSE}
  style <- match.arg(style)
  if (is.null(show_labels)) {
    show_labels <- identical(style, "classic")
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

  data <- ts@data
  col_from_meta <- FALSE
  if (!is.null(ts@meta$plot_x_col) && identical(x_col, "rel_x")) {
    x_col <- ts@meta$plot_x_col
    col_from_meta <- TRUE
    if (!is.null(ts@meta$plot_y_col) && identical(y_col, "rel_y"))
      y_col <- ts@meta$plot_y_col
  } else if (!is.null(ts@cols$x) && identical(x_col, "rel_x")) {
    x_col <- ts@cols$x
    if (!is.null(ts@cols$y) && identical(y_col, "rel_y")) y_col <- ts@cols$y
  }
  if (col_from_meta && identical(ts@meta$display_convention, "clock") &&
      all(c(x_col, y_col) %in% names(data))) {
    disp <- .to_clock_display(data[[x_col]], data[[y_col]])
    data[[".disp_x"]] <- disp$x
    data[[".disp_y"]] <- disp$y
    x_col <- ".disp_x"
    y_col <- ".disp_y"
  }
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

  g <- g + spartan_theme()

  layer_mapping <- NULL
  if (!is.null(group_col) || !is.null(colour_col)) {
    mapping_list <- list()
    if (!is.null(group_col)) mapping_list$group <- rlang::sym(group_col)
    if (!is.null(colour_col)) mapping_list$colour <- rlang::sym(colour_col)
    layer_mapping <- do.call(ggplot2::aes, mapping_list)
  }

  g <- g + draw_tracks(
    data = data,
    x_col = x_col,
    y_col = y_col,
    geom = geom,
    mapping = layer_mapping,
    ...
  )

  if (style == "minimal") {
    g <- g + spartan_theme()
    g <- g + add_quadrant_lines()
    g <- g + add_circ(circle_color = "grey60", circle_size = 1)
    if (degrees) g <- g + degree_labs()
    if (ticks) g <- g + add_ticks()
  } else {
    g <- g + sparse_theme()
    g <- g + add_quadrant_lines()
    g <- g + add_circ(circle_color = "black", circle_size = 1.5)
    g <- g + add_ticks()
    if (degrees) g <- g + degree_labs()
  }

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
    show_arrow <- identical(style, "classic")
  }
  if (show_arrow) {
    angle_col <- resolve_angle_column(data)
    if (!is.null(arrow_angle_col)) angle_col <- arrow_angle_col
    if (!is.null(angle_col) &&
        angle_col %in% names(data) &&
        is.numeric(data[[angle_col]]) &&
        any(!is.na(data[[angle_col]]))) {
      # Reduce to one circular mean per trial before computing the grand mean
      # direction and rho.  Averaging all individual timepoint angles would
      # weight dense within-trial sampling rather than between-trial spread.
      arrow_angles <- if (!is.null(group_col) && group_col %in% names(data)) {
        vapply(split(data[[angle_col]], data[[group_col]]), function(a) {
          a <- a[!is.na(a)]
          if (length(a) == 0L) return(NA_real_)
          atan2(mean(sin(a)), mean(cos(a)))
        }, numeric(1L))
      } else {
        data[[angle_col]]
      }
      arrow_angles <- arrow_angles[!is.na(arrow_angles)]
      g <- g + directedness_arrow(
        data = data.frame(theta = arrow_angles),
        angle_col = theta,
        colour = arrow_colour,
        size = arrow_size
      )
    } else if (!is.null(arrow_angle_col)) {
      warning("Arrow requested but column `", arrow_angle_col,
              "` is unavailable or not numeric; skipping arrow layer.", call. = FALSE)
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
  title     = NULL,
  style     = c("classic", "minimal"),
  ...) {

  style <- match.arg(style)

  if (style == "minimal") {
    g <- ggplot2::ggplot() + ggplot2::coord_fixed() + spartan_theme()
  } else {
    g <- ggplot2::ggplot() + ggplot2::coord_fixed() + sparse_theme()
  }

  g <- g +
    add_quadrant_lines() +
    add_circ(circle_color = if (style == "minimal") "grey60" else "black",
             circle_size  = if (style == "minimal") 1        else 1.5)

  if (ticks)   g <- g + add_ticks()
  if (degrees) g <- g + degree_labs()

  g <- g + add_stacked_headings(
    data, col = col, step = step, tol = tol, direction = direction,
    base_r = base_r, shade = shade, shape = shape, ...
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
