# radiatR Shiny app — Configure-step method preview (shiny-free helpers).
# Sourced by app.R; also unit-tested directly. Depends only on the radiatR
# package (assumed attached) and ggplot2.

# Session cache for the demo TrajSet so it is built at most once.
.preview_env <- new.env(parent = emptyenv())

# A small, fixed subset of the bundled cpunctatus example: a handful of trials
# spanning the straightness range, so the preview shows low-, mid-, and
# high-sinuosity real tracks. Cached after first call.
demo_tracks <- function() {
  if (!is.null(.preview_env$demo)) return(.preview_env$demo)

  e <- new.env()
  utils::data("cpunctatus", package = "radiatR", envir = e)
  ts  <- e$cpunctatus
  idc <- ts@cols$id

  st <- straightness_index(ts)
  st <- st[is.finite(st$straightness), , drop = FALSE]
  st <- st[order(st$straightness), , drop = FALSE]
  picks <- unique(round(stats::quantile(
    seq_len(nrow(st)), probs = c(.05, .35, .65, .95), names = FALSE)))
  ids <- st[[idc]][picks]

  demo <- ts
  demo@data <- ts@data[ts@data[[idc]] %in% ids, , drop = FALSE]
  .preview_env$demo <- demo
  demo
}

# Build the illustrative preview plot for a heading method on the demo tracks.
# Non-crossing methods mark their derived heading as a single boundary point.
# Ring crossing additionally draws its two detection rings and a dot on each
# interpolated ring crossing (inner c0, outer c1), with the dashed heading vector
# running through both crossings out to the unit-circle periphery. Heading
# markers and the crossing vector take each trajectory's colour. Tracks-only for
# "none". The preview always uses the identity circ_display(), so unit-circle
# coordinates map straight to the panel. (Trajectory-derived heading vectors for
# non-crossing methods are deferred bespoke work; no vector is drawn from the
# origin.)
build_method_preview <- function(ts, method, circ0 = 0.3, circ1 = 0.6,
                                 display = circ_display()) {
  idc <- ts@cols$id
  n   <- length(unique(ts@data[[idc]]))

  base <- radiate(ts, group_col = idc, show_arrow = FALSE, show_labels = FALSE,
                  show_tracks = TRUE, theme = "minimal", colour_cycle = n,
                  display = display)

  # method_labels is provided by app.R; fall back to the raw method name when this
  # helper is sourced standalone (e.g. in tests).
  lab <- if (exists("method_labels", inherits = TRUE)) method_labels[[method]] else NULL
  if (is.null(lab)) lab <- method

  if (identical(method, "none")) {
    return(base + ggplot2::labs(subtitle = lab,
                                caption = "No headings - tracks only"))
  }

  has_rel <- !is.null(ts@cols$rel_x) && !is.null(ts@cols$rel_y)
  coords  <- if (has_rel) "relative" else "absolute"

  hd <- tryCatch({
    if (identical(method, "crossing")) {
      derive_headings(ts, rule = "crossing", coords = coords,
                      circ0 = circ0, circ1 = circ1, return_coords = TRUE)
    } else {
      derive_headings(ts, rule = method, coords = coords)
    }
  }, error = function(e) NULL)

  if (is.null(hd) || all(!is.finite(hd$heading))) {
    return(base + ggplot2::labs(subtitle = lab))
  }

  # Keep only finite headings, then set the display attribute (subsetting a
  # data.frame drops attributes), so the points and vectors all draw the same
  # rows rather than relying on ggplot2 silently dropping NA coordinates.
  hd <- hd[is.finite(hd$heading), , drop = FALSE]
  attr(hd, "display") <- display

  # Colour the heading markers (and the crossing vector) to match each track.
  # radiate(colour_cycle = n) colours tracks by a factor of per-id cycle indices
  # (levels 1..n) under ggplot2's default hue scale; reproduce that index per
  # heading row so the overlay shares the same colour scale as the trajectories.
  ids_order <- unique(ts@data[[idc]])
  hd$.cycle_colour <- factor(match(hd$id, ids_order), levels = seq_len(n))
  attr(hd, "colour_col") <- ".cycle_colour"

  p <- base

  # Crossing is special: draw the two detection rings, a dot on each interpolated
  # ring crossing (inner c0, outer c1), and the dashed heading vector running
  # through both crossings out to the unit-circle periphery. c1 and the rim point
  # both lie on the heading ray from c0 (heading is defined as the c0->c1
  # direction), so we place them exactly on circ1 and on radius 1.
  if (identical(method, "crossing")) {
    u_x <- cos(hd$heading); u_y <- sin(hd$heading)
    x0  <- hd$x_inner;      y0  <- hd$y_inner
    b   <- x0 * u_x + y0 * u_y
    r2  <- x0^2 + y0^2
    t_out <- -b + sqrt(pmax(b^2 - (r2 - circ1^2), 0))  # outer ring crossing c1
    t_rim <- -b + sqrt(pmax(b^2 - (r2 - 1),       0))  # unit-circle periphery
    seg  <- data.frame(x_in = x0, y_in = y0,
                       x_out = x0 + t_rim * u_x, y_out = y0 + t_rim * u_y,
                       .cycle_colour = hd$.cycle_colour)
    dots <- data.frame(px = c(x0, x0 + t_out * u_x),
                       py = c(y0, y0 + t_out * u_y),
                       .cycle_colour = rep(hd$.cycle_colour, 2L))

    return(
      p +
        add_multiple_circles(radii = c(circ0, circ1),
                             circle_color = "grey60", circle_size = 0.4) +
        ggplot2::geom_segment(
          data    = seg,
          mapping = ggplot2::aes(x = .data$x_in,   y = .data$y_in,
                                 xend = .data$x_out, yend = .data$y_out,
                                 colour = .data[[".cycle_colour"]]),
          linetype = "dotted", inherit.aes = FALSE
        ) +
        ggplot2::geom_point(
          data    = dots,
          mapping = ggplot2::aes(x = .data$px, y = .data$py,
                                 colour = .data[[".cycle_colour"]]),
          size = 2, inherit.aes = FALSE
        ) +
        add_heading_points(hd, size = 3) +
        ggplot2::labs(subtitle = lab)
    )
  }

  p + add_heading_points(hd, size = 3) +
    ggplot2::labs(subtitle = lab)
}
