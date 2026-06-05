# radiatR Shiny app — Configure-step method preview (shiny-free helpers).
# Sourced by app.R; also unit-tested directly. Depends only on the radiatR
# package (assumed attached) and ggplot2.

# Session cache for the demo TrajSet so it is built at most once.
.preview_env <- new.env(parent = emptyenv())

# method_labels is normally provided by app.R; define a fallback so preview.R is
# self-contained when sourced on its own (e.g. in tests).
if (!exists("method_labels", inherits = TRUE)) {
  method_labels <- c(
    none = "None (no headings)", distal = "Direction at furthest point",
    net = "Net displacement direction", crossing = "Exit direction (ring crossing)",
    straight = "Longest straight segment",
    window_net = "Smoothed (windowed) net direction",
    origin_mean = "Mean direction from centre",
    velocity_mean = "Mean velocity direction",
    maxspeed_window = "Direction at peak speed",
    vm_fit = "Von Mises fit of step directions",
    pca_axis = "Principal axis (PCA)",
    ransac_straight = "Robust straight-line fit (RANSAC)",
    goal_bias = "Goal-biased direction"
  )
}

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
# Ring crossing instead draws its two detection rings, a dot on each interpolated
# ring crossing (inner c0, outer c1), and the dashed chord between them -- the
# actual construction, stopping at the outer ring rather than extrapolating to
# the rim. Tracks-only for "none". The preview always uses the identity
# circ_display(), so unit-circle coordinates map straight to the panel.
# (Trajectory-derived heading vectors for non-crossing methods are deferred
# bespoke work; no vector is drawn from the origin.)
build_method_preview <- function(ts, method, circ0 = 0.3, circ1 = 0.6,
                                 display = circ_display()) {
  idc <- ts@cols$id
  n   <- length(unique(ts@data[[idc]]))

  base <- radiate(ts, group_col = idc, show_arrow = FALSE, show_labels = FALSE,
                  show_tracks = TRUE, theme = "minimal", colour_cycle = n,
                  display = display)

  lab <- method_labels[[method]]
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

  p <- base

  # Crossing is special: draw the two detection rings and the heading vector as
  # the dotted chord between the two interpolated ring crossings (inner c0 ->
  # outer c1), with a dot on each. c1 is the point on the outer ring along the
  # heading ray from c0 (heading is defined as the c0->c1 direction, so this
  # places it exactly on circ1). No rim point is drawn: the vector stops at the
  # outer ring rather than extrapolating to the boundary.
  if (identical(method, "crossing")) {
    u_x <- cos(hd$heading); u_y <- sin(hd$heading)
    x0  <- hd$x_inner;      y0  <- hd$y_inner
    b    <- x0 * u_x + y0 * u_y
    disc <- pmax(b^2 - (x0^2 + y0^2 - circ1^2), 0)
    tt   <- -b + sqrt(disc)                 # forward ray-circle intersection
    seg  <- data.frame(x_in = x0, y_in = y0,
                       x_out = x0 + tt * u_x, y_out = y0 + tt * u_y)
    pts  <- data.frame(px = c(seg$x_in, seg$x_out),
                       py = c(seg$y_in, seg$y_out))

    return(
      p +
        add_multiple_circles(radii = c(circ0, circ1),
                             circle_color = "grey60", circle_size = 0.4) +
        ggplot2::geom_segment(
          data    = seg,
          mapping = ggplot2::aes(x = .data$x_in,   y = .data$y_in,
                                 xend = .data$x_out, yend = .data$y_out),
          linetype = "dotted", inherit.aes = FALSE
        ) +
        ggplot2::geom_point(
          data    = pts,
          mapping = ggplot2::aes(x = .data$px, y = .data$py),
          size = 2, inherit.aes = FALSE
        ) +
        ggplot2::labs(subtitle = lab)
    )
  }

  p + add_heading_points(hd, size = 3) +
    ggplot2::labs(subtitle = lab)
}
