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
# Every method marks its derived heading as a boundary point; crossing
# additionally draws its two detection rings and the dashed inner->boundary
# heading vectors arising from the ring crossings. Tracks-only for "none".
# Display defaults to the identity circ_display() so the heading points align
# with the unit circle. (Trajectory-derived heading vectors for non-crossing
# methods are deferred bespoke work; no vector is drawn from the origin.)
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

  if (identical(method, "crossing")) {
    p <- p +
      add_multiple_circles(radii = c(circ0, circ1),
                           circle_color = "grey60", circle_size = 0.4) +
      add_heading_vectors(hd)
  }

  p + add_heading_points(hd, size = 3) +
    ggplot2::labs(subtitle = lab)
}
