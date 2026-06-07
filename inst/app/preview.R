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
# Every method marks its derived heading with a boundary point. Methods with a
# registered construction (see preview_constructions.R) additionally draw how the
# heading is derived -- e.g. ring crossing draws its two detection rings, the two
# ring-crossing dots, and the dashed vector to the rim. Tracks-only for "none".
# Construction overlays and heading markers take each trajectory's colour. The
# preview always uses the identity circ_display(), so unit-circle coordinates map
# straight to the panel.
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
      derive_headings(ts, rule = method, coords = coords, return_coords = TRUE)
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

  # Construction overlay for the method (NULL when none applies), then the heading
  # point, which every method shares. See inst/app/preview_constructions.R.
  extra <- preview_construction_layers(method, ts, hd, circ0, circ1)
  p <- Reduce(`+`, extra, init = base)

  p + add_heading_points(hd, size = 3) +
    ggplot2::labs(subtitle = lab)
}
