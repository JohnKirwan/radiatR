# radiatR Shiny app — Results-plot overlay helpers (shiny-free).
# Sourced by app.R; also unit-tested directly. Depends only on the radiatR
# package (assumed attached) and ggplot2.

# Bin width for the stacked-dots display: 5 degrees, centred on the reference
# direction (phase 0) so the modal heading sits on a column rather than split
# across a bin boundary. Snapping headings to these centres first turns
# stack_headings(tol = NULL) into clean inward radial columns (continuous
# headings have no exact ties, so without binning nothing would stack).
STACK_BIN_WIDTH <- pi / 36   # 5 degrees in radians

# Stacked-dots spacing (data units; the unit circle has radius 1). Deliberately
# a little different from the circular package's defaults: STACK_START_SEP shifts
# the outermost dot just inside the rim so the dots abut the periphery line
# rather than straddle it, and STACK_STEP is wider than circular's 0.025 so
# individual points in a column read clearly.
STACK_START_SEP <- 0.05
STACK_STEP      <- 0.06

# Build the per-trial heading-marker layer for the Results plot.
#   hd      : headings frame with a "heading" column (radians).
#   style   : "points" (overlapping hollow circles), "stacked" (stacked inward),
#             or "none". Anything else falls back to "points".
#   gc      : condition/grouping column name in hd, or NULL.
#   display : a circ_display(); markers are rotated to match the plot.
#   colour_col : column in hd to map to dot colour, or NULL for plain black. The
#             app passes the same colour key the tracks use, so each heading
#             marker inherits its trajectory's colour (shared ggplot scale).
# Returns a ggplot2 layer, or NULL for "none". For "stacked" with a grouping
# column, headings are stacked WITHIN each group (stack_headings has no group
# argument), so the per-facet radial stacking is correct; the grouping column is
# carried through so the layer facets alongside the tracks.
heading_marker_layer <- function(hd, style = "points", gc = NULL,
                                 display = circ_display(), colour_col = NULL) {
  if (identical(style, "none")) return(NULL)

  attr(hd, "display") <- display

  if (identical(style, "stacked")) {
    # Bin the continuous headings to 5-degree centres so coincident-binned
    # angles become exact ties that stack_headings groups into radial columns.
    hd$heading <- bin_angles(hd$heading, width = STACK_BIN_WIDTH)
    attr(hd, "display") <- display
    if (!is.null(gc) && gc %in% names(hd)) {
      hd <- do.call(rbind, lapply(
        split(hd, hd[[gc]]),
        function(g) stack_headings(g, col = "heading",
                                   step = STACK_STEP, start_sep = STACK_START_SEP)
      ))
      rownames(hd) <- NULL
      attr(hd, "display") <- display
    }
    return(add_stacked_headings(hd, col = "heading", size = 2.5, alpha = 0.8,
                                step = STACK_STEP, start_sep = STACK_START_SEP,
                                colour_col = colour_col))
  }

  # "points" (and any unrecognised style)
  add_heading_points(hd, size = 2.5, alpha = 0.8, colour_col = colour_col)
}
