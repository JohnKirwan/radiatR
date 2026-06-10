# radiatR Shiny app — Results-plot overlay helpers (shiny-free).
# Sourced by app.R; also unit-tested directly. Depends only on the radiatR
# package (assumed attached) and ggplot2.

# derive_headings() standardises the trajectory identifier to this column, so a
# headings frame keys its rows on it regardless of the TrajSet's own id column
# name (which may be "trial_id", "animal", ...). The trajectory is the default
# grouping for colour; any chosen grouping column overrides it.
HEADING_TRAJ_COL <- "id"

# Global per-trajectory cycle index (1..n) for trajectory ids `vals`, numbered in
# `ordered_ids` order and wrapped at n. Used as the single colour key for BOTH
# tracks and heading markers, so colour always distinguishes the trajectory --
# never the facet/condition variable -- and a marker matches its own track
# regardless of faceting (a global key, unlike radiate's per-panel cycle).
cycle_colour_factor <- function(vals, ordered_ids, n) {
  idx <- ((match(vals, ordered_ids) - 1L) %% n) + 1L
  factor(idx, levels = seq_len(n))
}

# Attach the trajectory colour key to a TrajSet's track data as ".cycle_colour",
# so radiate(colour_col = ".cycle_colour") colours each trajectory consistently.
add_track_cycle_colour <- function(ts, id_col, n) {
  ids <- ts@data[[id_col]]
  ts@data[[".cycle_colour"]] <- cycle_colour_factor(ids, unique(ids), n)
  ts
}

# Attach the SAME trajectory colour key to a headings frame, so markers share the
# tracks' colour scale.
#   hd          : a headings frame.
#   ordered_ids : unique trajectory ids in track-data order (must match the order
#                 add_track_cycle_colour() used for the tracks).
#   n           : number of cycled colours.
#   traj_col    : the trajectory-id column in hd (default HEADING_TRAJ_COL).
# Returns hd with a ".cycle_colour" factor column (levels seq_len(n)).
attach_cycle_colour <- function(hd, ordered_ids, n,
                                traj_col = HEADING_TRAJ_COL) {
  hd[[".cycle_colour"]] <- cycle_colour_factor(hd[[traj_col]], ordered_ids, n)
  hd
}

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
