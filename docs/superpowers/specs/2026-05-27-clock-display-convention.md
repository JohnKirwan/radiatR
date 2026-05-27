# Clock Display Convention Design

**Date:** 2026-05-27

## Goal

Make `radiate()` plots display stimulus direction at the top (12 o'clock / North) rather than to the right (East / 3 o'clock). All heading overlays — points, vectors, circular mean arrow, interval arc — must share the same rotated coordinate space so trajectories and overlays remain aligned.

## Background

### Why this matters

`radiatR` currently renders circular arenas in unit-circle convention (UC): 0° points East, angles increase counter-clockwise. The `circular` package uses clock convention for geographic/biological data: 0° points North, angles increase clockwise (stimulus at top). Biological intuition matches the clock: "the animal moved toward the stimulus" should appear as "upward" in the plot.

Additionally, `radiate(plividus)` has a pre-existing coordinate mismatch: it plots `trans_x/trans_y` (absolute arena coordinates) while heading overlays are computed in `rel_x/rel_y` space (stimulus-at-right). For datasets where the stimulus position varies per trial (e.g., `plividus`), the two coordinate spaces diverge and overlays are misaligned. This design fixes both issues together.

### The rotation

To map UC coordinates to clock display (stimulus East → stimulus North), apply a 90° counter-clockwise rotation:

```
(x_display, y_display) = (-y_uc, x_uc)
```

For a heading at UC angle θ, the displayed arrow endpoint at radius R is:
```
(x_display, y_display) = (-R·sin θ, R·cos θ)
```

This matches the `circular` package's `plot.circular(zero=pi/2, rotation="clock")` formula for clock-convention input applied to UC data.

## Architecture

### Convention propagation

Display convention travels through two channels, chosen because the data types are independent:

1. **TrajSet metadata** — `ts@meta$display_convention = "clock"`, `ts@meta$plot_x_col = "rel_x"`, `ts@meta$plot_y_col = "rel_y"`. The three keys together tell `radiate()` which columns to use for trajectory rendering and to apply the rotation. Absent = no change from current behaviour.

2. **Heading data frame attribute** — `attr(hd, "display_convention") = "clock"`. Set by `derive_headings()` when `coords = "relative"`. Propagated by `compute_circ_mean()` to its output summary data frame. Heading overlay functions read this attribute and apply the rotation when present.

### Core helper (internal)

```r
.to_clock_display <- function(x, y) list(x = -y, y = x)
```

Applied uniformly by all rendering functions when display convention is "clock".

### No breaking changes

All convention keys are opt-in. Functions that receive data without the keys behave exactly as today. Existing code that creates TrajSets or heading data frames without setting these keys is unaffected.

## Functions Changed

### `R/circular_trials.R`

**`get_tracked_object_pos()`** — after building the TrajSet, set:
```r
ts@meta$display_convention <- "clock"
ts@meta$plot_x_col         <- "rel_x"
ts@meta$plot_y_col         <- "rel_y"
```

This is the only place a standard TrajSet is constructed from circular-arena data. `@cols$x` / `@cols$y` are left unchanged (they may be used elsewhere).

### `R/headings.R`

**`derive_headings()`** — at the end of the function, when `coords = "relative"`:
```r
attr(hd, "display_convention") <- "clock"
```

### `R/circular_plotting.R`

**`radiate()`** — reads `ts@meta$plot_x_col` (default `ts@cols$x`) and `ts@meta$plot_y_col` (default `ts@cols$y`) for trajectory coordinates. When `ts@meta$display_convention == "clock"`, applies `.to_clock_display()` to the trajectory points before rendering.

**`add_heading_points()`** — reads `attr(headings_df, "display_convention")`. When "clock", transforms `x_inner`/`y_inner` via `.to_clock_display()`.

**`add_heading_vectors()`** — same attribute check. Transforms both start and end of each segment.

**`compute_circ_mean()`** — propagates the attribute: `attr(result, "display_convention") <- attr(headings_df, "display_convention")`.

**`add_circ_mean()`** — reads convention from its `summary_df` argument. When "clock", computes arrow endpoint as `(-R·sin θ, R·cos θ)` instead of `(R·cos θ, R·sin θ)`.

**`add_circ_interval()`** — reads convention from its `summary_df` argument. Applies `.to_clock_display()` to the arc polygon vertices.

## Testing

### Unit tests (no TrajSet required)

1. `.to_clock_display()` — verify the formula: `(1, 0) → (0, 1)`, `(0, 1) → (-1, 0)`, `(-1, 0) → (0, -1)`, `(0, -1) → (1, 0)`.
2. `derive_headings()` attribute propagation — confirm `attr(hd, "display_convention") == "clock"` when `coords = "relative"`, absent otherwise.
3. `compute_circ_mean()` attribute propagation — confirm attribute flows from input `headings_df` to output summary.
4. `add_heading_points()` rotation — build a minimal headings_df with `display_convention = "clock"`, confirm rendered x/y in the geom data are rotated.
5. `add_heading_vectors()` rotation — same approach.
6. `add_circ_mean()` rotation — feed a summary_df with `display_convention = "clock"` and a known `mean_dir`, confirm segment coordinates match `(-R·sin θ, R·cos θ)`.

### Integration test

7. `get_tracked_object_pos()` meta keys — construct or load a minimal TrajSet, check that `meta$display_convention`, `meta$plot_x_col`, `meta$plot_y_col` are set correctly.

### Regression

8. All existing tests must continue to pass. Because convention keys are opt-in and the existing fixture data (`urchin_tracks.rda`) was not built via `get_tracked_object_pos()`, no existing test should need modification.

## Visible Effect

`radiate(plividus)` before: trajectories drawn in absolute arena coordinates, stimulus appears at East (right).

`radiate(plividus)` after: trajectories drawn in relative coordinates, stimulus appears at North (top, 12 o'clock). Heading overlays are in the same rotated space and align correctly with the trajectories.

Existing `radiate(urchin_data)` calls are unchanged if `urchin_data` was not created via `get_tracked_object_pos()`.
