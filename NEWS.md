# radiatR (development version)

## Track metrics

* New `path_straightness(x, y)` and `straightness_index(ts)` compute the path
  straightness index (net displacement / path length, 0–1) per trajectory.
* New `path_tortuosity(x, y)` and `tortuosity_ratio(ts)` compute the classic
  tortuosity ratio (path length / net displacement, ≥ 1), the reciprocal of the
  straightness index. Not yet surfaced in the Shiny app.

## Plotting

* `radiate()` gains an `angle_labels` argument: `"degrees"` (default, e.g.
  `45°`), `"none"`, or `"radians"` (now rendered as π fractions, e.g. `π/4`,
  rather than decimals). `degree_labs()` gains a matching `units` argument.
  `degrees = FALSE` remains as a back-compatible alias for `angle_labels =
  "none"`.
* **Breaking:** the quadrant lines (two dashed lines through the origin) are no
  longer drawn by default. `radiate()` gains a `quadrants` argument (default
  `FALSE`) to opt back in.
* `radiate()` gains a `rings` argument (default `FALSE`) drawing concentric
  guide rings (the radial analogue of a grid). Both the quadrant lines and the
  guide rings take their colour and width from the chosen theme's grid lines,
  so they match the theme (and fall back to a subtle grey for themes without a
  grid, e.g. `void` and `classic`).
* **Breaking:** the `radiate()` `style` argument (`"classic"`/`"minimal"`) is
  replaced by `theme`, named for the ggplot2 base themes: `"void"` (default),
  `"minimal"`, `"classic"`, `"bw"`, `"grey"`, `"light"`, `"dark"`, and
  `"linedraw"`. Each gives the matching `ggplot2::theme_*()` appearance (panel
  background, grid, border). The exported `sparse_theme()` and `spartan_theme()`
  are removed in favour of the new `radial_theme(name)`.
* Overlay elements (unit circle, ticks, degree labels) now adapt to the theme:
  they use light "ink" on the dark theme so they stay legible. `add_ticks()` and
  `degree_labs()` gain a `colour` argument.
* `radiate()` gains a `show_tracks` argument (default `TRUE`) to draw the arena
  and overlays without the trajectory paths, symmetric with `show_arrow` and
  `show_labels`.

## Shiny app

* The Results summary table gains a **Straightness** column: the mean path
  straightness index across each group's trials.
* The Results step's Display panel gains a **Theme** dropdown (Void, Minimal,
  Classic, Black & white, Grey, Light, Dark, Line draw) that restyles the
  on-screen plot and the download.
* The Results Display panel gains an **Angle labels** dropdown (Degrees / None /
  Radians).
* The Results Display panel gains **Quadrant lines** and **Guide rings** toggles
  (both off by default); both follow the selected theme's grid styling.
* The Results step now has on/off toggles for the trajectories, heading points,
  directedness arrow, and a mean-direction confidence-interval arc (95%
  bootstrap CI, off by default). The toggles drive both the on-screen plot and
  the plot download.
* The toggles moved to a "Display" card in the right-hand column, giving the
  plot panel the full width of its column.
* Plot downloads can now be exported as **PDF** or **SVG** (editable vector
  formats) as well as PNG, with editable width/height (inches) and, for PNG, a
  resolution control. The on-screen preview tracks the chosen aspect ratio.
* A "Preview size" slider scales the on-screen plot canvas without affecting the
  exported file dimensions.

## Bug fixes

* The `radiate()` mean-direction (directedness) arrow now respects the clock
  display convention. In clock-display plots the trajectories are rotated 90
  degrees (East to North) but the arrow was left in unit-circle coordinates, so
  it pointed ~90 degrees away from the tracks and heading markers it summarises.
  The arrow is now rotated with the rest of the plot.
* `as.data.frame()` on a `TrajSet` now works for users of the installed
  package. It had been defined as an S4 method on the base S3 generic, which is
  only reachable from within the package's own namespace, so user code got
  "cannot coerce class TrajSet to a data.frame". It is now a registered S3
  method.

## Shiny app

* The upload step now offers a one-click **"Load the example millipede
  dataset"** so the app can be tried end to end without supplying a tracking
  file. It loads the bundled `cpunctatus` example and jumps straight to the
  configure step.
* Fixed the results step crashing or producing an empty summary when the
  tracking data's trial-ID column was not literally named `id`. The condition
  join and per-trial summary now key off the headings frame's own `id` column.
* Fixed the results plot showing "Plot unavailable" whenever a condition column
  was detected: it set `radiate()`'s mutually exclusive `colour_col` and
  `colour_cycle` at once. Colours now cycle only when no condition drives the
  colour scale, and plot-render failures are logged rather than swallowed.

## Camera calibration

* Refocused the calibration layer on *importing* calibrations from established
  tools rather than estimating them. `read_calibration()` reads camera
  intrinsics and Brown-Conrady distortion coefficients from the **MATLAB
  Computer Vision Toolbox** (`.mat`), **OpenCV** `FileStorage` (YAML/JSON), or a
  plain **CSV**, and `cal_model()` builds a `CalModel` from coefficients you
  already hold. Both handle radiatR's transposed intrinsic-matrix convention and
  the 1-based (MATLAB) vs 0-based (OpenCV) principal-point difference for you.
* Removed the in-package calibration *estimator* and its interactive
  point-capture tools (`calibration_session()`, `calibration_from_points()`,
  `checkerboard_points()`, the `calibration_points_*` / `*_calibration_points`
  helpers, and `calibration_switch_axes()`). Estimating intrinsics from
  checkerboard images is better served by the mature toolboxes above; the
  bundled `calibration_corners.csv` / `calibration_truth.csv` fixtures are gone
  with it. `cam_cal_pt()`, `cam_cal_many()`, and `calibrate_positions()` are
  unchanged.

# radiatR 0.2.0

A large feature release that broadens *radiatR* from a focused plotting toolkit
into an end-to-end pipeline for circular-arena tracking: many more data
sources, a full circular-statistics layer, richer distribution overlays, and a
no-code graphical interface.

## Data loading

* Dialect-based loader system covering 20+ tracking tools. New or extended
  dialects include **TRex** (positional CSV, `#wcentroid` / `#centroid` /
  `#pcentroid` variants, and `(cm)` / `(px)` unit annotations), **ANY-maze**,
  **Tracktor**, **Ctrax** (`.mat` via `R.matlab`, with ellipse `theta`/`a`/`b`),
  **TrackMate**, **idtracker.ai**, **ToxTrac**, **BORIS**, and **dtrack**.
* Pose-estimation support for **DeepLabCut** and **SLEAP**: load a single
  bodypart or a likelihood-weighted centroid of several, with per-bodypart
  columns retained for the `bodypart_axis` heading rule.
* Multi-zone support for EthoVision and ANY-maze (nose/tail/centre zone
  centroid or single-zone selection).
* Register custom formats at runtime with `register_loader_dialect()`.
* Bundled single-file examples (`inst/extdata/`) for DeepLabCut, SLEAP, and
  Tracktor so every dialect can be tried without external data.

## Headings

* `pose_to_headings()` derives many per-frame headings from multi-point pose
  data — suited to tethered-animal / gaze-direction designs.
* Additional heading rules registered through `register_heading_rule()`,
  including `bodypart_axis` and `ellipse_axis`.

## Circular statistics

* Parametric fits: `vonmises_fit()` (with standard errors and a CI on the mean
  direction) and `wrappedcauchy_fit()`.
* `circ_dispersion()` (mean direction, resultant length, circular SD) and
  `sector_summary()` (per-sector dwell proportions).
* `circ_cor()` — circular-linear and circular-circular correlation.
* Hypothesis tests returning tidy data frames, all with multiple-comparison
  correction via `p_adjust`: `test_uniformity()` (Rayleigh / Kuiper / Rao /
  Watson), `test_mean_directions()` (Watson-Williams, omnibus or pairwise), and
  `test_concentration()`.

## Visualisation

* Distribution overlays that compose onto a `radiate()` plot with `+`:
  `add_angle_rose()`, `add_vonmises_density()`, `add_wrappedcauchy_density()`,
  and `add_circular_kde()`.
* Significance geometry: `add_critical_r()` draws the Rayleigh / V-test critical
  circle, and `add_critical_v_line()` draws the V-test decision boundary (a
  straight line perpendicular to the hypothesised direction).

## Graphical interface

* `launch_app()` starts a browser-based wizard (Upload → Configure → Results)
  for users who do not write R, including condition detection and a Rayleigh
  test. The app also builds as a static `shinylive` site.

## Example data

* The bundled example dataset is now `cpunctatus` (and the raw tracks
  `cpunctatus_tracks`), a subset of the *Cylindroiulus punctatus* millipede
  visual-orientation experiment of Kirwan & Nilsson (2019,
  *Vision Research* 165:36–44). This replaces the previous *Paracentrotus
  lividus* sea-urchin example.

## Documentation

* New vignette *Circular Statistics and Distribution Overlays* covering the
  fitting, testing, correlation, and overlay layers end to end.
* `pkgdown` site with a grouped reference index and a changelog.
