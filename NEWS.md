# radiatR (development version)

## Plotting

* `radiate.headings_frame()` now draws the same theme-responsive radial chrome
  as `radiate()` for trajectories (circumference, ticks, degree labels, radial
  grid, origin) instead of a fixed bold circle, and gains `show_markers`,
  `colour_col`, `legend`, `display`, `grid`, `grid_colour`, `circumference`, and
  `origin` arguments. `show_markers = FALSE` returns the radial frame only, so
  callers can layer their own marker and statistic overlays.

## Shiny app

* The app now accepts a dataset of pre-computed headings (one angle per trial)
  in place of trajectories. Choose the input type on the upload screen, map the
  angle column, units, and convention (unit-circle or compass) plus an optional
  grouping column, and the Results figure draws the stacked dots / points, mean
  arrow, confidence interval, and Rayleigh / V-test from the angles directly.
  A "Load example headings" link derives a demonstration set from the bundled
  millipede data.

# radiatR 0.3.1

## Plotting

* The fallback circumference (drawn on grid-less themes such as `void`) is now
  drawn at `theme_classic`'s axis-line weight (0.5) rather than a bold 1.2, so it
  reads like an axis rather than a heavy frame.
* The radial axis chrome -- the circumference (unit circle), angular tick marks,
  and degree labels -- now takes its styling from the base theme chosen via
  `radiate(theme = )`, mapped from that theme's `axis.line` / `axis.ticks` /
  `axis.text`. Picking a different theme restyles them (colour, line width, label
  size and family), and colour is kept legible against a dark panel. `add_ticks()`
  gains `colour`, `linewidth`, `length`, and `n`; `degree_labs()` gains `size` and
  `family`; `add_circ()` gains `linetype` and `colour`/`linewidth` aliases.
  (Styling follows the theme passed to `radiate()`, not a `+ theme()` added
  afterwards, since the chrome is drawn as layers.)
* On grid-bearing themes the radial grid's outer ring now reaches the unit circle,
  so it marks the boundary; the separate circumference is drawn only as a fallback
  where no grid delineates it (control with `radiate(circumference = )`). A
  consequence: on light grid themes (`minimal`, `bw`, `light`) the boundary is now
  a subtle grid-coloured ring rather than a bold dark circle -- add a bold ring
  back with `+ add_circ(colour = "black", linewidth = 1.2)` if desired.
* `radiate(origin = )` now defaults to `FALSE` (the centre point is opt-in on all
  themes) and, when drawn, takes the theme's axis ink colour.

## Shiny app

* Stacked heading dots are spaced a little further apart and start a little
  further inward from the circumference (the `step` and `start_sep` defaults the
  app uses).

# radiatR 0.3.0

## Plotting

* **Behaviour change:** for grid-bearing themes (`bw`, `grey`, `light`, `dark`,
  `linedraw`, `minimal`), `radiate()` now replaces the Cartesian grid behind the
  unit circle with a theme-styled *radial* grid -- a circular disc in the
  theme's panel colour, quadrant crosshairs and a ring at 0.5 (major), and 45
  degree diagonals plus rings at 0.25/0.75 (minor). Control it with the new
  `grid = c("radial", "cartesian", "none")` argument (default `"radial"`);
  `grid = "cartesian"` restores the previous square grid. `grid_colour` overrides
  the derived colour.
* New exported `add_radial_grid()` and `add_origin_point()` composable layers,
  so the radial grid can be hand-built or restyled like any ggplot2 layer. The
  existing `quadrants`/`rings` arguments still add a-la-carte guides when
  `grid != "radial"`.

# radiatR 0.2.1

## Coordinate handling

* **Fix:** `normalize_xy = TRUE` (the `TrajSet()` and `TrajSet_read()` default) now
  arena-scales each trajectory instead of collapsing every point onto the unit
  circle. Previously it replaced each point with its unit vector (radius → 1),
  destroying trajectory shape: radius- and displacement-based heading rules
  (`net`, `distal`, `crossing`, `exit`, `origin_mean`, `straight`, ...) returned
  wrong or `NA` headings, and uploaded data in the Shiny app was affected. Each
  trajectory is now centred on its bounding-box midpoint and scaled so its
  furthest point sits at radius 1, preserving shape and placing the arena centre
  at the origin (what the radius-based rules expect). Raw coordinates are still
  kept in `<x>_raw`/`<y>_raw`, and `rho` now holds the relative radius rather than
  a constant 1. Landmark-based mapping, when available, remains the accurate path;
  this only improves the no-landmark fallback. Stored `x`/`y`/`rho` values change
  for `normalize_xy = TRUE`; pass `normalize_xy = FALSE` to keep raw coordinates.
* Landmark-mapped tracks are never rescaled (they are built `normalize_xy =
  FALSE`), preserving legitimate excursions past the arena boundary. Track points
  that fall outside the boundary (radius > 1) are now reported once per dataset --
  "N points across M trials exceeded the arena boundary (radius > 1)" -- instead
  of a separate warning per trial.

## Camera calibration

* **Breaking:** removed the camera-calibration layer entirely (`read_calibration()`,
  `cal_model()`, `cam_cal_pt()`, `cam_cal_many()`, `calibrate_positions()`, and the
  `CalModel` class), along with its vignette. radiatR normalises each trajectory to
  a unit arena, so its outputs (headings, mean direction, resultant length, circular
  statistics) are scale-invariant and never needed metric calibration; lens-distortion
  correction and scaling to real-world units are better handled in the upstream
  tracking pipeline (the tracker's own calibration, or OpenCV `undistort`) before
  import. This completes the narrowing begun in 0.2.0, which removed the calibration
  *estimator*. The `R.matlab`, `yaml`, and `jsonlite` suggested dependencies remain —
  they are still used by the loaders.

## Shiny app

* The upload screen now loads faster: `ggplot2` and `radiatR` (and, through
  `radiatR`, `circular`/`boot`/`mvtnorm`) are attached lazily the first time the
  user loads data, rather than at app startup. Under the WebAssembly (shinylive)
  build, attaching those packages is the bulk of the in-browser R boot, so the
  initial upload screen now paints on `shiny` + `bslib` alone and the heavier
  load happens when a file or the example is chosen.
* The Results figure no longer prints a "Heading method: …" subtitle above the
  plot. (The plot spec can still carry a subtitle; the app just no longer sets
  one.)
* Restored the **Quadrant lines** and **Guide rings** toggles, which had become
  inert when the Results figure moved to the shared plot spec: the spec now
  carries them and passes `quadrants` / `rings` to `radiate()`, and the **R
  code** export emits them, so every plot control round-trips through the export
  again.

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
* `derive_headings(..., return_coords = TRUE)` now also works for `distal`, `net`,
  `straight`, and `pca_axis` (previously only `crossing`), attaching each rule's
  construction coordinates so the geometry behind a heading can be drawn.
* New `bin_angles()` snaps angles to fixed-width circular bin centres. `phase = 0`
  (default) centres bins on the reference direction; `phase = width / 2`
  reproduces the edge-aligned bins of `circular::plot.circular`; arbitrary phases
  allow e.g. quadrant binning. Intended as the precursor to a stacked dot plot
  (`bin_angles()` then `stack_headings()`).
* `stack_headings()` and `add_stacked_headings()` gain a `start_sep` argument (the
  analogue of `circular::plot.circular`'s `start.sep`): a radial offset of the
  first dot from the reference circle, so a stack can abut the periphery rather
  than straddle it. The existing `step` argument (the analogue of `sep`) sets the
  gap between dots. Both default to circular's behaviour at the package level.
* `stack_headings()` and `add_stacked_headings()` gain a `group` argument that
  stacks dots within each group independently (e.g. one stacking per facet).

## Track metrics

* New `path_straightness(x, y)` and `straightness_index(ts)` compute the path
  straightness index (net displacement / path length, 0–1) per trajectory.
* New `path_tortuosity(x, y)` and `tortuosity_ratio(ts)` compute the classic
  tortuosity ratio (path length / net displacement, ≥ 1), the reciprocal of the
  straightness index.

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
* `add_critical_r()` gains a `colour_by_group` argument (default `TRUE`). With
  `per_group = TRUE` the per-panel circles are mapped to the group by default;
  set `colour_by_group = FALSE` to draw them in a fixed `colour` while still
  attaching the group column so the circles facet -- avoiding a clash with an
  existing colour scale (e.g. the trajectory colours in the Shiny app).
* `radiate()` gains an `arrow_colour_col` argument: when set, the built-in mean
  resultant arrow is drawn once per level of that column (within each panel, if
  faceted) and coloured by it, so the arrow can follow a colour grouping
  independently of faceting. The composable `add_heading_arrow()` and
  `add_heading_interval()` already take `colour_col` for the same effect on the
  mean-direction arrow and its confidence interval; the circular-statistics
  vignette now shows this grouped-overlay pattern.
* New exported `cycle_colours()` -- the order-stable primitive behind
  `assign_cycle_colours()` and `radiate()`'s `colour_cycle`. It maps a key to a
  cycled `1:n` colour index and accepts an explicit level order, so two data
  frames sharing a key (e.g. tracks and an overlay) can be coloured identically.
  `assign_cycle_colours()` now delegates to it, and the Shiny app uses it too, so
  the colour-cycling logic has a single source of truth.
* New exported `assign_colour_key()` attaches a shared colour-key column to a
  TrajSet or data frame (cycled for the trajectory or a high-cardinality key, raw
  values + legend for a low-cardinality grouping; `reference` keeps the key
  consistent across frames), so tracks and overlays colour identically. It
  replaces the Shiny app's internal colour helpers.
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

## Graphical interface

* `launch_app()` starts a browser-based wizard (Upload → Configure → Results)
  for users who do not write R, including condition detection and a Rayleigh
  test. The app also builds as a static `shinylive` site.
* The package is the single source of truth and the app a thin presentation
  layer over it: the Results figure is resolved into a plain spec that both
  renders the on-screen plot (calling only exported functions) and emits the
  equivalent radiatR/ggplot2 script. A round-trip test asserts the emitted code
  reproduces the rendered figure (comparing layer coordinates), so the two paths
  cannot silently diverge.
* The Results step gains an **R code** panel that shows the radiatR/ggplot2 code
  reproducing the current figure, with **Copy** and **Download .R** buttons. The
  emitted script calls only exported package functions.
* The upload step now offers a one-click **"Load the example millipede
  dataset"** so the app can be tried end to end without supplying a tracking
  file. It loads the bundled `cpunctatus` example and jumps straight to the
  configure step.
* The Configure step gains a live **method preview**: a unit-circle panel beside
  the controls that draws a few example tracks and re-renders to show how the
  selected heading rule derives its heading, updating as the method or ring
  radii change. Several rules now draw their construction: `crossing` its two
  detection rings, ring-crossing dots, and dashed vector to the rim; `distal` the
  furthest-from-centre point; `net` the first-to-last chord; `straight` the
  longest straight run; and `pca_axis` the principal axis. Remaining rules mark
  just the derived heading point. Constructions and heading markers take each
  trajectory's colour.
* The Configure step's heading-method picker is now a dropdown exposing all of
  the package's parameter-free heading rules (plus ring `crossing`), with a
  one-line description of the selected rule shown beneath it.
* The Results Display panel's "Heading points" toggle is replaced by a **Heading
  display** dropdown (Points (overlapping) / Stacked dots (inward) / None) that
  selects how the per-trial heading markers are drawn. Stacked dots fan
  coincident headings radially inward to reduce overplotting, stacked within
  each condition group when the plot is faceted.
* A new **None (no headings)** option plots the tracks and path metrics only:
  the plot gains a straightness caption, the summary table reduces to Group and
  Straightness, and all circular-statistics overlays are suppressed.
* In None mode the data download exports the per-trial path-metrics table
  ("Metrics (CSV)") instead of headings. The data-download button is also
  restyled so it no longer appears greyed out.
* The Results summary table gains a **Straightness** column: the mean path
  straightness index across each group's trials.
* Plot colour now distinguishes the **trajectory** (in sequential order) by
  default, not the panel/facet variable. The "Facet by" selector (formerly
  "Group by condition") now only facets; a new **"Colour by"** selector in the
  Display panel controls colour independently -- defaulting to "Trajectory", or
  any grouping column (condition, cohort, individual, ...). Tracks and heading
  markers (points and stacked) share one colour key, so each marker matches its
  own trajectory/group even when faceted. A grouping with up to 20 levels gets a
  distinct colour per level and a legend; a higher-cardinality key (the
  trajectory, or e.g. an individual id with many animals) cycles a capped set of
  20 colours with no legend.
* The "Heading vectors" overlay inherits each trajectory's (or group's) colour,
  matching the tracks and markers, instead of being drawn in one colour.
* The mean-direction confidence interval, the Rayleigh critical circle, and the
  V-test decision boundary are all part of the shared plot spec, drawn by the
  exported `add_heading_interval()`, `add_critical_r()`, and
  `add_critical_v_line()` that the **R code** panel emits, so every statistical
  overlay on the Results figure is reproduced by the code export. The figure's
  subtitle (heading method) and caption (path-metrics summary, in the no-headings
  mode) are carried by the spec and emitted too, so the entire figure — body,
  overlays, and annotations — is reproduced by the exported script.
* The Results step's Display panel gains a **Theme** dropdown (Void, Minimal,
  Classic, Black & white, Grey, Light, Dark, Line draw) that restyles the
  on-screen plot and the download, an **Angle labels** dropdown (Degrees / None /
  Radians), and **Quadrant lines** and **Guide rings** toggles (both off by
  default; both follow the selected theme's grid styling).
* The Results step has on/off toggles for the trajectories, heading points,
  directedness arrow, and a mean-direction confidence-interval arc (95%
  bootstrap CI, off by default). The toggles drive both the on-screen plot and
  the plot download, and live in a "Display" card in the right-hand column,
  giving the plot panel the full width of its column.
* Plot downloads can now be exported as **PDF** or **SVG** (editable vector
  formats) as well as PNG, with editable width/height (inches) and, for PNG, a
  resolution control. A "Preview size" slider scales the on-screen plot canvas
  without affecting the exported file dimensions, and the preview tracks the
  chosen aspect ratio.
* Fixed the "Stacked dots (inward)" heading display, which superimposed every dot
  at the rim instead of stacking. Headings are now binned (5-degree bins centred
  on the reference direction) before stacking, producing inward radial columns;
  the stack sits just inside the unit circle (dots abut the periphery line rather
  than straddle it) with a slightly wider gap between dots.
* Fixed an error ("NAs are not allowed in subscripted assignments") when stacking
  headings that include two or more `NA` values (trials with no defined heading).
  The internal angle-wrapping helper is now NA-safe.

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

## Bug fixes

* `add_angle_rose()`, `add_circular_kde()`, `add_vonmises_density()`, and
  `add_wrappedcauchy_density()` now honor the plot's `display` rotation (via a
  new `display` argument, the input's `display` attribute, or the identity
  default), so they stay aligned with the tracks and heading markers on a
  rotated display (e.g. a clock-oriented plot) instead of being drawn ~90
  degrees off. Behaviour on the default plot is unchanged.
* The `crossing` heading rule now computes the heading as the bearing of the
  inner→outer ring-crossing vector projected onto the arena boundary (the unit
  circle) — the method described in the vignette and used in the original
  analysis — rather than the slope of the crossing segment. The two agree for
  radial tracks but differ for oblique ones, so crossing-based circular
  statistics change for non-radial trajectories. `derive_headings(rule =
  "crossing", return_coords = TRUE)` now also returns the outer crossing
  (`x_outer`/`y_outer`).
* The mean-direction (directedness) arrow now respects the clock display
  convention. In clock-display plots the trajectories are rotated 90 degrees
  (East to North) but the arrow was left in unit-circle coordinates, so it
  pointed ~90 degrees away from the tracks and heading markers it summarises.
  `compute_circ_mean()` carries the input's `display` attribute onto its output
  and the arrow is rotated with the rest of the plot, on screen and in the
  exported code.
* `as.data.frame()` on a `TrajSet` now works for users of the installed
  package. It had been defined as an S4 method on the base S3 generic, which is
  only reachable from within the package's own namespace, so user code got
  "cannot coerce class TrajSet to a data.frame". It is now a registered S3
  method.

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
