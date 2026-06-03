# Changelog

## radiatR (development version)

### Bug fixes

- The
  [`radiate()`](https://johnkirwan.github.io/radiatR/reference/radiate.md)
  mean-direction (directedness) arrow now respects the clock display
  convention. In clock-display plots the trajectories are rotated 90
  degrees (East to North) but the arrow was left in unit-circle
  coordinates, so it pointed ~90 degrees away from the tracks and
  heading markers it summarises. The arrow is now rotated with the rest
  of the plot.
- [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) on a
  `TrajSet` now works for users of the installed package. It had been
  defined as an S4 method on the base S3 generic, which is only
  reachable from within the package’s own namespace, so user code got
  “cannot coerce class TrajSet to a data.frame”. It is now a registered
  S3 method.

### Shiny app

- The upload step now offers a one-click **“Load the example millipede
  dataset”** so the app can be tried end to end without supplying a
  tracking file. It loads the bundled `cpunctatus` example and jumps
  straight to the configure step.
- Fixed the results step crashing or producing an empty summary when the
  tracking data’s trial-ID column was not literally named `id`. The
  condition join and per-trial summary now key off the headings frame’s
  own `id` column.
- Fixed the results plot showing “Plot unavailable” whenever a condition
  column was detected: it set
  [`radiate()`](https://johnkirwan.github.io/radiatR/reference/radiate.md)’s
  mutually exclusive `colour_col` and `colour_cycle` at once. Colours
  now cycle only when no condition drives the colour scale, and
  plot-render failures are logged rather than swallowed.

### Camera calibration

- Refocused the calibration layer on *importing* calibrations from
  established tools rather than estimating them.
  [`read_calibration()`](https://johnkirwan.github.io/radiatR/reference/read_calibration.md)
  reads camera intrinsics and Brown-Conrady distortion coefficients from
  the **MATLAB Computer Vision Toolbox** (`.mat`), **OpenCV**
  `FileStorage` (YAML/JSON), or a plain **CSV**, and
  [`cal_model()`](https://johnkirwan.github.io/radiatR/reference/cal_model.md)
  builds a `CalModel` from coefficients you already hold. Both handle
  radiatR’s transposed intrinsic-matrix convention and the 1-based
  (MATLAB) vs 0-based (OpenCV) principal-point difference for you.
- Removed the in-package calibration *estimator* and its interactive
  point-capture tools (`calibration_session()`,
  `calibration_from_points()`, `checkerboard_points()`, the
  `calibration_points_*` / `*_calibration_points` helpers, and
  `calibration_switch_axes()`). Estimating intrinsics from checkerboard
  images is better served by the mature toolboxes above; the bundled
  `calibration_corners.csv` / `calibration_truth.csv` fixtures are gone
  with it.
  [`cam_cal_pt()`](https://johnkirwan.github.io/radiatR/reference/cam_cal_pt.md),
  [`cam_cal_many()`](https://johnkirwan.github.io/radiatR/reference/cam_cal_many.md),
  and
  [`calibrate_positions()`](https://johnkirwan.github.io/radiatR/reference/calibrate_positions.md)
  are unchanged.

## radiatR 0.2.0

A large feature release that broadens *radiatR* from a focused plotting
toolkit into an end-to-end pipeline for circular-arena tracking: many
more data sources, a full circular-statistics layer, richer distribution
overlays, and a no-code graphical interface.

### Data loading

- Dialect-based loader system covering 20+ tracking tools. New or
  extended dialects include **TRex** (positional CSV, `#wcentroid` /
  `#centroid` / `#pcentroid` variants, and `(cm)` / `(px)` unit
  annotations), **ANY-maze**, **Tracktor**, **Ctrax** (`.mat` via
  `R.matlab`, with ellipse `theta`/`a`/`b`), **TrackMate**,
  **idtracker.ai**, **ToxTrac**, **BORIS**, and **dtrack**.
- Pose-estimation support for **DeepLabCut** and **SLEAP**: load a
  single bodypart or a likelihood-weighted centroid of several, with
  per-bodypart columns retained for the `bodypart_axis` heading rule.
- Multi-zone support for EthoVision and ANY-maze (nose/tail/centre zone
  centroid or single-zone selection).
- Register custom formats at runtime with
  [`register_loader_dialect()`](https://johnkirwan.github.io/radiatR/reference/register_loader_dialect.md).
- Bundled single-file examples (`inst/extdata/`) for DeepLabCut, SLEAP,
  and Tracktor so every dialect can be tried without external data.

### Headings

- [`pose_to_headings()`](https://johnkirwan.github.io/radiatR/reference/pose_to_headings.md)
  derives many per-frame headings from multi-point pose data — suited to
  tethered-animal / gaze-direction designs.
- Additional heading rules registered through
  [`register_heading_rule()`](https://johnkirwan.github.io/radiatR/reference/register_heading_rule.md),
  including `bodypart_axis` and `ellipse_axis`.

### Circular statistics

- Parametric fits:
  [`vonmises_fit()`](https://johnkirwan.github.io/radiatR/reference/vonmises_fit.md)
  (with standard errors and a CI on the mean direction) and
  [`wrappedcauchy_fit()`](https://johnkirwan.github.io/radiatR/reference/wrappedcauchy_fit.md).
- [`circ_dispersion()`](https://johnkirwan.github.io/radiatR/reference/circ_dispersion.md)
  (mean direction, resultant length, circular SD) and
  [`sector_summary()`](https://johnkirwan.github.io/radiatR/reference/sector_summary.md)
  (per-sector dwell proportions).
- [`circ_cor()`](https://johnkirwan.github.io/radiatR/reference/circ_cor.md)
  — circular-linear and circular-circular correlation.
- Hypothesis tests returning tidy data frames, all with
  multiple-comparison correction via `p_adjust`:
  [`test_uniformity()`](https://johnkirwan.github.io/radiatR/reference/test_uniformity.md)
  (Rayleigh / Kuiper / Rao / Watson),
  [`test_mean_directions()`](https://johnkirwan.github.io/radiatR/reference/test_mean_directions.md)
  (Watson-Williams, omnibus or pairwise), and
  [`test_concentration()`](https://johnkirwan.github.io/radiatR/reference/test_concentration.md).

### Visualisation

- Distribution overlays that compose onto a
  [`radiate()`](https://johnkirwan.github.io/radiatR/reference/radiate.md)
  plot with `+`:
  [`add_angle_rose()`](https://johnkirwan.github.io/radiatR/reference/add_angle_rose.md),
  [`add_vonmises_density()`](https://johnkirwan.github.io/radiatR/reference/add_vonmises_density.md),
  [`add_wrappedcauchy_density()`](https://johnkirwan.github.io/radiatR/reference/add_wrappedcauchy_density.md),
  and
  [`add_circular_kde()`](https://johnkirwan.github.io/radiatR/reference/add_circular_kde.md).
- Significance geometry:
  [`add_critical_r()`](https://johnkirwan.github.io/radiatR/reference/add_critical_r.md)
  draws the Rayleigh / V-test critical circle, and
  [`add_critical_v_line()`](https://johnkirwan.github.io/radiatR/reference/add_critical_v_line.md)
  draws the V-test decision boundary (a straight line perpendicular to
  the hypothesised direction).

### Graphical interface

- [`launch_app()`](https://johnkirwan.github.io/radiatR/reference/launch_app.md)
  starts a browser-based wizard (Upload → Configure → Results) for users
  who do not write R, including condition detection and a Rayleigh test.
  The app also builds as a static `shinylive` site.

### Example data

- The bundled example dataset is now `cpunctatus` (and the raw tracks
  `cpunctatus_tracks`), a subset of the *Cylindroiulus punctatus*
  millipede visual-orientation experiment of Kirwan & Nilsson (2019,
  *Vision Research* 165:36–44). This replaces the previous
  *Paracentrotus lividus* sea-urchin example.

### Documentation

- New vignette *Circular Statistics and Distribution Overlays* covering
  the fitting, testing, correlation, and overlay layers end to end.
- `pkgdown` site with a grouped reference index and a changelog.
