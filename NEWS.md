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
