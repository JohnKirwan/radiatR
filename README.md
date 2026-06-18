# radiatR <a href="https://johnkirwan.github.io/radiatR"><img src="man/figures/logo.png" align="right" height="139" alt="radiatR website" /></a>

<!-- badges: start -->
[![R-CMD-check](https://github.com/JohnKirwan/radiatR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JohnKirwan/radiatR/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

An R package for analysing and visualising **headings and trajectories in circular space** — directional (angular) data of any kind. It covers the full pipeline: angle/trajectory input → coordinate transformation → circular statistics → ggplot2 visualisation. Bring a table of headings directly, or reconstruct them from movement trajectories, with import from 20+ tracking tools.

## Key Features

- **Flexible loader framework** — read data from 20+ tracking tools via registered dialects; extend with custom formats
- **Pose-estimation support** — multi-bodypart centroid and body-axis heading from DeepLabCut, EthoVision multi-zone, ANY-maze nose/tail, Ctrax ellipse
- **Heading rule registry** — derive per-trial headings by any of 15+ built-in rules or register your own
- **Circular statistics** — mean direction, resultant length, concentration, and grouped summaries via the `circular` package
- **ggplot2 visualisation** — `radiate()` renders unit-circle plots of headings and trajectories with concentric guides, mean-direction arrows, tick marks, and faceting; composable with `add_*` layer helpers

## Supported Tracking Tools

| Dialect | Tool |
|---|---|
| `dtrack` | dtrack |
| `deeplabcut` / `deeplabcut_multiheader` | DeepLabCut (single point or multi-bodypart centroid) |
| `idtrackerai_wide` | idtracker.ai |
| `ethovision` | EthoVision XT (including multi-zone) |
| `trackmate` | TrackMate (Fiji) |
| `toxtrac` | ToxTrac |
| `boris_xy` | BORIS |
| `trex` | TRex (positional CSV; plain `X`/`Y` or `#wcentroid`/`#centroid`/`#pcentroid` variants) |
| `anymaze` | ANY-maze (including nose/tail zones) |
| `tracktor` | Tracktor |
| `ctrax` | Ctrax (`.mat` files; preserves `theta`, `a`, `b`) |
| `motchallenge` | MOTChallenge / SORT / DeepSORT |
| `geojson_linestring` | GeoJSON LineString |
| `gpx` | GPX tracks |
| `nmea_gprmc` | NMEA GPRMC |
| `wide_prefix_xy` | Generic `x_<id>` / `y_<id>` wide format |

Register additional formats with `register_loader_dialect()` or the declarative `register_loader_format()`.

## Heading Rules

`derive_headings()` dispatches to any of the built-in rules or a registered custom rule:

| Rule | Method |
|---|---|
| `crossing` | Angle at ring-crossing event (two radii) |
| `distal` | Angle at frame of maximum radial distance |
| `exit` | Velocity direction at ring crossing |
| `net` | Start-to-end vector |
| `velocity_mean` | Circular mean of per-frame velocity angles |
| `vm_fit` | von Mises MLE over per-frame angles |
| `ransac_straight` | RANSAC-fitted straight-segment direction |
| `origin_mean` | Distance-weighted mean angle from origin |
| `bodypart_axis` | Axis between two tracked keypoints (pose data) |
| `ellipse_axis` | Pre-computed orientation angle column (e.g. Ctrax `theta`) |
| … | `straight`, `window_net`, `goal_bias`, `pca_axis`, `maxspeed_window`, `entry`, `ring_tangent` |

Register custom rules with `register_heading_rule()`.

## Circular statistics

Beyond summaries, radiatR covers the common circular-statistics workflow — all
returning tidy data frames, and most accepting `axial = TRUE` for bidirectional
(mod-180°) data such as orientation or polarization axes.

- **Uniformity / modality tests** — `test_uniformity()` runs the Rayleigh
  (default), Kuiper, Rao spacing, Watson, and **Hermans–Rasson** (Monte-Carlo,
  powerful against multimodal alternatives) tests.
- **Parametric fits** — `vonmises_fit()` and `wrappedcauchy_fit()` (each with an
  `axial` mode), overlaid on plots with `add_vonmises_density()` /
  `add_wrappedcauchy_density()`.
- **Model selection** — `circ_model_select()` ranks *uniform* vs *unimodal* vs
  *axial* von Mises by AICc with Akaike weights, so the data indicate the modality.
- **Summaries & association** — `circ_summary()` / `circ_summarise()`,
  `circ_dispersion()`, and `circ_cor()` (circular–linear and circular–circular).

```r
hd <- derive_headings(cpunctatus, rule = "distal")

# Is the sample non-uniform, and how is it best described?
test_uniformity(hd, test = "hermans_rasson")   # omnibus; catches multimodality
circ_model_select(hd)                            # uniform / unimodal / axial by AICc
```

The graphical app surfaces these too: a selectable omnibus test and a
model-selection readout alongside the summary table.

## Typical Workflow

```r
library(radiatR)

# 1. Load tracking data (example: DeepLabCut with head + thorax bodyparts)
ts <- TrajSet_read(
  "my_tracks.csv",
  dialect      = "deeplabcut",
  dialect_args = list(bodypart = c("head", "thorax"))
)

# 2. Derive headings
hd <- derive_headings(ts, rule = "crossing",
                      circ0 = 0.3, circ1 = 0.6,
                      coords = "absolute",
                      angle_convention = "unit_circle")

# 3. Circular summary per condition
compute_circ_mean(hd, colour_col = "condition")

# 4. Visualise
radiate(ts,
        group_col  = "trial_id",
        colour_col = "condition",
        panel_by   = "condition",
        show_arrow = TRUE) +
  add_heading_points(hd, colour_col = "condition")
```

> **Coordinates.** radiatR normalises each trajectory to a unit arena, so its
> outputs (headings, mean direction, resultant length, circular statistics) are
> scale-invariant and need no metric calibration. Correct lens distortion and
> any scaling to real-world units in your tracking pipeline (e.g. your tracker's
> own calibration, or OpenCV `undistort`) before importing.

## Simulate Data

`simulate_tracks()` generates synthetic arena trajectories for testing pipelines and teaching:

```r
# Three default conditions differing in concentration and tortuosity
ts <- simulate_tracks(seed = 42, output = "trajset")
radiate(ts, group_col = "trial_id", panel_by = "condition", show_arrow = TRUE)

# Custom conditions table
conds <- data.frame(
  condition          = c("low", "high"),
  n_trials           = 20L,
  concentration_base = c(1.5, 8),
  tortuosity_base    = c(0.12, 0.03)
)
ts2 <- simulate_tracks(conditions = conds, seed = 1, output = "trajset")
```

Each condition can also carry a known directional **ground truth**. Set a
**modality** (`uniform` / `unimodal` / `axial` / `multimodal`, with `n_modes`)
to control the distribution of per-track headings, and a **track shape**
(`directed` or `oscillatory`) to control within-track geometry — `oscillatory`
tracks move back and forth along an axis, the kind of data the per-track axial
heading methods are built for:

```r
# Oscillatory axial tracks: each moves back-and-forth along a ~0.6 rad axis
osc <- data.frame(condition = "axial_osc", n_trials = 40L, ref_mean = 0.6,
                  concentration_base = 50, track_shape = "oscillatory",
                  n_reversals = 4L)
ts <- simulate_tracks(conditions = osc, n_points = 80, output = "trajset", seed = 1)

# velocity_axis recovers the ~0.6 rad axis (read as an axial mean):
ax <- derive_headings(ts, rule = "velocity_axis")
circ_summarise(ax, "heading", axial = TRUE, units = "radians", stats = "mean_dir")

# a directional rule cannot: the back-and-forth directions cancel
net <- derive_headings(ts, rule = "net")
circ_summarise(net, "heading", units = "radians", stats = "resultant_R")
```

## Installation

```r
# From GitHub
remotes::install_github("JohnKirwan/radiatR")

# From local clone
devtools::install_local(".")
```

## The graphical app

A browser-based interface covers the common workflow — upload a tracking
file, choose a heading method, and view circular plots and summary
statistics — with no R coding required. There are three ways to reach it:

**1. Run locally** (works after installing the package):

```r
radiatR::launch_app()
```

**2. Hosted on the web** — the bundled app deploys to a Shiny host without
modification, giving a clickable link for collaborators who do not use R:

```r
rsconnect::deployApp(system.file("app", package = "radiatR"))
```

**3. Static site (no server)** — a `shinylive` build runs the app entirely
in the browser via WebAssembly, so uploaded files never leave the user's
machine (which suits sensitive data). The GitHub Actions workflow in
`.github/workflows/shinylive.yaml` publishes it to GitHub Pages alongside
the documentation, at <https://johnkirwan.github.io/radiatR/app/>. The build
runs on each **published GitHub Release**: it compiles radiatR into a webR
WebAssembly image (attached to the release), then exports and deploys the
app. Publish or re-publish a release to refresh the hosted app.

## Optional Dependencies

| Package | Used for |
|---|---|
| `ggrepel` | Non-overlapping track labels |
| `readr` / `data.table` | Faster CSV reading |
| `jsonlite` | GeoJSON and JSON dialect loaders |
| `xml2` | GPX dialect loader |
| `R.matlab` | Ctrax `.mat` file loader |
| `arrow` | Parquet/Feather file support |
