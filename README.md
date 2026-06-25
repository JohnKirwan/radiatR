# radiatR <a href="https://johnkirwan.github.io/radiatR/"><img src="man/figures/logo.png" align="right" height="139" alt="radiatR website" /></a>

<!-- badges: start -->
[![R-CMD-check](https://github.com/JohnKirwan/radiatR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JohnKirwan/radiatR/actions/workflows/R-CMD-check.yaml)
[![r-universe](https://johnkirwan.r-universe.dev/badges/radiatR)](https://johnkirwan.r-universe.dev/radiatR)
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

### Circular regression

Model a heading on linear covariates (Fisher-Lee circular-linear regression):

```r
# simulate a known predictor -> mean-heading effect, then recover it
cond <- data.frame(condition = "demo", n_trials = 150, ref_mean = 0,
                   concentration_base = 12, mean_slope = 0.6,
                   predictor_mean = 0, predictor_sd = 1)
s  <- simulate_tracks(conditions = cond, n_points = 8, seed = 1)
hd <- s[!duplicated(s$trial_id), c("predictor", "final_heading")]
names(hd)[2] <- "heading"

fit <- circ_regression(hd, heading ~ predictor)
summary(fit)            # tidy coefficient table (recovers the positive slope)
predict(fit, data.frame(predictor = c(-1, 0, 1)))

# draw the fitted sweep on the circular panel
radiate(headings_frame(hd, heading, units = "radians")) +
  add_circ_mean(fitted_directions(fit, at = seq(-2, 2, length.out = 7)),
                colour_col = "predictor")
```

## Typical Workflow

```r
library(radiatR)

# 1. Load tracking data (example: DeepLabCut with head + thorax bodyparts)
ts <- read_tracks(
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

> **Coordinates.** radiatR normalises each trajectory to a unit circle, so its
> outputs (headings, mean direction, resultant length, circular statistics) are
> scale-invariant and need no metric calibration. Correct lens distortion and
> any scaling to real-world units in your tracking pipeline (e.g. your tracker's
> own calibration, or OpenCV `undistort`) before importing.

### Label the perimeter in domain units

The circumference can be labelled in domain units instead of degrees — handy for
time-of-year or compass data. `angle_labels` selects a built-in scale
(`"cardinal"`, `"hours"`, `"months"`, `"seconds"`) and aligns the tick count to
it:

```r
# Label the circle in months instead of degrees
radiate(ts, angle_labels = "months")
```

### Colour tracks by position

`track_colour = "sequence"` shades each trajectory along its own length, so the
direction of travel is visible at a glance — a per-track gradient from start
(dark) to finish (bright), with a continuous "start → finish" colourbar:

```r
# colour each track from start (dark) to finish (bright)
radiate(cpunctatus, show_tracks = TRUE, track_colour = "sequence")
```

### Representing time

Attach a capture **frame rate** to a `Tracks` object and the time aspect of
frame-indexed tracks can be reported in real seconds and shown on the plot.
`track_duration()` gives the elapsed seconds per trajectory, and
`track_colour = "time"` colours each path by elapsed time (POSIXct time works
without a frame rate):

```r
# attach a capture rate, then represent the time aspect
ts <- set_frame_rate(cpunctatus, fps = 30)
track_duration(ts)                                       # seconds per track
radiate(ts, show_tracks = TRUE, track_colour = "time")   # colour by elapsed time
```

A frame rate also lets `track_speed()` report trajectory speed in real units
(radii per second):

```r
# speed in radii per second once a frame rate is set
ts <- set_frame_rate(cpunctatus, fps = 30)
track_speed(ts)              # mean speed per track
track_speed(ts, stat = "max")
```

```r
# colour each path by its instantaneous speed (needs a frame rate)
ts <- set_frame_rate(cpunctatus, fps = 30)
radiate(ts, show_tracks = TRUE, track_colour = "speed")
```

If you know a physical scale, calibrate distances so lengths and speeds report
in real units. The scale is physical units per coordinate unit (e.g. mm per
radius); unset, everything stays in coordinate units (radii).

```r
# calibrate distance (optional): 50 mm per coordinate unit (radius)
ts <- set_distance_scale(set_frame_rate(cpunctatus, 30), 50, unit = "mm")
track_length(ts)            # path length per track, in mm
track_speed(ts)             # mean speed per track, in mm/s
# or from two measured landmarks:
# ts <- calibrate_distance(ts, coord_distance = 0.8, real_distance = 40, unit = "mm")
```

```r
# velocity components and turning rate (need a frame rate; vx/vy use the distance scale)
ts <- set_frame_rate(cpunctatus, fps = 30)
head(velocity_vector(ts))                 # vx, vy per observation
head(angular_velocity(ts, units = "degrees"))   # turning rate, deg/s (CCW +)
```

```r
# per-track summaries (need a frame rate; velocity uses the distance scale)
ts <- set_frame_rate(cpunctatus, fps = 30)
track_velocity(ts)                       # net velocity vector (vx, vy) per track
track_turning(ts, units = "degrees")     # typical turning rate (deg/s) per track
```

```r
# non-circular kinematics: speed (or turning rate) along each track over time
ts <- set_frame_rate(cpunctatus, fps = 30)
plot_profile(ts, metric = "speed")
plot_profile(ts, metric = "turning", units = "degrees")
```

### Circular boxplot

A Tukey-like boxplot for circular data (Buttarazzi, Pandolfo & Porzio, 2018):
the box spans the central 50% around the circular median, whiskers reach a
concentration-adjusted fence, and far-out values are marked individually.
Position-based axial data is supported via `axial = TRUE` (drawn at both poles).

```r
hd <- derive_headings(cpunctatus, rule = "crossing", circ0 = 0.2, circ1 = 0.4)
radiate(cpunctatus) + add_circular_boxplot(hd)

# the summary on its own (median, hinges, fences, far-out, fence multiplier)
circ_boxplot_stats(hd)$constant
```

## Simulate Data

`simulate_tracks()` generates synthetic circular trajectories for testing pipelines and teaching:

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
tracks move back and forth along an axis, the kind of data the position-based
axial heading methods (`pca_axis`, `ransac_straight`) are built for:

```r
# Oscillatory axial tracks: each moves back-and-forth along a ~0.6 rad axis
osc <- data.frame(condition = "axial_osc", n_trials = 40L, ref_mean = 0.6,
                  concentration_base = 50, track_shape = "oscillatory",
                  n_reversals = 4L)
ts <- simulate_tracks(conditions = osc, output = "trajset", seed = 1)

# the position-based axial methods recover the ~0.6 rad axis (read as an
# axial mean) at default settings:
ax <- derive_headings(ts, rule = "pca_axis")
circ_summarise(ax, "heading", axial = TRUE, units = "radians", stats = "mean_dir")

# a directional rule cannot: the back-and-forth directions cancel
net <- derive_headings(ts, rule = "net")
circ_summarise(net, "heading", units = "radians", stats = "resultant_R")
```

## Installation

```r
# From r-universe (pre-built binaries)
install.packages("radiatR",
                 repos = c("https://johnkirwan.r-universe.dev",
                           "https://cloud.r-project.org"))

# From GitHub
remotes::install_github("JohnKirwan/radiatR")

# From local clone
devtools::install_local(".")
```

## The graphical app

A browser-based interface covers the common workflow — upload a tracking
file, choose a heading method, and view circular plots and summary
statistics — with no R coding required.

> **Try it live:** <https://johnkirwan.shinyapps.io/radiatR/>

There are two ways to reach it:

**1. Run locally** (works after installing the package):

```r
radiatR::launch_app()
```

**2. Hosted on shinyapps.io** (recommended for collaborators) —
<https://johnkirwan.shinyapps.io/radiatR/>. A real Shiny server runs R
server-side, so the page loads like any normal web app. The GitHub Actions
workflow in `.github/workflows/shinyapps.yaml` deploys `inst/app` on each
**published GitHub Release** (and on demand via *workflow_dispatch*). It needs
three repository secrets from your shinyapps.io account — `SHINYAPPS_NAME`
(the account name in the URL), `SHINYAPPS_TOKEN`, and `SHINYAPPS_SECRET`
(Account → Tokens). To deploy by hand instead:

```r
rsconnect::deployApp(system.file("app", package = "radiatR"))
```

> The old in-browser `shinylive` (WebAssembly) build at
> <https://johnkirwan.github.io/radiatR/app/> has been retired — that URL now
> redirects to the shinyapps.io app above. The WASM build shipped the whole R
> runtime to the browser, so it loaded slowly and needed cross-origin isolation
> that GitHub Pages can't provide, making it unreliable. The server-side
> shinyapps.io deploy replaces it.

## Optional Dependencies

| Package | Used for |
|---|---|
| `ggrepel` | Non-overlapping track labels |
| `readr` / `data.table` | Faster CSV reading |
| `jsonlite` | GeoJSON and JSON dialect loaders |
| `xml2` | GPX dialect loader |
| `R.matlab` | Ctrax `.mat` file loader |
| `arrow` | Parquet/Feather file support |
