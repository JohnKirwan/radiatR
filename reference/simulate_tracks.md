# Simulate trajectory sets under configurable experimental conditions

This module generates synthetic trajectories whose directional
concentration and tortuosity vary across experimental conditions and
continuous predictors. It is useful for creating reproducible examples,
tutorials, and test fixtures without relying on large raw tracking
files.

## Usage

``` r
simulate_tracks(
  n_points = 200,
  conditions = NULL,
  output = c("tibble", "trajset", "both"),
  write_path = NULL,
  seed = NULL,
  radial_noise = 0.02,
  phi = 0.85,
  frame_rate = NULL
)
```

## Arguments

- n_points:

  Integer number of frames per trajectory.

- conditions:

  Optional data frame describing experimental conditions. See Details
  for column descriptions. When omitted, a three-condition template is
  used.

- output:

  Character string controlling the return type: "tibble" (default)
  returns a long-form data frame, "trajset" returns a \[Tracks\] object,
  and "both" returns a list containing both representations.

- write_path:

  Optional file path (CSV) to which the simulated data should be
  written.

- seed:

  Optional integer seed supplied to \[set.seed()\] for reproducibility.

- radial_noise:

  Standard deviation of radial noise applied to the unit radius profile.

- phi:

  Autocorrelation parameter (0-1) used when generating angular noise
  series; higher values produce smoother paths.

- frame_rate:

  Optional capture frame rate (frames per second) attached to the
  returned \[Tracks\] via \[set_frame_rate()\] when \`output\` is
  \`"trajset"\` or \`"both"\`. The default \`NULL\` leaves the output
  unchanged.

## Value

Depending on \`output\`, a tibble, a \`Tracks\`, or a list containing
both. When \`write_path\` is supplied the data are also written to disk.

## Details

The \`conditions\` data frame can contain the following columns
(defaults are supplied when missing):

\- \`condition\` (character): condition label. - \`n_trials\` (integer):
number of trajectories to simulate for the condition. - \`ref_mean\`
(numeric radian): baseline reference heading (default 0). -
\`mean_slope\` (numeric, default 0): per-condition slope shifting the
mean heading with the predictor. The effective per-trial mean is
\`ref_mean + mean_slope \* predictor\` and is recorded in
\`ref_heading\`. A default of 0 reproduces the historical seeded output
byte-for-byte. - \`concentration_base\` (numeric): baseline von Mises
concentration (kappa). - \`concentration_slope\` (numeric): optional
slope applied to the predictor. - \`tortuosity_base\` (numeric):
baseline angular noise scale. - \`tortuosity_slope\` (numeric): slope
applied to the predictor. - \`tortuosity_sd\` (numeric): additional
random variation per trial. - \`predictor_mean\`, \`predictor_sd\`
(numeric): parameters used to sample the per-trial predictor when
explicit values are not supplied. - \`predictor_values\` (list-column):
optional explicit predictor values (length \`n_trials\`) overriding the
generated values. - \`modality\` (character): the sample modality from
which per-trial principal headings are drawn. One of \`"unimodal"\`
(default), \`"uniform"\`, \`"axial"\`, or \`"multimodal"\`. Controls the
\*distribution of headings across trials\*, not the within-trial path
shape. - \`n_modes\` (integer): number of evenly spaced modes used when
\`modality == "multimodal"\` (default 1; ignored by other modalities). -
\`track_shape\` (character): the \*within-track\* path shape. One of
\`"directed"\` (default) – a single sweep towards \`final_heading\` – or
\`"oscillatory"\` – back-and-forth motion along the principal axis
\`final_heading\`. Oscillatory tracks form a genuinely axial position
cloud, so the position-based axial methods (\`pca_axis\`,
\`ransac_straight\`) recover the axis at default settings; the
directional methods (e.g. \`net\`) cancel. The step-based
\`velocity_axis\` recovers the axis only when sampling is coarse enough
that the per-step axial motion exceeds the perpendicular line-width
jitter (see the \`line_width\` note below). - \`n_reversals\` (integer):
number of direction reversals in an oscillatory track (default 3;
ignored when \`track_shape == "directed"\`). - \`amplitude\` (numeric):
peak excursion along the axis for an oscillatory track, clamped to
\`\[1e-3, 1\]\` (default 0.9; ignored when directed). - \`line_width\`
(numeric): half-width of an oscillatory track expressed as a fraction of
\`amplitude\`, controlling the perpendicular Gaussian jitter (default
0.05, clamped to \`\[1e-4, 1\]\`; ignored when directed). The line-width
is intentionally independent of \`tortuosity\_\*\`, so the track is a
genuinely thin line and the principal axis is recoverable by the
position-based methods (\`pca_axis\`, \`ransac_straight\`) at default
settings. The step-based \`velocity_axis\` is sampling-density
sensitive: because the per-frame along-axis step shrinks with
\`n_points\` while the perpendicular jitter step does not, dense
sampling lets the jitter dominate and the estimate flips toward the
perpendicular.

The predictor can represent any continuous covariate (e.g. reference
intensity). The final heading concentration increases with larger kappa,
whereas larger tortuosity values produce more sinuous paths.

For each trial the principal (final) heading is drawn according to the
condition's \`modality\`: \`"unimodal"\` draws from a single von Mises
about \`ref_mean\`; \`"uniform"\` draws from a circular uniform
distribution; \`"axial"\` draws from a von Mises about \`ref_mean\` or
\`ref_mean + pi\` with equal probability; \`"multimodal"\` draws from
one of \`n_modes\` von Mises components evenly spaced around the circle
starting at \`ref_mean\`. The \`"unimodal"\` branch is identical to the
historical draw, so seeded output is unchanged from earlier versions.

For an \`"oscillatory"\` track the position sweeps back and forth along
the axis \`final_heading\` following a deterministic triangle wave of
amplitude \`amplitude\` with \`n_reversals\` direction changes, plus a
small Gaussian jitter perpendicular to the axis with standard deviation
\`amplitude \* line_width\`. This line-width is independent of the
tortuosity settings, so the track stays a thin line and the axis remains
recoverable by the position-based methods (\`pca_axis\`,
\`ransac_straight\`) at default settings. The step-based
\`velocity_axis\` recovers the same axis only at coarse sampling, since
its per-step axial signal shrinks with \`n_points\` while the
perpendicular jitter step does not. The \`"directed"\` branch is
byte-identical to the historical geometry (and never draws the
perpendicular jitter), so the default seeded output is unchanged.

Every simulated row records the ground-truth generating structure in
additional columns: \`modality\` (character), \`n_modes\` (integer),
\`mode_id\` (integer index of the component the heading came from),
\`mode_mean\` (numeric radian mean of that component as the un-wrapped
generating centre, e.g. \`ref_mean + pi\` for the second axial pole,
versus \`final_heading\` which is the wrapped draw; \`NA\` for
\`"uniform"\`), \`track_shape\` (character), \`n_reversals\` (integer),
\`amplitude\` (numeric) and \`line_width\` (numeric). When a \`Tracks\`
is returned, the resolved generating conditions are stored in
\`meta\$sim_conditions\`.

## See also

[`circ_model_select`](https://johnkirwan.github.io/radiatR/reference/circ_model_select.md),
[`test_uniformity`](https://johnkirwan.github.io/radiatR/reference/test_uniformity.md),
[`derive_headings`](https://johnkirwan.github.io/radiatR/reference/derive_headings.md)

## Examples

``` r
sim <- simulate_tracks(seed = 1)
head(sim)
#> # A tibble: 6 × 24
#>   condition trial_id    trial_num frame predictor concentration tortuosity
#>   <chr>     <chr>           <int> <int>     <dbl>         <dbl>      <dbl>
#> 1 control   control_001         1     1    -0.125          7.87     0.0407
#> 2 control   control_001         1     2    -0.125          7.87     0.0407
#> 3 control   control_001         1     3    -0.125          7.87     0.0407
#> 4 control   control_001         1     4    -0.125          7.87     0.0407
#> 5 control   control_001         1     5    -0.125          7.87     0.0407
#> 6 control   control_001         1     6    -0.125          7.87     0.0407
#> # ℹ 17 more variables: ref_heading <dbl>, final_heading <dbl>, rho <dbl>,
#> #   abs_theta <dbl>, rel_theta <dbl>, abs_x <dbl>, abs_y <dbl>, rel_x <dbl>,
#> #   rel_y <dbl>, modality <chr>, n_modes <int>, mode_id <int>, mode_mean <dbl>,
#> #   track_shape <chr>, n_reversals <int>, amplitude <dbl>, line_width <dbl>

# Request both tibble and Tracks representations
sim_both <- simulate_tracks(output = "both", seed = 123)
names(sim_both)
#> [1] "tibble"  "trajset"

example_conditions <- tibble::tibble(
  condition = paste0("level_", 1:4),
  n_trials = 30L,
  ref_mean = seq(-pi/8, pi/8, length.out = 4),
  concentration_base = c(2, 4, 6, 8),
  concentration_slope = 0.8,
  tortuosity_base = c(0.12, 0.09, 0.06, 0.04),
  tortuosity_slope = -0.01,
  tortuosity_sd = 0.015,
  predictor_mean = seq(-1, 1, length.out = 4),
  predictor_sd = 0.15
)
sim_four_levels <- simulate_tracks(
  n_points = 120,
  conditions = example_conditions,
  output = "both",
  seed = 42
)
aggregate(trial_id ~ condition, data = sim_four_levels$tibble, length)
#>   condition trial_id
#> 1   level_1     3600
#> 2   level_2     3600
#> 3   level_3     3600
#> 4   level_4     3600
names(sim_four_levels)
#> [1] "tibble"  "trajset"
```
