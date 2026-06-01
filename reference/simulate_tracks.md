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
  phi = 0.85
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
  returns a long-form data frame, "trajset" returns a \[TrajSet\]
  object, and "both" returns a list containing both representations.

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

## Value

Depending on \`output\`, a tibble, a \`TrajSet\`, or a list containing
both. When \`write_path\` is supplied the data are also written to disk.

## Details

The \`conditions\` data frame can contain the following columns
(defaults are supplied when missing):

\- \`condition\` (character): condition label. - \`n_trials\` (integer):
number of trajectories to simulate for the condition. - \`ref_mean\`
(numeric radian): baseline reference heading (default 0). -
\`concentration_base\` (numeric): baseline von Mises concentration
(kappa). - \`concentration_slope\` (numeric): optional slope applied to
the predictor. - \`tortuosity_base\` (numeric): baseline angular noise
scale. - \`tortuosity_slope\` (numeric): slope applied to the
predictor. - \`tortuosity_sd\` (numeric): additional random variation
per trial. - \`predictor_mean\`, \`predictor_sd\` (numeric): parameters
used to sample the per-trial predictor when explicit values are not
supplied. - \`predictor_values\` (list-column): optional explicit
predictor values (length \`n_trials\`) overriding the generated values.

The predictor can represent any continuous covariate (e.g. reference
intensity). The final heading concentration increases with larger kappa,
whereas larger tortuosity values produce more sinuous paths.

## Examples

``` r
sim <- simulate_tracks(seed = 1)
head(sim)
#> # A tibble: 6 × 16
#>   condition trial_id    trial_num frame predictor concentration tortuosity
#>   <chr>     <chr>           <int> <int>     <dbl>         <dbl>      <dbl>
#> 1 control   control_001         1     1    -0.125          7.87     0.0407
#> 2 control   control_001         1     2    -0.125          7.87     0.0407
#> 3 control   control_001         1     3    -0.125          7.87     0.0407
#> 4 control   control_001         1     4    -0.125          7.87     0.0407
#> 5 control   control_001         1     5    -0.125          7.87     0.0407
#> 6 control   control_001         1     6    -0.125          7.87     0.0407
#> # ℹ 9 more variables: ref_heading <dbl>, final_heading <dbl>, rho <dbl>,
#> #   abs_theta <dbl>, rel_theta <dbl>, abs_x <dbl>, abs_y <dbl>, rel_x <dbl>,
#> #   rel_y <dbl>

# Request both tibble and TrajSet representations
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
