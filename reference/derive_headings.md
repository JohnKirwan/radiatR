# Derive heading angle(s) from trajectories using specified rule

Derive heading angle(s) from trajectories using specified rule

## Usage

``` r
derive_headings(
  x,
  rule = c("crossing", "distal", "straight", "origin_mean", "net", "velocity_mean",
    "window_net", "goal_bias", "pca_axis", "ransac_straight", "maxspeed_window",
    "vm_fit", "exit", "entry", "ring_tangent"),
  ...,
  coords = c("absolute", "relative"),
  angle_convention = c("clock", "unit_circle")
)

# S4 method for class 'TrajSet'
derive_headings(
  x,
  rule = c("crossing", "distal", "straight", "origin_mean", "net", "velocity_mean",
    "window_net", "goal_bias", "pca_axis", "ransac_straight", "maxspeed_window",
    "vm_fit", "exit", "entry", "ring_tangent"),
  ...,
  first_only = FALSE,
  carry = NULL,
  coords = c("absolute", "relative"),
  angle_convention = c("clock", "unit_circle")
)
```

## Arguments

- x:

  TrajSet

- rule:

  one of "crossing", "distal", "straight"

- ...:

  rule-specific parameters

- coords:

  Character. Which Cartesian columns to use: \`"absolute"\` (default,
  uses \`x\`/\`y\` from \`TrajSet@cols\`) or \`"relative"\` (uses
  \`rel_x\`/\`rel_y\`; errors if not registered).

- angle_convention:

  Character. Output angle convention: \`"clock"\` (default; 0 =
  North/top, clockwise) or \`"unit_circle"\` (0 = East,
  counterclockwise). The returned data frame carries \`attr(result,
  "angle_convention")\` and \`attr(result, "coords")\` for downstream
  auto-detection by \[circ_summary_headings()\] and
  \[circ_mean_segments()\].

- first_only:

  logical; if TRUE, return only the first matching heading per
  trajectory

- carry:

  optional character vector of columns from the source data to append
  via nearest time

## Value

data.frame with columns id, time (approx), heading (radians). For some
rules there may be multiple headings per id.
