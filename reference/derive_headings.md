# Derive heading angle(s) from trajectories using specified rule

Derive heading angle(s) from trajectories using specified rule

## Usage

``` r
derive_headings(
  x,
  rule = c("crossing", "distal", "straight", "origin_mean", "net", "velocity_mean",
    "velocity_axis", "window_net", "goal_bias", "pca_axis", "ransac_straight",
    "maxspeed_window", "vm_fit", "exit", "entry", "ring_tangent"),
  ...,
  coords = c("absolute", "relative")
)

# S4 method for class 'Tracks'
derive_headings(
  x,
  rule = c("crossing", "distal", "straight", "origin_mean", "net", "velocity_mean",
    "velocity_axis", "window_net", "goal_bias", "pca_axis", "ransac_straight",
    "maxspeed_window", "vm_fit", "exit", "entry", "ring_tangent"),
  ...,
  first_only = FALSE,
  carry = NULL,
  on_missing = c("warn", "error", "quiet"),
  coords = c("absolute", "relative")
)
```

## Arguments

- x:

  Tracks

- rule:

  one of "crossing", "distal", "straight"

- ...:

  rule-specific parameters, including \`return_coords\` (see below)

- coords:

  Character. Which Cartesian columns to use: \`"absolute"\` (default,
  uses whatever \`x\`/\`y\` the \`Tracks\` currently holds in
  \`Tracks@cols\` – calibrated only if the caller supplied or normalized
  them that way) or \`"relative"\` (uses \`rel_x\`/\`rel_y\`, the
  landmark-rotated frame; errors if not registered).

- first_only:

  logical; if TRUE, return only the first matching heading per
  trajectory

- carry:

  optional character vector of columns from the source data to append
  via nearest time

- on_missing:

  One of \`"warn"\` (default), \`"error"\`, or \`"quiet"\`, controlling
  what happens when a rule produces no heading (\`NA\`) for one or more
  trials. The \`NA\` rows are always retained; the returned object
  carries \`n_total\`, \`n_missing\`, and \`missing_ids\` attributes.
  \`"warn"\` emits a warning, \`"error"\` stops, \`"quiet"\` is silent.
  Rule-based failures are often non-random (e.g. tracks that never reach
  the circumference) and can bias circular statistics, so they are
  surfaced by default.

## Value

data.frame with columns id, time (approx), heading (radians, unit-circle
convention), plus the rule-specific construction columns above when
\`return_coords = TRUE\`. For some rules there may be multiple headings
per id.

## Details

Passing \`return_coords = TRUE\` (via \`...\`, default \`FALSE\`)
attaches the construction coordinates each rule used to derive the
heading, in the chosen \`coords\` frame: \`crossing\` adds
\`x_inner\`/\`y_inner\`; \`distal\` adds \`x_distal\`/\`y_distal\`;
\`net\` adds \`x_start\`/\`y_start\`/\`x_end\`/\`y_end\`; \`straight\`
adds \`x_seg0\`/\`y_seg0\`/\`x_seg1\`/\`y_seg1\` (the run endpoints);
\`pca_axis\` adds \`x_centroid\`/\`y_centroid\`/\`axis_x\`/\`axis_y\` (a
unit axis vector). Other rules ignore it. The \`distal\` rule accepts
\`max_radius\` (default \`Inf\`): the furthest-point search is
restricted to positions with radius \`\<= max_radius\`, so \`max_radius
= 1\` ignores beyond-circumference (\`rho \> 1\`) tracking outliers.
