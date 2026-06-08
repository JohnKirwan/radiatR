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
  coords = c("absolute", "relative")
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
  coords = c("absolute", "relative")
)
```

## Arguments

- x:

  TrajSet

- rule:

  one of "crossing", "distal", "straight"

- ...:

  rule-specific parameters, including \`return_coords\` (see below)

- coords:

  Character. Which Cartesian columns to use: \`"absolute"\` (default,
  uses \`x\`/\`y\` from \`TrajSet@cols\`) or \`"relative"\` (uses
  \`rel_x\`/\`rel_y\`; errors if not registered).

- first_only:

  logical; if TRUE, return only the first matching heading per
  trajectory

- carry:

  optional character vector of columns from the source data to append
  via nearest time

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
unit axis vector). Other rules ignore it.
