# Compute a circular interval arc and add it to a radial plot in one step

Convenience wrapper that calls \[compute_circ_interval()\] followed by
\[add_circ_interval()\]. Use \[compute_circ_interval()\] +
\[add_circ_interval()\] directly when you need to replace
\`lower\`/\`upper\` with Bayesian credible interval bounds before
rendering.

## Usage

``` r
add_heading_interval(
  headings_df,
  heading_col = "heading",
  colour_col = NULL,
  angle_convention = NULL,
  coords = NULL,
  stat = c("bootstrap_ci", "sd"),
  boot_reps = 1000L,
  boot_alpha = 0.05,
  radius = 1.05,
  linewidth = 1.5,
  colour = NULL,
  linetype = "solid",
  n_theta = 500L
)
```

## Arguments

- headings_df:

  Data frame containing heading angles.

- heading_col:

  Name of the heading column (radians). Default \`"heading"\`.

- colour_col:

  Optional grouping column. When set, one row is returned per group and
  the column is preserved in the output.

- angle_convention:

  Angle convention of the heading column: \`"unit_circle"\` (default, 0
  = East CCW) or \`"clock"\` (0 = North CW). If \`NULL\`, read from
  \`attr(headings_df, "angle_convention")\`.

- coords:

  Coordinate system used when \`angle_convention = "clock"\`:
  \`"relative"\` or \`"absolute"\`. If \`NULL\`, read from
  \`attr(headings_df, "coords")\`.

- stat:

  Statistic: \`"bootstrap_ci"\` (default) or \`"sd"\`.

- boot_reps:

  Integer. Bootstrap replicates for \`stat = "bootstrap_ci"\`. Default
  \`1000L\`. Ignored when \`stat = "sd"\`.

- boot_alpha:

  Significance level for the bootstrap CI. Default \`0.05\` produces a
  95% interval.

- radius:

  Radial position of the arc. Default \`1.05\`.

- linewidth:

  Line width. Default \`1.5\`.

- colour:

  Fixed colour. When \`NULL\` (default) and \`colour_col\` is set,
  colour is mapped from that column; when \`NULL\` and no
  \`colour_col\`, draws in \`"black"\`. Supplying any colour string
  always overrides \`colour_col\`.

- linetype:

  Line type. Default \`"solid"\`.

- n_theta:

  Number of points along the arc. Default \`500L\`.

## Value

A \`geom_path()\` layer.

## See also

\[compute_circ_interval()\], \[add_circ_interval()\]
