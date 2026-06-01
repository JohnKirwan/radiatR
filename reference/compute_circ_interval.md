# Compute a circular interval arc from heading angles

Returns a data frame of arc bounds centred on the circular mean
direction. Two built-in statistics are available: a bootstrap confidence
interval for the mean direction (\`"bootstrap_ci"\`, via
\[circular::mle.vonmises.bootstrap.ci()\]) and +/-1 circular SD
(\`"sd"\`, via \[circular::sd.circular()\]). The \`lower\` and \`upper\`
columns of the output can be replaced with Bayesian credible interval
bounds from any model before passing to \[add_circ_interval()\].

## Usage

``` r
compute_circ_interval(
  headings_df,
  heading_col = "heading",
  colour_col = NULL,
  angle_convention = NULL,
  coords = NULL,
  stat = c("bootstrap_ci", "sd"),
  boot_reps = 1000L,
  boot_alpha = 0.05
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

## Value

A data frame with columns \`mean_dir\`, \`lower\`, \`upper\` (radians,
\`\[-pi, pi\]\`), and \`wraps\` (logical, \`TRUE\` when the arc crosses
the +/-pi discontinuity). \`lower\` and \`upper\` are \`NA\` when \`n \<
3\`.

## See also

\[add_circ_interval()\], \[add_heading_interval()\]
