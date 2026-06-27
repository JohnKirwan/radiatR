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
  facets = NULL,
  group_col = NULL,
  stat = c("bootstrap_ci", "sd"),
  boot_reps = 1000L,
  boot_alpha = 0.05,
  axial = FALSE
)
```

## Arguments

- headings_df:

  Data frame containing heading angles.

- heading_col:

  Name of the heading column (radians). Default \`"heading"\`.

- facets:

  Character vector of column names used for faceting (placement). One
  row is returned per occupied combination of \`facets\` and
  \`group_col\`. Each column is preserved in the output so
  \`facet_grid()\`/\`facet_wrap()\` can place the layer correctly.
  Default \`NULL\`.

- group_col:

  Single column name used for colour grouping. When set, one row is
  returned per group and the column is preserved in the output for
  colour mapping in \[add_circ_interval()\]. Default \`NULL\`.

- stat:

  Statistic: \`"bootstrap_ci"\` (default) or \`"sd"\`.

- boot_reps:

  Integer. Bootstrap replicates for \`stat = "bootstrap_ci"\`. Default
  \`1000L\`. Ignored when \`stat = "sd"\`.

- boot_alpha:

  Significance level for the bootstrap CI. Default \`0.05\` produces a
  95% interval.

- axial:

  Logical. Treat the angles as axial (bidirectional, mod-pi) data: the
  interval is computed via the angle-doubling method and \`mean_dir\` is
  reported as an axis in \`\[0, pi)\`, with the endpoints scaled
  accordingly. Default \`FALSE\` (ordinary directional data).

## Value

A data frame with columns \`mean_dir\`, \`lower\`, \`upper\` (radians,
\`\[-pi, pi\]\`), and \`wraps\` (logical, \`TRUE\` when the arc crosses
the +/-pi discontinuity). \`lower\` and \`upper\` are \`NA\` when \`n \<
3\`.

## See also

\[add_circ_interval()\], \[add_heading_interval()\]
