# Render pre-computed circular mean arrows on a radial plot

Takes a data frame produced by \[compute_circ_mean()\] and renders each
row as a \`geom_segment()\` arrow. \`mean_dir\` must be in unit-circle
convention (0 = East, CCW), as returned by \[compute_circ_mean()\]. Rows
where \`mean_dir\` or \`resultant_R\` is \`NA\` are silently skipped.

## Usage

``` r
add_circ_mean(
  summary_df,
  colour_col = NULL,
  linewidth = 1,
  colour = NULL,
  arrow_length_cm = 0.2,
  ...
)
```

## Arguments

- summary_df:

  Data frame with columns \`mean_dir\` (UC radians, 0 to 2pi) and
  \`resultant_R\` (0–1). Typically the output of
  \[compute_circ_mean()\].

- colour_col:

  Optional. Name of a column in \`summary_df\` to map to the colour
  aesthetic. Ignored when \`colour\` is also supplied.

- linewidth:

  Line width of the arrow segment. Default \`1\`.

- colour:

  Fixed colour. When \`NULL\` (default) and \`colour_col\` is set,
  colour is mapped from that column; when \`NULL\` and no
  \`colour_col\`, draws in \`"black"\`. Supplying any colour string
  always overrides \`colour_col\`.

- arrow_length_cm:

  Arrowhead length in cm. Default \`0.2\`.

- ...:

  Additional arguments forwarded to \`geom_segment\` (e.g. \`linetype\`,
  \`alpha\`, or a custom \`arrow\` spec that overrides the default).

## Value

A \`geom_segment()\` layer.

## See also

\[compute_circ_mean()\], \[add_heading_arrow()\]
