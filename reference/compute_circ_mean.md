# Compute circular mean direction and resultant length from a headings data frame

Computes the circular mean direction and resultant length (R) per group
from a headings data frame, typically the output of
\[derive_headings()\]. \`mean_dir\` in the returned data frame is
\*\*always in unit-circle convention\*\* (0 = East, counterclockwise),
regardless of the input convention, making it suitable for direct use in
\[add_circ_mean()\].

## Usage

``` r
compute_circ_mean(
  headings_df,
  heading_col = "heading",
  facets = NULL,
  group_col = NULL,
  axial = FALSE
)
```

## Arguments

- headings_df:

  Data frame with a column of heading angles in radians.
  \[derive_headings()\] sets \`attr(headings_df, "angle_convention")\`
  and \`attr(headings_df, "coords")\` automatically.

- heading_col:

  Name of the column containing heading angles. Default \`"heading"\`.

- facets:

  Character vector of column names used as faceting variables. One row
  is returned per unique combination of these columns. These columns are
  attached to the output so that \[add_circ_mean()\] can route each
  arrow to the correct facet panel.

- group_col:

  Optional. Name of a single column to group by for colour mapping. One
  row is returned per unique combination of \`c(facets, group_col)\`.
  Map colour in \[add_circ_mean()\] via its \`colour_col\` argument.

- axial:

  Logical. Treat the angles as axial (bidirectional, mod-pi) data:
  \`mean_dir\` is the axis in \`\[0, pi)\` and \`resultant_R\` is the
  axial resultant length, both via the angle-doubling method. Default
  \`FALSE\`.

## Value

A data frame with columns \`mean_dir\` (unit-circle radians, 0 to 2pi),
\`resultant_R\` (0–1), plus any \`facets\`/\`group_col\` columns. Both
statistics are \`NA\` when a cell contains fewer than 2 finite angles.

## See also

\[add_circ_mean()\], \[add_heading_arrow()\]
