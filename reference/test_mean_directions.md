# Test whether groups share the same mean direction (Watson-Williams)

Wraps
[`watson.williams.test`](https://rdrr.io/pkg/circular/man/watson.williams.test.html)
– the circular analogue of the parametric *F*-test for equal means.
Assumes von Mises- distributed data with equal concentrations across
groups; if concentrations differ substantially or the distribution is
non-von-Mises, consider a non-parametric alternative.

## Usage

``` r
test_mean_directions(
  hd,
  group_col,
  angle_col = "heading",
  pairwise = FALSE,
  p_adjust = "none",
  axial = FALSE
)
```

## Arguments

- hd:

  Data frame with heading and group columns.

- group_col:

  Column identifying conditions or groups.

- angle_col:

  Heading column in radians. Default `"heading"`.

- pairwise:

  Logical. `FALSE` (default) returns a single omnibus test across all
  groups. `TRUE` returns all pairwise comparisons.

- p_adjust:

  Multiple-comparison correction method passed to
  [`p.adjust`](https://rdrr.io/r/stats/p.adjust.html). Default `"none"`.
  Applies only to the pairwise output; a `p_value_adj` column is added.
  Strongly recommended when `pairwise = TRUE`: use `"BH"`
  (Benjamini-Hochberg) or `"holm"` (family-wise control). Ignored for
  the omnibus test (single p-value, no adjustment needed).

- axial:

  Logical. Treat the angles as axial (bidirectional, mod-pi) data: the
  test is run via the angle-doubling method, comparing group axes.
  Default \`FALSE\` (ordinary directional data).

## Value

Tidy data frame. Omnibus result has columns `n_groups`, `statistic`,
`df1`, `df2`, `p_value`, `test`. Pairwise result additionally has
`group1`, `group2`, and `p_value_adj` (when `p_adjust != "none"`).
