# Per-group tests of circular uniformity

Tests whether each group's headings are uniformly distributed (i.e. no
preferred direction), using any of four classical tests. The Rayleigh
test (`"rayleigh"`, default) returns an exact numeric p-value; the other
three (`"kuiper"`, `"rao"`, `"watson"`) use look-up tables and the
`p_value` column contains the tabled significance level rather than a
continuous p-value.

## Usage

``` r
test_uniformity(
  hd,
  group_col = NULL,
  angle_col = "heading",
  test = c("rayleigh", "kuiper", "rao", "watson"),
  p_adjust = "none",
  axial = FALSE
)
```

## Arguments

- hd:

  Data frame with a heading column in radians.

- group_col:

  Column to group by. `NULL` tests the whole data frame as one group.

- angle_col:

  Heading column name. Default `"heading"`.

- test:

  One of `"rayleigh"` (default), `"kuiper"`, `"rao"`, or `"watson"`.

- p_adjust:

  Multiple-comparison correction method passed to
  [`p.adjust`](https://rdrr.io/r/stats/p.adjust.html). Default `"none"`.
  Applies only when `group_col` is supplied; a `p_value_adj` column is
  added to the result. Recommended: `"BH"` (Benjamini-Hochberg) when
  testing many conditions. Only meaningful for the Rayleigh test (exact
  p-values).

- axial:

  Logical. Treat the angles as axial (bidirectional, mod-pi) data: the
  uniformity test is run via the angle-doubling method (testing for an
  axis). Default \`FALSE\` (ordinary directional data).

## Value

Tidy data frame with columns `group_col` (if supplied), `statistic`,
`p_value`, `n`, `test`, and `p_value_adj` (when `p_adjust != "none"`).
