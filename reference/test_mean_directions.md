# Test whether groups share the same mean direction

Two tests of equal mean direction across groups:

- `method = "watson_williams"` (default):

  [`watson.williams.test`](https://rdrr.io/pkg/circular/man/watson.williams.test.html)
  – the circular analogue of the parametric *F*-test for equal means.
  Assumes von Mises-distributed data with equal concentrations across
  groups.

- `method = "permutation"`:

  A distribution-free permutation test using a between-/within-group
  resultant-length statistic, with the p-value obtained by shuffling
  group labels. Makes no von Mises assumption and is robust when
  Watson-Williams' *F* calibration is doubtful (moderate concentration,
  non-von-Mises shape). Note it assumes exchangeability under the null,
  so like Watson-Williams it is most trustworthy when group
  concentrations are similar.

## Usage

``` r
test_mean_directions(
  hd,
  group_col,
  angle_col = "heading",
  method = c("watson_williams", "permutation"),
  pairwise = FALSE,
  p_adjust = "none",
  axial = FALSE,
  n_perm = 9999L
)
```

## Arguments

- hd:

  Data frame with heading and group columns.

- group_col:

  Column identifying conditions or groups.

- angle_col:

  Heading column in radians. Default `"heading"`.

- method:

  One of `"watson_williams"` (default) or `"permutation"`.

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

- n_perm:

  Number of label permutations for `method = "permutation"`. Default
  `9999`. Ignored otherwise. Set the RNG seed with
  [`set.seed`](https://rdrr.io/r/base/Random.html) for reproducible
  p-values.

## Value

Tidy data frame. Omnibus result has columns `n_groups`, `statistic`,
`df1`, `df2`, `p_value`, `test` (`df1`/`df2` are `NA` for the
permutation test). Pairwise result additionally has `group1`, `group2`,
and `p_value_adj` (when `p_adjust != "none"`).

## See also

[`test_distributions`](https://johnkirwan.github.io/radiatR/reference/test_distributions.md),
[`test_concentration`](https://johnkirwan.github.io/radiatR/reference/test_concentration.md)
