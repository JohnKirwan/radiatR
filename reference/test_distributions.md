# Test whether groups share the same circular distribution

Compares the angular distributions of two or more groups. Complements
[`test_mean_directions`](https://johnkirwan.github.io/radiatR/reference/test_mean_directions.md)
(which tests only the mean direction) and
[`test_concentration`](https://johnkirwan.github.io/radiatR/reference/test_concentration.md)
(which tests only the spread) by testing the distributions as a whole,
or – for `method = "rao"` – the mean and the dispersion jointly.

## Usage

``` r
test_distributions(
  hd,
  group_col,
  angle_col = "heading",
  method = c("watson_wheeler", "watson_two", "rao"),
  pairwise = FALSE,
  p_adjust = "none",
  axial = FALSE,
  p_method = c("table", "monte_carlo"),
  n_perm = 9999L
)
```

## Arguments

- hd:

  Data frame with heading and group columns.

- group_col:

  Column identifying the groups to compare.

- angle_col:

  Heading column in radians. Default `"heading"`.

- method:

  One of `"watson_wheeler"` (default), `"watson_two"`, or `"rao"`.

- pairwise:

  Logical. `FALSE` (default) runs a single omnibus test across all
  groups (`"watson_two"` then requires exactly two groups). `TRUE` runs
  every pairwise comparison.

- p_adjust:

  Multiple-comparison correction passed to
  [`p.adjust`](https://rdrr.io/r/stats/p.adjust.html). Default `"none"`.
  Applies only to the pairwise output, where a `p_value_adj` column is
  added; `"BH"` or `"holm"` are recommended. Meaningless for the tabled
  `"watson_two"` p-values.

- axial:

  Logical. Treat the angles as axial (bidirectional, mod-pi) data: the
  test is run via the angle-doubling method. Default `FALSE`.

- p_method:

  For `method = "watson_two"` only: `"table"` (default) reports the
  tabled significance band; `"monte_carlo"` instead returns a continuous
  permutation p-value from `n_perm` label shuffles. Ignored by the other
  methods (which already give continuous p-values).

- n_perm:

  Number of label permutations for `p_method = "monte_carlo"`. Default
  `9999`. Set the RNG seed with
  [`set.seed`](https://rdrr.io/r/base/Random.html) for reproducible
  p-values.

## Value

A tidy data frame. Omnibus output has columns `method`, `component`,
`statistic`, `df`, `p_value`, `n_groups`, `n`. Pairwise output replaces
`n_groups` with `group1`/`group2` and adds `p_value_adj` when
`p_adjust != "none"`.

## Details

- `"watson_wheeler"` (default):

  Mardia-Watson-Wheeler uniform-scores test: a non-parametric k-sample
  test that makes no von Mises assumption. Returns an exact
  (chi-squared) p-value.

- `"watson_two"`:

  Watson's two-sample \\U^2\\ test of homogeneity. Strictly two-sample:
  with more than two groups set `pairwise = TRUE`. The `circular`
  package returns only tabled critical values, so by default `p_value`
  is the upper edge of a tabled band (e.g. `0.05` means \\0.01 \< p \<
  0.05\\) and is `NA` when \\p \> 0.10\\. Set `p_method = "monte_carlo"`
  for a continuous permutation p-value.

- `"rao"`:

  Rao's test for homogeneity of both mean directions and dispersions
  across k groups; returns two rows per comparison, one for each
  `component` (`"mean"`, `"dispersion"`).

## See also

[`test_mean_directions`](https://johnkirwan.github.io/radiatR/reference/test_mean_directions.md),
[`test_concentration`](https://johnkirwan.github.io/radiatR/reference/test_concentration.md),
[`test_uniformity`](https://johnkirwan.github.io/radiatR/reference/test_uniformity.md)
