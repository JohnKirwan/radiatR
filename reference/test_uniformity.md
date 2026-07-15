# Per-group tests of circular uniformity

Tests whether each group's headings are uniformly distributed (i.e. no
preferred direction), using any of four classical tests. The Rayleigh
test (`"rayleigh"`, default) returns an exact numeric p-value; the other
three (`"kuiper"`, `"rao"`, `"watson"`) use look-up tables, so by
default the `p_value` column contains the tabled significance level
rather than a continuous p-value. Set `p_method = "monte_carlo"` to
obtain a continuous p-value for those three by simulation instead
(usable with `p_adjust`).

## Usage

``` r
test_uniformity(
  hd,
  group_col = NULL,
  angle_col = "heading",
  test = c("rayleigh", "vtest", "kuiper", "rao", "watson", "hermans_rasson", "pycke"),
  p_adjust = "none",
  axial = FALSE,
  n_sim = 9999L,
  mu = NULL,
  p_method = c("table", "monte_carlo")
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

  One of `"rayleigh"` (default), `"vtest"`, `"kuiper"`, `"rao"`,
  `"watson"`, `"hermans_rasson"`, or `"pycke"`. `"vtest"` is the V-test:
  uniformity against a *specified* mean direction `mu`, considerably
  more powerful than the Rayleigh test when a bearing is expected a
  priori. The Hermans-Rasson and Pycke omnibus tests are far more
  powerful than Rayleigh against multimodal / non-symmetric
  alternatives; both obtain `p_value` by Monte-Carlo simulation.

- p_adjust:

  Multiple-comparison correction method passed to
  [`p.adjust`](https://rdrr.io/r/stats/p.adjust.html). Default `"none"`.
  Applies only when `group_col` is supplied; a `p_value_adj` column is
  added to the result. Recommended: `"BH"` (Benjamini-Hochberg) when
  testing many conditions. Meaningful only for tests with continuous
  p-values: Rayleigh, the V-test, Hermans-Rasson, Pycke, and any test
  run with `p_method = "monte_carlo"`.

- axial:

  Logical. Treat the angles as axial (bidirectional, mod-pi) data: the
  uniformity test is run via the angle-doubling method (testing for an
  axis). Default \`FALSE\` (ordinary directional data).

- n_sim:

  Number of Monte-Carlo replicates for the `"hermans_rasson"` and
  `"pycke"` p-values and for `p_method = "monte_carlo"`. Default `9999`.
  Set the RNG seed with [`set.seed`](https://rdrr.io/r/base/Random.html)
  for reproducible p-values.

- mu:

  Numeric scalar, radians. The expected mean direction for the V-test
  (`test = "vtest"`); required for that test and ignored otherwise.

- p_method:

  For `"kuiper"`, `"rao"`, and `"watson"` only: `"table"` (default)
  reports the tabled significance level from the `circular` package (or
  `NA` when the statistic falls outside the table); `"monte_carlo"`
  instead simulates `n_sim` uniform samples to obtain a continuous
  p-value. Ignored by the other tests.

## Value

Tidy data frame with columns `group_col` (if supplied), `statistic`,
`p_value`, `n`, `test`, and `p_value_adj` (when `p_adjust != "none"`).

## References

Landler, L., Ruxton, G.D. & Malkemper, E.P. (2019). The Hermans-Rasson
test as a powerful alternative to the Rayleigh test for circular
statistics in biology. BMC Ecology 19:30.
[doi:10.1186/s12898-019-0246-8](https://doi.org/10.1186/s12898-019-0246-8)
.

Pycke, S.-R. (2010). Some tests for uniformity of circular distributions
powered by the multivariate Rayleigh test. Annals of the Institute of
Statistical Mathematics, 62(2), 323-340.
[doi:10.1007/s10463-008-0179-9](https://doi.org/10.1007/s10463-008-0179-9)
.
