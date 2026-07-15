# Goodness-of-fit test against a wrapped Cauchy distribution

Tests whether each group's headings plausibly come from a wrapped Cauchy
distribution, using Watson's U^2 statistic on the probability-integral
transform of the fitted distribution, with a parametric-bootstrap
p-value (the bootstrap accounts for the mean direction and concentration
having been estimated from the same sample, which invalidates the naive
tabled p-value for `watson.test()`). Unlike
[`test_uniformity`](https://johnkirwan.github.io/radiatR/reference/test_uniformity.md),
which tests "is this uniform?", `test_gof` tests a different null
hypothesis: "does this fit a wrapped Cauchy?" — useful when a unimodal
but non-von-Mises shape (heavier or lighter tails) is suspected.

## Usage

``` r
test_gof(
  hd,
  group_col = NULL,
  angle_col = "heading",
  dist = c("wrappedcauchy"),
  p_adjust = "none",
  n_boot = 999L
)
```

## Arguments

- hd:

  Data frame with a heading column in radians.

- group_col:

  Column to group by. `NULL` tests the whole data frame as one group.

- angle_col:

  Heading column name. Default `"heading"`.

- dist:

  Candidate distribution family. Currently only `"wrappedcauchy"` is
  implemented.

- p_adjust:

  Multiple-comparison correction method passed to
  [`p.adjust`](https://rdrr.io/r/stats/p.adjust.html). Default `"none"`.
  Applies only when `group_col` is supplied; a `p_value_adj` column is
  added to the result.

- n_boot:

  Number of parametric bootstrap replicates for the p-value. Default
  `999`. Each replicate refits the wrapped-Cauchy MLE, so this is more
  expensive per-replicate than the package's other Monte-Carlo p-values;
  lower it for quick exploratory calls. Set the RNG seed with
  [`set.seed`](https://rdrr.io/r/base/Random.html) for reproducible
  p-values.

## Value

Tidy data frame with columns `group_col` (if supplied), `statistic`,
`p_value`, `n`, `mu` (fitted mean direction, radians), `rho` (fitted
concentration, `[0, 1)`), `test`, and `p_value_adj` (when
`p_adjust != "none"`). Returns `NULL` (ungrouped) or omits a group's row
(grouped) when that sample has fewer than 3 finite angles or the fit
fails to converge. If the wrapped-Cauchy fit succeeds but the bootstrap
step itself fails, the row is kept with valid `statistic`/`mu`/`rho` but
`p_value` set to `NA`.
