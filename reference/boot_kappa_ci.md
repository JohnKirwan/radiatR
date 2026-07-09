# Bootstrap confidence intervals for circular concentration

Computes nonparametric bootstrap confidence intervals for the
concentration of each group, for both the von Mises maximum-likelihood
concentration \\\kappa\\ and the mean resultant length \\R\\. Angles are
resampled with replacement and the interval is the percentile envelope
of the bootstrap estimates. Unlike the normal-approximation `se_kappa`
from
[`vonmises_fit`](https://johnkirwan.github.io/radiatR/reference/vonmises_fit.md),
it makes no distributional assumption and stays usable at low
concentration or small sample size, where that approximation is
unreliable and where \\\hat\kappa\\/\\\hat R\\ carry a known upward
bias.

## Usage

``` r
boot_kappa_ci(
  hd,
  group_col = NULL,
  angle_col = "heading",
  conf = 0.95,
  R = 999L,
  axial = FALSE
)
```

## Arguments

- hd:

  Data frame containing headings in radians.

- group_col:

  Column(s) to group by. `NULL` (default) treats the whole data frame as
  one group.

- angle_col:

  Name of the heading column. Default `"heading"`.

- conf:

  Confidence level. Default `0.95`.

- R:

  Number of bootstrap resamples. Default `999`. Set the RNG seed with
  [`set.seed`](https://rdrr.io/r/base/Random.html) for reproducible
  intervals.

- axial:

  Logical; when `TRUE`, treat the angles as axial (bidirectional,
  mod-pi): concentration is estimated in the doubled-angle frame (about
  the axis) and, like
  [`vonmises_fit`](https://johnkirwan.github.io/radiatR/reference/vonmises_fit.md),
  is not rescaled. Default `FALSE` (directional).

## Value

Data frame with columns `group_col` (if supplied), `kappa` (von Mises
MLE concentration), `kappa_ci_lo`/`kappa_ci_hi` (`conf`-level percentile
CI on \\\kappa\\), `kappa_bias`, `resultant_R` (mean resultant length in
\\\[0,1\]\\), `R_ci_lo`/`R_ci_hi`, `R_bias`, and `n`. Groups with fewer
than two finite angles yield an all-`NA` row.

## Details

The `kappa_bias`/`R_bias` columns report the bootstrap estimate of that
bias (bootstrap mean minus the sample estimate); the intervals
themselves are the raw percentile envelope and are not bias-shifted.

## See also

[`boot_mean_ci`](https://johnkirwan.github.io/radiatR/reference/boot_mean_ci.md),
[`boot_kappa_contrast`](https://johnkirwan.github.io/radiatR/reference/boot_kappa_contrast.md),
[`vonmises_fit`](https://johnkirwan.github.io/radiatR/reference/vonmises_fit.md),
[`circ_dispersion`](https://johnkirwan.github.io/radiatR/reference/circ_dispersion.md),
[`test_concentration`](https://johnkirwan.github.io/radiatR/reference/test_concentration.md)
