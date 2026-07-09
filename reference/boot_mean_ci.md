# Bootstrap confidence interval for a mean direction

Computes a nonparametric bootstrap confidence interval for the mean
direction of each group, resampling angles with replacement and taking
the percentiles of the bootstrap mean directions (centred on the sample
mean). Unlike the normal-approximation interval from
[`vonmises_fit`](https://johnkirwan.github.io/radiatR/reference/vonmises_fit.md),
it makes no distributional assumption and stays usable at low
concentration or small sample size, where that approximation is
unreliable.

## Usage

``` r
boot_mean_ci(
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
  mod-pi): `mu`/`mu_deg` are the mean **axis** in \\\[0, \pi)\\ and the
  interval is scaled accordingly. Default `FALSE` (directional).

## Value

Data frame with columns `group_col` (if supplied), `mu` (mean direction,
radians), `mu_deg` (degrees), `ci_lo` and `ci_hi` (`conf`-level
bootstrap interval on `mu`, radians; bounds may fall outside \\\[0,
2\pi)\\ to describe an arc), `resultant_R`, and `n`. Groups with fewer
than two finite angles yield an all-`NA` interval.

## See also

[`vonmises_fit`](https://johnkirwan.github.io/radiatR/reference/vonmises_fit.md),
[`circ_dispersion`](https://johnkirwan.github.io/radiatR/reference/circ_dispersion.md)
