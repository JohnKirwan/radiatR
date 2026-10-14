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

## Examples

``` r
hd <- derive_headings(cpunctatus, rule = "distal", carry = "arc")
set.seed(1)
boot_mean_ci(hd, group_col = "arc", R = 199)
#>   arc        mu    mu_deg      ci_lo    ci_hi resultant_R  n
#> 1  10 1.3766939  78.87875 -1.4921571 3.812127  0.07055967 29
#> 2  15 0.7363627  42.19048 -0.6126016 1.996328  0.24477975 30
#> 3  20 3.8069634 218.12293  1.3390950 6.357445  0.07326231 27
#> 4  30 5.9212227 339.26107  4.3594085 8.083512  0.12936762 37
#> 5  40 3.5642725 204.21777  0.6403791 6.238911  0.06536414 20
#> 6  50 1.5873930  90.95092 -0.3378658 4.424066  0.15790182 26
#> 7   5 2.4205480 138.68719 -0.4536937 5.257315  0.07181952 31
#> 8   0 3.0747577 176.17064  1.4943839 4.453216  0.23531457 35
```
