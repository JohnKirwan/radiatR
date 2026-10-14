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

## Examples

``` r
hd <- derive_headings(cpunctatus, rule = "distal", carry = "arc")
set.seed(1)
boot_kappa_ci(hd, group_col = "arc", R = 199)
#>   arc     kappa kappa_ci_lo kappa_ci_hi kappa_bias resultant_R    R_ci_lo
#> 1  10 0.1414721  0.07297546   0.8233801 0.24019287  0.07055967 0.03646332
#> 2  15 0.5049583  0.13689467   1.1292648 0.06859720  0.24477975 0.06828717
#> 3  20 0.1469196  0.04620455   0.7923231 0.22601901  0.07326231 0.02309611
#> 4  30 0.2609305  0.08182301   0.7906745 0.13712105  0.12936762 0.04087731
#> 5  40 0.1310085  0.07051089   0.8753393 0.27263171  0.06536414 0.03523355
#> 6  50 0.3198224  0.11040234   1.1019951 0.16021242  0.15790182 0.05511723
#> 7   5 0.1440111  0.05746202   0.7538006 0.19788764  0.07181952 0.02871915
#> 8   0 0.4842605  0.12024121   1.0191277 0.05178532  0.23531457 0.06001221
#>     R_ci_hi     R_bias  n
#> 1 0.3807554 0.11379845 29
#> 2 0.4927208 0.02546300 30
#> 3 0.3683435 0.10747749 27
#> 4 0.3676731 0.06323782 37
#> 5 0.4010783 0.12871590 20
#> 6 0.4834778 0.07038950 26
#> 7 0.3526907 0.09451140 31
#> 8 0.4545285 0.01821682 35
```
