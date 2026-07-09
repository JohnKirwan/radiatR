# Bootstrap confidence interval for a between-condition concentration contrast

Contrasts the concentration of two (or more) groups via a stratified
nonparametric bootstrap: angles are resampled with replacement within
each group independently, and the contrast is recomputed on every
resample. For a pair of groups it reports the difference in von Mises
concentration (\\\Delta\kappa\\), the concentration ratio
(\\\kappa_1/\kappa_2\\), and the difference in mean resultant length
(\\\Delta R\\), each with a percentile confidence interval, plus a
two-sided bootstrap p-value for \\\Delta\kappa = 0\\. It is the
assumption-light, estimation-focused companion to
[`test_concentration`](https://johnkirwan.github.io/radiatR/reference/test_concentration.md)
(parametric `equal.kappa` / non-parametric Wallraff).

## Usage

``` r
boot_kappa_contrast(
  hd,
  group_col,
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

  Grouping column. Required; must have at least two distinct values.
  With exactly two groups a single contrast row is returned; with more
  than two, every unordered pair is returned.

- angle_col:

  Name of the heading column. Default `"heading"`.

- conf:

  Confidence level. Default `0.95`.

- R:

  Number of bootstrap resamples. Default `999`. Set the RNG seed with
  [`set.seed`](https://rdrr.io/r/base/Random.html) for reproducible
  intervals.

- axial:

  Logical; when `TRUE`, treat the angles as axial and estimate
  concentration in the doubled-angle frame (see
  [`boot_kappa_ci`](https://johnkirwan.github.io/radiatR/reference/boot_kappa_ci.md)).
  Default `FALSE`.

## Value

Data frame with one row per group pair: `group1`, `group2`,
`delta_kappa`, `delta_kappa_ci_lo`/`delta_kappa_ci_hi`, `kappa_ratio`,
`kappa_ratio_ci_lo`/`kappa_ratio_ci_hi`, `delta_R`,
`delta_R_ci_lo`/`delta_R_ci_hi`, `p_value` (two-sided bootstrap p for
\\\Delta\kappa = 0\\), `n1`, `n2`.

## Details

The concentration ratio is skewed and can be unstable or non-finite when
the denominator concentration is near zero; \\\Delta\kappa\\ is the more
robust default in the low-concentration regime.

## See also

[`boot_kappa_ci`](https://johnkirwan.github.io/radiatR/reference/boot_kappa_ci.md),
[`test_concentration`](https://johnkirwan.github.io/radiatR/reference/test_concentration.md)
