# Fit a von Mises distribution to per-group heading data

Estimates the mean direction \\\mu\\ and concentration \\\kappa\\ of a
von Mises distribution via maximum likelihood, together with asymptotic
standard errors and a confidence interval on \\\mu\\. Intended as a
parametric companion to
[`circ_dispersion`](https://johnkirwan.github.io/radiatR/reference/circ_dispersion.md):
where `circ_dispersion` returns the empirical resultant length *R* and
circular SD, `vonmises_fit` returns the MLE \\\hat{\kappa}\\ with its
uncertainty.

## Usage

``` r
vonmises_fit(
  hd,
  group_col = NULL,
  angle_col = "heading",
  conf = 0.95,
  axial = FALSE
)
```

## Arguments

- hd:

  Data frame containing headings in radians.

- group_col:

  Column(s) to group by. `NULL` fits a single model to all rows.

- angle_col:

  Name of the heading column. Default `"heading"`.

- conf:

  Confidence level for the interval on \\\mu\\. Default `0.95`.

- axial:

  Logical; when \`TRUE\`, fit an axial (bidirectional, mod-pi) von Mises
  via the doubled-angle method: \`mu\`/\`mu_deg\` are the mean
  \*\*axis\*\* in \[0, pi), \`kappa\` is the concentration about that
  axis (estimated in the doubled-angle frame), and
  \`se_mu\`/\`ci_lo\`/\`ci_hi\` are halved accordingly. Default
  \`FALSE\` (directional).

## Value

Data frame with columns `group_col` (if supplied), `mu` (MLE mean
direction, radians), `mu_deg` (degrees), `kappa` (MLE concentration),
`se_mu`, `se_kappa` (asymptotic standard errors), `ci_lo` and `ci_hi`
(`conf`-level interval on \\\mu\\, radians), `n`.

## Details

\\\kappa = 0\\ corresponds to a uniform distribution (no preferred
direction); larger values indicate increasing concentration. The
confidence interval on \\\mu\\ uses a normal approximation and is
unreliable for \\\kappa \< 0.5\\ or small samples.

## Examples

``` r
hd <- derive_headings(cpunctatus, rule = "distal", carry = "arc")
vonmises_fit(hd, group_col = "arc")
#>   arc         mu     mu_deg     kappa     se_mu  se_kappa      ci_lo    ci_hi
#> 1  10  1.3766939   78.87875 0.1414721 1.8586062 0.2635984 -2.2661073 5.019495
#> 2  15  0.7363627   42.19048 0.5049583 0.5193281 0.2705529 -0.2815015 1.754227
#> 3  20 -2.4762219 -141.87707 0.1469196 1.8549744 0.2732671 -6.1119050 1.159461
#> 4  30 -0.3619626  -20.73893 0.2609305 0.8947965 0.2354640 -2.1157316 1.391806
#> 5  40 -2.7189128 -155.78223 0.1310085 2.4163809 0.3172455 -7.4549324 2.017107
#> 6  50  1.5873930   90.95092 0.3198224 0.8727033 0.2826711 -0.1230741 3.297860
#> 7   5  2.4205480  138.68719 0.1440111 1.7660376 0.2549880 -1.0408221 5.881918
#> 8   0  3.0747577  176.17064 0.4842605 0.5007446 0.2495643  2.0933164 4.056199
#>    n
#> 1 29
#> 2 30
#> 3 27
#> 4 37
#> 5 20
#> 6 26
#> 7 31
#> 8 35
```
