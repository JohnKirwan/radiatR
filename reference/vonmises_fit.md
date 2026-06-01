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
vonmises_fit(hd, group_col = NULL, angle_col = "heading", conf = 0.95)
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
