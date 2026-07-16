# Fit a wrapped Cauchy distribution to per-group heading data

Estimates the mean direction \\\mu\\ and concentration \\\rho\\ of a
wrapped Cauchy distribution via maximum likelihood. The wrapped Cauchy
has heavier tails than the von Mises and is more appropriate for data
with outliers, weak or noisy directionality, or when a von Mises fit
looks visually poor on a rose diagram.

## Usage

``` r
wrappedcauchy_fit(hd, group_col = NULL, angle_col = "heading", axial = FALSE)
```

## Arguments

- hd:

  Data frame containing headings in radians.

- group_col:

  Column(s) to group by. `NULL` fits a single model.

- angle_col:

  Name of the heading column. Default `"heading"`.

- axial:

  Logical; when \`TRUE\`, fit an axial (bidirectional, mod-pi) wrapped
  Cauchy via the doubled-angle method: \`mu\`/\`mu_deg\` are the mean
  \*\*axis\*\* in \[0, pi) and \`rho\` is the concentration about that
  axis (estimated in the doubled-angle frame). Default \`FALSE\`
  (directional).

## Value

Data frame with columns `group_col` (if supplied), `mu` (MLE mean
direction, radians), `mu_deg` (degrees), `rho` (concentration, 0–1),
`convergence` (`0` = converged, `1` = not converged; `NA` when the fit
could not be attempted), `n`.

## Details

\\\rho = 0\\ is a uniform distribution (no preferred direction); \\\rho
= 1\\ is a point mass (perfect concentration). Unlike von Mises
\\\kappa\\, the wrapped Cauchy \\\rho\\ is bounded to \\\[0, 1)\\.

Standard errors are not computed by `mle.wrappedcauchy`; check the
`convergence` flag (`0` = the fixed-point MLE iteration converged; `1` =
it did not, so treat that row's estimates with caution). For uncertainty
estimation use
[`vonmises_fit`](https://johnkirwan.github.io/radiatR/reference/vonmises_fit.md)
with the same data and compare model fits visually via
[`add_vonmises_density`](https://johnkirwan.github.io/radiatR/reference/add_vonmises_density.md)
and
[`add_wrappedcauchy_density`](https://johnkirwan.github.io/radiatR/reference/add_wrappedcauchy_density.md).

## See also

[`vonmises_fit`](https://johnkirwan.github.io/radiatR/reference/vonmises_fit.md),
[`add_wrappedcauchy_density`](https://johnkirwan.github.io/radiatR/reference/add_wrappedcauchy_density.md)
