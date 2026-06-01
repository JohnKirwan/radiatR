# Overlay a fitted wrapped Cauchy density curve on a radiate plot

Evaluates
[`dwrappedcauchy`](https://rdrr.io/pkg/circular/man/wrappedcauchy.html)
on a fine angular grid and draws it as a closed polygon in the same
Cartesian space as
[`radiate`](https://johnkirwan.github.io/radiatR/reference/radiate.md).
Intended as a visual companion to
[`add_vonmises_density`](https://johnkirwan.github.io/radiatR/reference/add_vonmises_density.md):
overlaying both curves shows whether the data favour the lighter-tailed
von Mises or the heavier-tailed wrapped Cauchy.

## Usage

``` r
add_wrappedcauchy_density(
  fit,
  scale = 0.4,
  inner_r = 0,
  group_col = NULL,
  n_pts = 360L,
  colour = "darkorange",
  linewidth = 0.8,
  fill = NA,
  alpha = 0.8
)
```

## Arguments

- fit:

  Data frame from
  [`wrappedcauchy_fit`](https://johnkirwan.github.io/radiatR/reference/wrappedcauchy_fit.md),
  containing at least `mu` and `rho` columns.

- scale:

  Maximum outer radius as a fraction of the unit circle. Default `0.4`.

- inner_r:

  Inner radius. Default `0`.

- group_col:

  Column for faceting; must match `panel_by` in the parent
  [`radiate()`](https://johnkirwan.github.io/radiatR/reference/radiate.md)
  call.

- n_pts:

  Angular evaluation points. Default `360L`.

- colour:

  Outline colour. Default `"darkorange"`.

- linewidth:

  Outline width. Default `0.8`.

- fill:

  Fill colour. Default `NA` (outline only).

- alpha:

  Opacity. Default `0.8`.

## Value

A `geom_polygon` layer, or `NULL` if estimation fails.

## Details

Default colour is `"darkorange"` to distinguish from
`add_vonmises_density` (`"steelblue"`) and `add_circular_kde`
(`"tomato"`).

## See also

[`add_vonmises_density`](https://johnkirwan.github.io/radiatR/reference/add_vonmises_density.md),
[`add_circular_kde`](https://johnkirwan.github.io/radiatR/reference/add_circular_kde.md)
