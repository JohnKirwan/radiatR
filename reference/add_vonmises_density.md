# Overlay a fitted von Mises density curve on a radiate plot

Evaluates the von Mises probability density on a fine angular grid and
draws it as a closed polygon in the same Cartesian coordinate space used
by
[`radiate`](https://johnkirwan.github.io/radiatR/reference/radiate.md)
and
[`add_angle_rose`](https://johnkirwan.github.io/radiatR/reference/add_angle_rose.md).
The curve peaks at `scale` so the two layers align when given matching
`scale` values.

## Usage

``` r
add_vonmises_density(
  fit,
  scale = 0.4,
  inner_r = 0,
  group_col = NULL,
  n_pts = 360L,
  colour = "steelblue",
  linewidth = 0.8,
  fill = NA,
  alpha = 0.8,
  display = NULL
)
```

## Arguments

- fit:

  Data frame from
  [`vonmises_fit`](https://johnkirwan.github.io/radiatR/reference/vonmises_fit.md),
  containing at least `mu` and `kappa` columns.

- scale:

  Maximum outer radius as a fraction of the unit circle. Default `0.4`
  matches `add_angle_rose`.

- inner_r:

  Inner radius; default `0`.

- group_col:

  Column in `fit` for faceting; must match the `panel_by` argument of
  the parent
  [`radiate()`](https://johnkirwan.github.io/radiatR/reference/radiate.md)
  call.

- n_pts:

  Angular evaluation points. Default `360L`.

- colour:

  Outline colour. Default `"steelblue"`.

- linewidth:

  Outline width. Default `0.8`.

- fill:

  Fill colour. Default `NA` (outline only).

- alpha:

  Opacity. Default `0.8`.

- display:

  A \`circ_display()\` controlling rotation, matching the parent
  \`radiate()\` plot. Default \`NULL\` uses the input's \`display\`
  attribute when present, otherwise the identity display.

## Value

A `geom_polygon` layer, or `NULL` if `fit` is all `NA`.
