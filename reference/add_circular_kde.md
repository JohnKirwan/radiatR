# Overlay a non-parametric circular kernel density estimate on a radiate plot

Estimates the circular density using
[`density.circular`](https://rdrr.io/pkg/circular/man/density.circular.html)
(no distributional assumptions) and draws it as a closed polygon in the
same Cartesian coordinate space used by
[`radiate`](https://johnkirwan.github.io/radiatR/reference/radiate.md)
and
[`add_angle_rose`](https://johnkirwan.github.io/radiatR/reference/add_angle_rose.md).
Unlike
[`add_vonmises_density`](https://johnkirwan.github.io/radiatR/reference/add_vonmises_density.md),
this makes no assumption about the shape of the distribution and handles
multimodal data naturally.

## Usage

``` r
add_circular_kde(
  hd,
  angle_col = "heading",
  group_col = NULL,
  bw = NULL,
  scale = 0.4,
  inner_r = 0,
  n_pts = 512L,
  kernel = "vonmises",
  colour = "tomato",
  linewidth = 0.8,
  fill = NA,
  alpha = 0.8,
  display = NULL
)
```

## Arguments

- hd:

  Data frame with a heading column in radians.

- angle_col:

  Column name. Default `"heading"`.

- group_col:

  Column for faceting; must match `panel_by` in the parent
  [`radiate()`](https://johnkirwan.github.io/radiatR/reference/radiate.md)
  call.

- bw:

  Bandwidth (concentration). `NULL` uses `bw.nrd.circular` automatic
  selection.

- scale:

  Maximum outer radius as a fraction of the unit circle. Default `0.4`.

- inner_r:

  Inner radius. Default `0`.

- n_pts:

  Number of evaluation points. Default `512L`.

- kernel:

  Kernel name passed to `density.circular`. Default `"vonmises"` (the
  kernel shape, not a model assumption).

- colour:

  Outline colour. Default `"tomato"`.

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

A `geom_polygon` layer, or `NULL` if estimation fails.

## Details

The `bw` parameter is a *concentration* parameter (analogous to
\\\kappa\\ of the von Mises kernel) – larger values produce a sharper,
data-following estimate; smaller values over-smooth towards uniform.
`NULL` (default) selects bandwidth automatically via
[`bw.nrd.circular`](https://rdrr.io/pkg/circular/man/bw.circular.html).
