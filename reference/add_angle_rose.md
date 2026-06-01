# Add a rose diagram of heading angles to a radiate plot

Draws angular frequency as filled wedge polygons in the Cartesian
coordinate space used by
[`radiate`](https://johnkirwan.github.io/radiatR/reference/radiate.md).
Each wedge spans one angular bin; its outer radius is proportional to
the proportion (or count) of frames in that bin. The layer can be
faceted by passing the same column used in the parent
`radiate(panel_by = ...)` call.

## Usage

``` r
add_angle_rose(
  hd,
  bins = 12L,
  angle_col = "heading",
  group_col = NULL,
  scale = 0.4,
  inner_r = 0,
  normalize = TRUE,
  fill = "steelblue",
  colour = NA,
  alpha = 0.5,
  arc_pts = 20L
)
```

## Arguments

- hd:

  Data frame of headings, e.g. from
  [`pose_to_headings`](https://johnkirwan.github.io/radiatR/reference/pose_to_headings.md)
  or `derive_headings(..., frame_select = "all")`.

- bins:

  Integer; number of equal angular sectors. Default `12`.

- angle_col:

  Column containing headings in radians. Default `"heading"`.

- group_col:

  Column used for faceting; must match the `panel_by` argument of the
  parent
  [`radiate()`](https://johnkirwan.github.io/radiatR/reference/radiate.md)
  call.

- scale:

  Maximum outer radius of the tallest wedge as a fraction of the unit
  circle radius. Default `0.4`.

- inner_r:

  Inner radius of wedges. Default `0`; set `> 0` for a donut style.

- normalize:

  `TRUE` (default) scales wedges by proportion; `FALSE` uses raw counts.

- fill:

  Wedge fill colour. Default `"steelblue"`.

- colour:

  Wedge border colour. Default `NA` (no border).

- alpha:

  Opacity. Default `0.5`.

- arc_pts:

  Points used to approximate each wedge arc. Default `20L`.

## Value

A `geom_polygon` layer that can be added to a
[`radiate()`](https://johnkirwan.github.io/radiatR/reference/radiate.md)
plot with `+`.
