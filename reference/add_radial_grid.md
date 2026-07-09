# Radial grid layers (the radial analogue of a Cartesian grid)

Returns a list of layers – an optional filled disc, concentric rings,
and radial spokes, in two weights (major/minor) – to compose onto a
radial \`radiate()\` plot with \`+\`. The unit boundary (radius 1) is
left to the circumference (\[add_circ()\]); grid rings are interior
only.

## Usage

``` r
add_radial_grid(
  rings_major = 0.5,
  rings_minor = c(0.25, 0.75),
  spokes_major = 4L,
  spokes_minor = 4L,
  colour = "grey92",
  colour_minor = NULL,
  linewidth = 0.5,
  linewidth_minor = NULL,
  linetype = "solid",
  disc_fill = NA,
  origin = FALSE,
  origin_colour = "grey50",
  origin_size = 1.5,
  n_pts = 200L,
  color = NULL,
  color_minor = NULL,
  origin_color = NULL
)
```

## Arguments

- rings_major, rings_minor:

  Numeric radii (\`\<1\`) of the major and minor rings. Defaults \`0.5\`
  and \`c(0.25, 0.75)\`.

- spokes_major, spokes_minor:

  Number of evenly spaced spokes for each weight. \`4\` major gives the
  quadrant crosshairs; \`4\` minor interleaves them as 45-degree
  diagonals. Minor spokes are offset from major by half the major
  spacing (\`pi / spokes_major\`), so their placement is defined
  relative to the major count.

- colour, colour_minor, color, color_minor:

  Spoke/ring colour. \`colour_minor\` defaults to \`colour\`. \`color\`
  and \`color_minor\` are the American-spelling aliases.

- linewidth, linewidth_minor:

  Line widths. \`linewidth_minor\` defaults to \`0.5 \* linewidth\`.

- linetype:

  Line type for the spokes. Default \`"solid"\`.

- disc_fill:

  Fill colour for the background disc; \`NA\` (default) draws no disc.

- origin:

  Logical; add a centre dot (\[add_origin_point()\]). Default \`FALSE\`.

- origin_colour, origin_size, origin_color:

  Centre-dot style. \`origin_color\` is the American-spelling alias.

- n_pts:

  Points used to approximate the disc outline. Default \`200L\`.

## Value

A list of ggplot2 layers.

## See also

\[add_multiple_circles()\], \[add_quadrant_lines()\],
\[add_origin_point()\]

## Examples

``` r
library(ggplot2)
ggplot() + coord_fixed() +
  add_radial_grid(disc_fill = "grey92", colour = "white") +
  add_circ()
```
