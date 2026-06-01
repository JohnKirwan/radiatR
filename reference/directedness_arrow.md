# Make mean resultant length arrow

Computes the circular mean direction and resultant length, returning a
\`geom_segment()\` layer that can be added to an existing ggplot.

## Usage

``` r
directedness_arrow(
  data,
  angle_col,
  arrow_head_cm = 0.2,
  colour = "gray",
  size = 2
)
```

## Arguments

- data:

  Data frame containing the angle column.

- angle_col:

  Column containing angles in radians.

- arrow_head_cm:

  Length of the arrowhead in centimetres.

- colour:

  Colour of the arrow.

- size:

  Width of the arrow segment (applied to the geom's \`linewidth\`).

## Value

A \`geom_segment()\` layer.
