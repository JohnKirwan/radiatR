# Add quadrant lines to a radial plot

Draws two dashed lines through the centre of the unit circle – one
horizontal (0degrees/180degrees) and one vertical (90degrees/270degrees)
– dividing the unit circle into four quadrants. The lines extend to the
circumference (unit circle).

## Usage

``` r
add_quadrant_lines(
  colour = "grey60",
  linewidth = 0.5,
  linetype = "dashed",
  color = NULL
)
```

## Arguments

- colour, color:

  Line colour. Default \`"grey60"\`. \`color\` is the American-spelling
  alias.

- linewidth:

  Line width. Default \`0.5\`.

- linetype:

  Line type. Default \`"dashed"\`.

## Value

A \`geom_segment()\` layer.

## Examples

``` r
library(ggplot2)
ggplot() + coord_fixed() + add_quadrant_lines()
```
