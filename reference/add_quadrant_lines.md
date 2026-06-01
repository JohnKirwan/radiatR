# Add quadrant lines to a radial plot

Draws two dashed lines through the centre of the unit circle – one
horizontal (0degrees/180degrees) and one vertical (90degrees/270degrees)
– dividing the arena into four quadrants. The lines extend to the arena
boundary (unit circle).

## Usage

``` r
add_quadrant_lines(colour = "grey60", linewidth = 0.5, linetype = "dashed")
```

## Arguments

- colour:

  Line colour. Default \`"grey60"\`.

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
