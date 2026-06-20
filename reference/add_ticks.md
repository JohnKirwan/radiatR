# Create evenly spaced radial tick marks.

Generates a \`geom_segment()\` layer containing \`n\` evenly spaced tick
marks around the unit circle, each spanning a radial distance of
\`length\` straddling radius 1. The layer can be added to any ggplot.

## Usage

``` r
add_ticks(
  colour = "black",
  linewidth = 0.5,
  length = 0.1,
  n = 8L,
  color = NULL
)
```

## Arguments

- colour, color:

  Tick colour. Default \`"black"\`. \`color\` is the American-spelling
  alias.

- linewidth:

  Tick line width. Default \`0.5\`.

- length:

  Radial length of each tick, in data units. Default \`0.1\`.

- n:

  Number of evenly spaced ticks. Default \`8L\`.

## Value

A \`geom_segment()\` layer.

## Examples

``` r
library(ggplot2)
ggplot() +
  coord_fixed() +
  add_ticks()
```
