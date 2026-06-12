# Mark the centre of a radial plot

Adds a single point at the origin \`(0, 0)\` – a centre reference for
sparse themes where no crosshairs meet at the middle.

## Usage

``` r
add_origin_point(colour = "grey50", size = 1.5, shape = 16, ...)
```

## Arguments

- colour:

  Point colour. Default \`"grey50"\`.

- size:

  Point size. Default \`1.5\`.

- shape:

  Point shape. Default \`16\` (filled circle).

- ...:

  Further arguments passed to \[ggplot2::geom_point()\].

## Value

A \`geom_point()\` layer.

## Examples

``` r
library(ggplot2)
ggplot() + coord_fixed() + add_circ() + add_origin_point()
```
