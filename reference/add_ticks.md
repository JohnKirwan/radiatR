# Create tick marks at the cardinal directions.

Generates a \`geom_segment()\` layer containing small tick marks at
north, south, east, and west. The layer can be added to any ggplot.

## Usage

``` r
add_ticks(colour = "black")
```

## Arguments

- colour:

  Tick colour. Default \`"black"\`.

## Value

A \`geom_segment()\` layer.

## Examples

``` r
library(ggplot2)
ggplot() +
  coord_fixed() +
  add_ticks()
```
