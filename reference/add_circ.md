# Draw a circular guide.

Creates a list of annotation layers that render a circle with the
requested radius and appearance. The returned list can be added directly
to a ggplot.

## Usage

``` r
add_circ(
  radius = 1,
  circle_color = "grey60",
  circle_alpha = 1,
  circle_size = 1
)
```

## Arguments

- radius:

  Radius of the circle, expressed in the same units as the plot
  coordinates.

- circle_color:

  Line colour for the circle.

- circle_alpha:

  Alpha transparency for the circle.

- circle_size:

  Line width for the circle.

## Value

A list containing a single ggplot annotation layer.

## Examples

``` r
library(ggplot2)
ggplot() +
  coord_fixed() +
  add_circ(radius = 1)
```
