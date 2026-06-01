# Add multiple concentric circles to a ggplot object

This function creates a list of layers for concentric circles with
specified radii that can be added to a ggplot object. The function takes
optional arguments to customize the appearance of the circles.

## Usage

``` r
add_multiple_circles(
  radii = c(0.25, 0.5, 0.75),
  circle_color = "grey20",
  circle_alpha = 1,
  circle_size = 0.5
)
```

## Arguments

- radii:

  A vector of radii for the concentric circles (default is c(0.25, 0.5,
  0.75))

- circle_color:

  The color of the circles (default is "grey40")

- circle_alpha:

  The transparency of the circles (default is 1)

- circle_size:

  The size of the circle lines (default is 1)

## Value

A list of layers for concentric circles

## Examples

``` r
library(ggplot2)
ggplot() + coord_fixed() + add_multiple_circles()

```
