# Label the four diagonal directions.

Provides a list of annotation layers that mark 45, 135, 225, and 315
degrees on a unit circle.

## Usage

``` r
degree_labs(display = circ_display())
```

## Arguments

- display:

  A \[\`circ_display\`\] object. Controls whether labels are shown in
  degrees or radians. Default \`circ_display()\`.

## Value

A list of ggplot2 annotation layers.

## Examples

``` r
library(ggplot2)
ggplot() +
  coord_fixed() +
  degree_labs()
```
