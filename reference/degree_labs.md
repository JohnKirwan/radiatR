# Label the four diagonal directions.

Provides a list of annotation layers that mark 45, 135, 225, and 315
degrees on a unit circle.

## Usage

``` r
degree_labs(
  display = circ_display(),
  colour = "black",
  units = NULL,
  size = 3.88,
  family = ""
)
```

## Arguments

- display:

  A \[\`circ_display\`\] object. Default \`circ_display()\`. Supplies
  the label units when \`units\` is \`NULL\`.

- colour:

  Label colour. Default \`"black"\`.

- units:

  \`"degrees"\` (e.g. \`45°\`) or \`"radians"\` (e.g. \`π/4\`). When
  \`NULL\` (default) the units are taken from \`display\`.

- size:

  Label text size, in mm. Default \`3.88\` (ggplot2's default text
  size).

- family:

  Label font family. Default \`""\` (the device default).

## Value

A list of ggplot2 annotation layers.

## Examples

``` r
library(ggplot2)
ggplot() +
  coord_fixed() +
  degree_labs()
```
