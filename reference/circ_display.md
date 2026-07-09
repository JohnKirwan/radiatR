# Circular display convention specification

Describes how unit-circle radian angles are rendered in plots and
tables. Pass to any display function as the \`display\` argument.

## Usage

``` r
circ_display(zero = pi/2, clockwise = TRUE, units = c("degrees", "radians"))
```

## Arguments

- zero:

  UC angle (radians) that maps to display 0. Default \`pi/2\`
  (geographic North at top — standard compass/clock layout). Use \`0\`
  to put a stimulus that lies at East (positive rel_x) at the top.

- clockwise:

  Logical. \`TRUE\` (default) for clockwise-positive angles.

- units:

  \`"degrees"\` (default) or \`"radians"\` for table outputs and degree
  label annotations.

## Value

A \`circ_display\` list.
