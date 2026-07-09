# Add a landmark marker to a circular plot

Draws a styled point just outside the rim at \`bearing\`, with a
sun-like default (a gold asterisk), plus an optional text label. Change
\`shape\`, \`colour\`/\`fill\` and \`label\` to mark any landmark (nest,
feeder, light source). The bearing is given in \*\*display units\*\*
(the value read off the axis), so the marker lands at the labelled
position under any \[circ_display()\] convention.

## Usage

``` r
add_landmark(
  bearing = 0,
  r = 1.12,
  shape = 8,
  size = 4,
  colour = "goldenrod",
  fill = "gold",
  label = NULL,
  display = circ_display(),
  ...,
  color = NULL
)
```

## Arguments

- bearing:

  Direction of the landmark, in display units (degrees by default).
  Default \`0\`.

- r:

  Radius at which to draw the point. Default \`1.12\` (just outside the
  unit circle).

- shape:

  Point shape. Default \`8\` (asterisk, sun-ray-like).

- size:

  Point size. Default \`4\`.

- colour, color:

  Point colour. Default \`"goldenrod"\`. \`color\` is the
  American-spelling alias.

- fill:

  Fill colour, used only by fillable shapes (21–25). Default \`"gold"\`.

- label:

  Optional text label, drawn just beyond the point. \`NULL\` (default)
  draws no label.

- display:

  A \[circ_display()\] giving the plot's angle convention. Default
  \`circ_display()\`. Pass the same value used in \[radiate()\].

- ...:

  Further arguments passed to \[ggplot2::geom_point()\].

## Value

A list of ggplot layers: a \`geom_point()\` layer, plus a
\`geom_text()\` layer when \`label\` is supplied.

## American spellings

Every \`colour...\` argument and the \`assign_colour\_\*\` /
\`cycle_colours\` / \`hf_colour_col\` functions accept the American
\`color...\` spelling as an alias (e.g. \`color\`, \`color_col\`,
\`track_color\`). British spelling is canonical; supplying both
spellings of a pair is an error.

## See also

\[add_stimulus_arc()\], \[add_origin_point()\]

## Examples

``` r
library(ggplot2)
ggplot() + coord_fixed() + add_circ() +
  add_landmark(bearing = 45, label = "sun")
```
