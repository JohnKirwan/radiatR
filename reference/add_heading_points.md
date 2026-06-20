# Add heading endpoint markers on the unit circle

Draws a hollow circle at \`(cos(heading), sin(heading))\` for each row
of a headings data frame, placing one marker per trajectory on the
unit-circle boundary at the derived heading direction. The data frame is
normally the output of \[derive_headings()\].

## Usage

``` r
add_heading_points(
  headings_df,
  colour_col = NULL,
  colour = NULL,
  size = 2,
  alpha = 1,
  axial = FALSE
)
```

## Arguments

- headings_df:

  Data frame with a \`heading\` column (angles in radians).

- colour_col:

  Name of a column in \`headings_df\` to map to the colour aesthetic.
  When \`NULL\` (default), the value of \`attr(headings_df,
  "colour_col")\` is used if set – so heading markers automatically
  inherit the colour mapping from the associated trajectory plot when
  that attribute is present. Ignored when \`colour\` is supplied.

- colour:

  Fixed colour string. Overrides \`colour_col\` when supplied; when
  \`NULL\` and no \`colour_col\` resolves, defaults to \`"black"\`.

- size:

  Point size passed to \`geom_point\`.

- alpha:

  Point alpha transparency.

- axial:

  Logical; when \`TRUE\`, draw each observation at both \`heading\` and
  \`heading + pi\` (bidirectional/axial display). Default \`FALSE\`.

## Value

A \`geom_point()\` layer (shape = 1, hollow circle).

## See also

\[add_heading_vectors()\], \[derive_headings()\]

## Examples

``` r
library(ggplot2)
# headings from a Tracks via derive_headings(ts, rule = "crossing", ...)
hd <- data.frame(id = "A", time = 1, heading = pi / 4)
ggplot() + coord_fixed() + add_heading_points(hd)

ggplot() + coord_fixed() + add_heading_points(hd, colour = "steelblue")
```
