# Add a circular boxplot layer to a radial plot

Renders the \[circ_boxplot_stats()\] summary onto a \[radiate()\] polar
plot in the Buttarazzi et al. (2018) style: a filled box band between
the hinges, whisker arcs, short crossbars at the median and hinges,
far-out points, and an optional median arrow. For \`axial = TRUE\` every
element is drawn at both poles of the axis (offsets are computed with
mod-pi arithmetic so the box is a short band even when the axis sits on
the 0/pi seam). Composable with \`+\`.

## Usage

``` r
add_circular_boxplot(
  hd,
  angle_col = "heading",
  axial = FALSE,
  radius = 1.1,
  width = 0.05,
  colour = "black",
  box_fill = "grey90",
  farout_shape = 8,
  show_median_arrow = FALSE,
  linewidth = 0.8,
  n_theta = 200L,
  display = NULL,
  panel_by = NULL,
  theme = NULL,
  color = NULL
)
```

## Arguments

- hd:

  A data frame with a column of heading angles in radians (unit-circle
  convention), or a numeric vector of angles.

- angle_col:

  Name of the angle column when \`hd\` is a data frame. Default
  \`"heading"\`.

- axial:

  Logical. Treat the angles as axial (bidirectional, mod-pi): angles are
  doubled, the boxplot computed, and locations halved back to \`\[0,
  pi)\`; the fence multiplier is taken on the doubled data. Default
  \`FALSE\`.

- radius:

  Perimeter radius for the box/whiskers. Default \`1.1\` (just outside
  the unit circle).

- width:

  Radial thickness of the box band. Default \`0.05\`.

- colour, color:

  Outline colour for box, whiskers, crossbars, far-out. Default
  \`"black"\`. \`color\` is the American-spelling alias.

- box_fill:

  Fill colour of the box band. Default \`"grey90"\`.

- farout_shape:

  Point shape for far-out values. Default \`8\` (star).

- show_median_arrow:

  Draw a centre-to-edge arrow at the median direction (a median pointer,
  not a rho resultant). Default \`FALSE\`.

- linewidth:

  Line width for arcs/crossbars. Default \`0.8\`.

- n_theta:

  Points per arc. Default \`200\`.

- display:

  A \[circ_display()\] object; when \`NULL\` (default), taken from
  \`attr(hd, "display")\`, falling back to \`circ_display()\`.

- panel_by:

  Optional name of a column in \`hd\` identifying facet panels (the same
  column passed to \`radiate(panel_by = )\`). When set, a separate
  boxplot is computed and drawn per level so each facet shows its own
  summary; the layer data is tagged with this column so it faces
  correctly. A level that is not drawable (fewer than 4 usable
  observations or a non-unique median) is skipped with a warning while
  the others still draw. Default \`NULL\` draws a single boxplot from
  all of \`hd\`.

- theme:

  Optional radiate theme name (e.g. "void", "minimal", "dark"). When
  set, the box, whiskers and crossbars take that theme's chrome colour
  (matching the circle and ticks), overriding `colour`.

## Value

A list of ggplot2 layers, or \`NULL\` when the boxplot is not drawable
(a \`warning()\` is emitted with the reason).

## References

Buttarazzi, D., Pandolfo, G. & Porzio, G. C. (2018). A boxplot for
circular data. *Biometrics* 74(4), 1492–1501.
[doi:10.1111/biom.12889](https://doi.org/10.1111/biom.12889)

## See also

\[circ_boxplot_stats()\], \[radiate()\]
