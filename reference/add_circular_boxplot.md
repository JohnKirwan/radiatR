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
  radius = 1,
  width = 0.1,
  colour = "black",
  box_fill = "grey80",
  farout_shape = 8,
  show_median_arrow = TRUE,
  linewidth = 0.8,
  n_theta = 200L,
  display = NULL
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

  Perimeter radius for the box/whiskers. Default \`1\`.

- width:

  Radial thickness of the box band. Default \`0.1\`.

- colour:

  Outline colour for box, whiskers, crossbars, far-out. Default
  \`"black"\`.

- box_fill:

  Fill colour of the box band. Default \`"grey80"\`.

- farout_shape:

  Point shape for far-out values. Default \`8\` (star).

- show_median_arrow:

  Draw a radial arrow at the median. Default \`TRUE\`.

- linewidth:

  Line width for arcs/crossbars. Default \`0.8\`.

- n_theta:

  Points per arc. Default \`200\`.

- display:

  A \[circ_display()\] object; when \`NULL\` (default), taken from
  \`attr(hd, "display")\`, falling back to \`circ_display()\`.

## Value

A list of ggplot2 layers, or \`NULL\` when the boxplot is not drawable
(a \`warning()\` is emitted with the reason).

## References

Buttarazzi, D., Pandolfo, G. & Porzio, G. C. (2018). A boxplot for
circular data. *Biometrics* 74(4), 1492–1501.
[doi:10.1111/biom.12889](https://doi.org/10.1111/biom.12889)

## See also

\[circ_boxplot_stats()\], \[radiate()\]
