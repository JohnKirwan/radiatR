# Render a pre-computed circular interval arc on a radial plot

Takes a data frame produced by \[compute_circ_interval()\] (or
equivalent) and renders it as a \`geom_path()\` arc at radius
\`radius\`. Each row produces one arc. Rows where \`lower\` or \`upper\`
is \`NA\` are silently skipped.

## Usage

``` r
add_circ_interval(
  interval_df,
  colour_col = NULL,
  radius = 1.05,
  linewidth = 1.5,
  colour = NULL,
  linetype = "solid",
  n_theta = 500L,
  axial = FALSE,
  color_col = NULL,
  color = NULL
)
```

## Arguments

- interval_df:

  Data frame with columns \`mean_dir\`, \`lower\`, \`upper\` (radians,
  \`\[-pi, pi\]\`), and optionally \`wraps\` (logical). Typically the
  output of \[compute_circ_interval()\].

- colour_col, color_col:

  Optional column name to map to the colour aesthetic. Ignored when
  \`colour\` is also supplied. \`color_col\` is the American-spelling
  alias.

- radius:

  Radial position of the arc. Default \`1.05\`.

- linewidth:

  Line width. Default \`1.5\`.

- colour, color:

  Fixed colour. When \`NULL\` (default) and \`colour_col\` is set,
  colour is mapped from that column; when \`NULL\` and no
  \`colour_col\`, draws in \`"black"\`. Supplying any colour string
  always overrides \`colour_col\`. \`color\` is the American-spelling
  alias.

- linetype:

  Line type. Default \`"solid"\`.

- n_theta:

  Number of points along the arc. Default \`500L\`.

- axial:

  Logical. Render the overlay for axial (bidirectional, mod-pi) data via
  the angle-doubling method: the CI is drawn at both poles of the axis.
  Default \`FALSE\`.

## Value

A \`geom_path()\` layer.

## Details

For the Bayesian extension, replace \`lower\` and \`upper\` in the
output of \[compute_circ_interval()\] with credible interval bounds from
any model before calling this function: “\`r iv \<-
compute_circ_interval(hd) iv\$lower \<- posterior_lower iv\$upper \<-
posterior_upper ggplot() + coord_fixed() + add_circ_interval(iv) “\`

## See also

\[compute_circ_interval()\], \[add_heading_interval()\]
