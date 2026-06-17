# Add stacked heading dots as a ggplot2 layer

Places one point per observation at its heading angle, stacking
coincident angles radially to avoid overplotting. If `stack_r` is
already a column in `data` (from a prior call to
[`stack_headings`](https://johnkirwan.github.io/radiatR/reference/stack_headings.md)),
it is used as-is; otherwise stacking is computed internally.

## Usage

``` r
add_stacked_headings(
  data,
  col = NULL,
  step = 0.025,
  start_sep = 0,
  tol = NULL,
  direction = "inward",
  base_r = 1,
  shade = FALSE,
  shape = FALSE,
  group = NULL,
  colour = "black",
  colour_col = NULL,
  size = 2,
  alpha = 1,
  axial = FALSE,
  ...
)
```

## Arguments

- data:

  A data frame with an angle column in radians, typically a
  [`headings_frame`](https://johnkirwan.github.io/radiatR/reference/headings_frame.md).

- col:

  Name of the angle column. Defaults to the `heading_col` attribute when
  `data` is a `headings_frame`.

- step, start_sep, tol, direction, base_r:

  Passed to
  [`stack_headings`](https://johnkirwan.github.io/radiatR/reference/stack_headings.md)
  when `stack_r` is absent. See that function for details. `step` sets
  the gap between dots and `start_sep` offsets the first dot off the
  reference circle.

- shade:

  If `TRUE`, map `stack_n` to the alpha aesthetic (scaled 0.2–1 across
  the observed range). Overrides the fixed `alpha` argument.

- shape:

  Passed to
  [`stack_headings`](https://johnkirwan.github.io/radiatR/reference/stack_headings.md)
  to request per-observation shape encoding. Shape is also applied when
  `shape_code` is already a column in `data`. Mapped to ggplot2 shape
  integers: 1 = hollow, 16 = filled, 21 = filled with ring.

- group:

  Optional column name; stack within each group independently (e.g. one
  stacking per facet). Default `NULL`.

- colour:

  Fixed point colour (ignored when `colour_col` is set).

- colour_col:

  Optional column name to map to the colour aesthetic.

- size:

  Point size passed to
  [`geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html).

- alpha:

  Fixed alpha. Ignored when `shade = TRUE`.

- axial:

  Logical; when \`TRUE\`, mirror each observation to \`col + pi\` before
  stacking, so the figure reads as bidirectional. Stacking is computed
  after mirroring, so each antipodal cluster stacks within itself.
  Default \`FALSE\`.

- ...:

  Additional arguments passed to
  [`ggplot2::geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html).

## Value

A
[`geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html)
layer.

## See also

[`headings_frame`](https://johnkirwan.github.io/radiatR/reference/headings_frame.md),
[`stack_headings`](https://johnkirwan.github.io/radiatR/reference/stack_headings.md),
[`add_heading_points`](https://johnkirwan.github.io/radiatR/reference/add_heading_points.md)
