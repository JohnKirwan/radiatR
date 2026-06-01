# Add a V-test significance boundary to a radiate plot

Draws the decision boundary for the Rayleigh V test against a specified
direction `mu0`. Unlike the Rayleigh test (a circle, see
[`add_critical_r`](https://johnkirwan.github.io/radiatR/reference/add_critical_r.md)),
the V test privileges one direction, so its boundary is a *straight
line* perpendicular to `mu0` at distance \\c = z\_\alpha / \sqrt{2n}\\
from the centre. A mean-direction arrow
([`add_heading_arrow`](https://johnkirwan.github.io/radiatR/reference/add_heading_arrow.md))
is V-significant if and only if its tip falls on the far side of this
line – equivalently, if its projection onto `mu0` exceeds \\c\\.

## Usage

``` r
add_critical_v_line(
  hd,
  mu0,
  alpha = 0.05,
  angle_col = "heading",
  group_col = NULL,
  per_group = FALSE,
  show_region = FALSE,
  colour = "firebrick",
  linetype = "dashed",
  linewidth = 0.6,
  region_fill = "firebrick",
  region_alpha = 0.08,
  n_pts = 100L
)
```

## Arguments

- hd:

  Data frame of headings with a heading column (radians).

- mu0:

  Hypothesised direction in radians (unit-circle convention).

- alpha:

  Significance level. Default `0.05`.

- angle_col:

  Heading column name. Default `"heading"`.

- group_col:

  Column identifying groups. `NULL` pools all rows.

- per_group:

  Logical. Draw one boundary per group (`TRUE`) or a single conservative
  boundary (`FALSE`, default). Ignored when faceting, where each panel
  gets its own boundary.

- show_region:

  Logical; shade the rejection segment. Default `FALSE`.

- colour:

  Line colour. Default `"firebrick"`.

- linetype:

  Line type. Default `"dashed"`.

- linewidth:

  Line width. Default `0.6`.

- region_fill:

  Fill colour for the rejection region. Default `"firebrick"`.

- region_alpha:

  Fill opacity. Default `0.08`.

- n_pts:

  Points approximating the rejection arc. Default `100L`.

## Value

A list of ggplot2 layers, or `NULL` if no group has a boundary inside
the unit circle.

## Details

The line is clipped to the unit circle (drawn as a chord). With
`show_region = TRUE` the circular segment beyond the line – the
rejection region – is shaded.

Sample size `n` is taken per group from `hd`, with the same options as
[`add_critical_r`](https://johnkirwan.github.io/radiatR/reference/add_critical_r.md):
per-panel when faceting, per-group when `per_group = TRUE`, or a single
conservative boundary (smallest `n`, largest \\c\\) otherwise.

## See also

[`add_critical_r`](https://johnkirwan.github.io/radiatR/reference/add_critical_r.md),
[`add_heading_arrow`](https://johnkirwan.github.io/radiatR/reference/add_heading_arrow.md)
