# Add a critical resultant-length circle to a radiate plot

Draws an inner reference circle at the *critical mean resultant length*
– the smallest \\\bar R\\ at which a circular significance test reaches
`alpha` for the given sample size. If a group's mean-direction arrow
([`add_heading_arrow`](https://johnkirwan.github.io/radiatR/reference/add_heading_arrow.md))
extends beyond this circle, that group's headings are significantly
directed at the `alpha` level. This is the convention used by Oriana and
similar circular-statistics software.

## Usage

``` r
add_critical_r(
  hd,
  alpha = 0.05,
  test = c("rayleigh", "vtest"),
  angle_col = "heading",
  group_col = NULL,
  per_group = FALSE,
  colour = "firebrick",
  colour_by_group = TRUE,
  linetype = "dashed",
  linewidth = 0.6,
  n_pts = 200L,
  color = NULL,
  color_by_group = NULL
)
```

## Arguments

- hd:

  Data frame of headings with a heading column (radians).

- alpha:

  Significance level. Default `0.05`.

- test:

  `"rayleigh"` (default) or `"vtest"`.

- angle_col:

  Heading column name. Default `"heading"`.

- group_col:

  Column identifying groups. `NULL` pools all rows.

- per_group:

  Logical. When `group_col` is set but the plot is not faceted, draw one
  circle per group (`TRUE`) or a single conservative circle (`FALSE`,
  default). Ignored when faceting (always per panel).

- colour, color:

  Circle colour. Default `"firebrick"`. When `per_group = TRUE` and
  `colour_by_group = TRUE` this is overridden by the colour scale.
  `color` is the American-spelling alias.

- colour_by_group, color_by_group:

  Logical. When `per_group = TRUE`, map each circle's colour to its
  group (`TRUE`, default) or draw every circle in the fixed `colour`
  while still attaching the group column so the circles facet (`FALSE`).
  Use `FALSE` to keep per-panel circles a single colour without
  injecting the grouping levels into the parent plot's colour scale.
  Ignored unless `per_group = TRUE`. `color_by_group` is the
  American-spelling alias.

- linetype:

  Line type. Default `"dashed"`.

- linewidth:

  Line width. Default `0.6`.

- n_pts:

  Points used to approximate each circle. Default `200L`.

## Value

A `geom_path` layer, or `NULL` if no group has `n >= 2`.

## Details

Two tests are supported:

- `"rayleigh"` (default):

  Tests against uniformity with no hypothesised direction. Critical
  value \\\bar R\_{crit} = \sqrt{-\log(\alpha) / n}\\ (asymptotic;
  accurate for \\n \ge 10\\).

- `"vtest"`:

  Tests against a specific direction. Critical value \\\bar R\_{crit} =
  z\_\alpha / \sqrt{2n}\\ at its most powerful (when the observed mean
  direction equals the hypothesised \\\mu_0\\); the effective threshold
  rises as the observed direction departs from \\\mu_0\\, so this circle
  is a lower bound.

Sample size `n` is taken per group from `hd`. When `group_col` matches
the parent `radiate(facets = ...)` argument, one circle is drawn per
panel at the radius appropriate to that panel's `n`. For groups overlaid
in a single panel, set `per_group = TRUE` to draw one circle per group
(colour-matched), or `per_group = FALSE` (default) to draw a single
conservative circle using the smallest `n` (largest critical radius).

## See also

[`add_heading_arrow`](https://johnkirwan.github.io/radiatR/reference/add_heading_arrow.md),
[`add_circ`](https://johnkirwan.github.io/radiatR/reference/add_circ.md)
