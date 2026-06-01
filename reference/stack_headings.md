# Add stacking columns to a headings data frame

Computes radial positions for stacked dot plots on circular plots.
Observations at the same angle (or within `tol` radians of each other)
are assigned successive radial positions, preventing overplotting of
coincident or binned headings.

## Usage

``` r
stack_headings(
  data,
  col = NULL,
  step = 0.025,
  tol = NULL,
  direction = "inward",
  base_r = 1,
  shade = FALSE,
  shape = FALSE
)
```

## Arguments

- data:

  A data frame with an angle column in radians.

- col:

  Name of the angle column. Defaults to the `heading_col` attribute when
  `data` is a `headings_frame`.

- step:

  Radial offset per stack level as a fraction of `base_r`. Default
  `0.025` matches `circular`'s `sep` default.

- tol:

  Grouping tolerance in radians. `NULL` (default) = exact equality,
  correct for binned data. `tol > 0` assigns each observation to the
  nearest group centre within `tol` radians (greedy, sorted-order scan);
  angles near `0` and `2*pi` are not treated as neighbours.

- direction:

  `"inward"` (default, stacks toward centre) or `"outward"` (away from
  perimeter, matches `circular` default).

- base_r:

  Radius of the reference circle in data units. Default 1.

- shade:

  If `TRUE`, add a `shade_n` column (alias of `stack_n`) for use as an
  alpha aesthetic.

- shape:

  If `TRUE`, add a `shape_code` integer column: 1 = hollow (outermost /
  singleton), 2 = filled (middle), 3 = filled with ring (innermost in a
  stack of 3+).

## Value

`data` augmented with `stack_r` and `stack_n` columns (always), plus
`shade_n` and/or `shape_code` when requested. Row count is unchanged.

## See also

[`headings_frame`](https://johnkirwan.github.io/radiatR/reference/headings_frame.md),
`add_stacked_headings`
