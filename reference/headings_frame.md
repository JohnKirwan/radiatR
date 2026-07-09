# Construct a headings frame from a data frame of angles

Validates the angle column, optionally converts degrees to radians, and
marks the data frame with class `headings_frame` and attributes that
downstream functions (`stack_headings`, `add_stacked_headings`,
[`radiate`](https://johnkirwan.github.io/radiatR/reference/radiate.md))
will use as defaults.

## Usage

``` r
headings_frame(
  data,
  col,
  units,
  angle_convention = "unit_circle",
  coords = "absolute"
)
```

## Arguments

- data:

  A data frame containing the angle column.

- col:

  Unquoted or quoted name of the angle column.

- units:

  Units of the angle column: `"radians"` or `"degrees"`. No default —
  must be specified. Values are converted to radians in place when
  `"degrees"`.

- angle_convention:

  `"unit_circle"` (0 = East, CCW, default) or `"clock"` (0 = North, CW).

- coords:

  `"absolute"` (default) or `"relative"`.

## Value

A `data.frame` with additional class `"headings_frame"` and attributes
`heading_col`, `angle_convention`, `coords`.

## See also

`stack_headings`, `add_stacked_headings`,
[`radiate`](https://johnkirwan.github.io/radiatR/reference/radiate.md)
