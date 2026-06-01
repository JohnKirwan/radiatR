# Build a mapping from raw pixels to unit-circle coordinates

Build a mapping from raw pixels to unit-circle coordinates

## Usage

``` r
build_unit_circle_mapping(origin, reference, flip_y = TRUE)
```

## Arguments

- origin:

  Numeric vector of length 2 giving the circle origin (x, y).

- reference:

  Numeric vector of length 2 giving the landmark on the perimeter that
  defines the zero-degree heading.

- flip_y:

  Logical; if \`TRUE\` (default), invert the y-axis so positive values
  point upward (matching the unit-circle convention).

## Value

A list containing origin metadata, a forward mapping (\`map\`) that
converts raw pixels to unit-circle coordinates, and an inverse mapping
(\`inverse\`) that converts back to the original pixel space.
