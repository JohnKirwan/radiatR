# Generate world coordinates for checkerboard corners

Helper mirroring MATLAB's \`generateCheckerboardPoints\`, returning the
coordinates of inner checkerboard corners in world units.

## Usage

``` r
checkerboard_points(
  board_dims,
  square_size = 1,
  origin = c(0, 0),
  order = c("row_major", "col_major"),
  as_tibble = TRUE
)
```

## Arguments

- board_dims:

  Integer vector of length 2 giving the number of squares in the
  checkerboard along the vertical (rows) and horizontal (columns)
  directions. Must be \>= 2 in each dimension.

- square_size:

  Numeric scalar or length-2 vector giving the spacing between adjacent
  corners in world units. When length-1, the same spacing is used in
  both directions.

- origin:

  Length-2 numeric vector specifying the world coordinate to assign to
  the corner at row 0, column 0. Defaults to \`c(0, 0)\`.

- order:

  Traversal order for the returned points. \`"row_major"\` increments
  the row index fastest (matching MATLAB), while \`"col_major"\`
  increments the column index fastest.

- as_tibble:

  Logical; if \`TRUE\` (default), return a tibble with columns
  \`corner\`, \`row\`, \`col\`, \`x\`, and \`y\`. If \`FALSE\`, return a
  numeric matrix of x/y coordinates.

## Value

Tibble or matrix describing the checkerboard corner coordinates in world
units.
