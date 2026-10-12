# Find the intercept of a line with the unit circle

Vectorised over its arguments: each \`(x0, y0)\` -\> \`(x1, y1)\` pair
gives one row of the result.

## Usage

``` r
line_circle_intercept(x0, y0, x1, y1)
```

## Arguments

- x0, y0:

  Starting coordinates of the vector.

- x1, y1:

  Ending coordinates of the vector.

## Value

A tibble with the intersection point closest to \`(x1, y1)\`.
