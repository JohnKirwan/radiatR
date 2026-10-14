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

## Examples

``` r
# A ray from the origin through (2, 0) hits the unit circle at (1, 0)
line_circle_intercept(0, 0, 2, 0)
#> # A tibble: 1 × 2
#>   x_int y_int
#>   <dbl> <dbl>
#> 1     1     0

# Vectorised: multiple rays at once
line_circle_intercept(c(0, 0), c(0, 0), c(2, 0), c(0, 3))
#> # A tibble: 2 × 2
#>   x_int y_int
#>   <dbl> <dbl>
#> 1     1     0
#> 2     0     1
```
