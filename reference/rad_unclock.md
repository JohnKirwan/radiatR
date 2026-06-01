# Convert clock-oriented angles back to unit-circle orientation

Convert clock-oriented angles back to unit-circle orientation

## Usage

``` r
rad_unclock(theta)
```

## Arguments

- theta:

  Angle (radians) measured clockwise with zero at the top.

## Value

Angle in radians in \[0, 2\*pi), unit-circle convention.

## Examples

``` r
theta <- seq(from = 0, to = 2 * pi, length.out = 5)
rad_unclock(theta)
#> [1] 1.570796 0.000000 4.712389 3.141593 1.570796
```
