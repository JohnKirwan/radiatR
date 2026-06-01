# Convert unit-circle angles to clock orientation

Convert unit-circle angles to clock orientation

## Usage

``` r
rad2clock(theta)
```

## Arguments

- theta:

  Angle in radians using the standard unit-circle convention.

## Value

Angle in radians measured clockwise with zero at the top.

## Examples

``` r
theta <- seq(from = pi/2, to = -pi/2, length.out = 5)
rad2clock(theta)
#> [1] 0.0000000 0.7853982 1.5707963 2.3561945 3.1415927
```
