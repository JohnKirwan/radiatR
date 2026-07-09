# Wrap angles to the interval (-pi, pi\]

Wrap angles to the interval (-pi, pi\]

## Usage

``` r
rad_shepherd(theta)
```

## Arguments

- theta:

  Numeric vector of angles (radians).

## Value

Angles wrapped to (-pi, pi\].

## Examples

``` r
theta <- seq(from = -5, to = 5, length.out = 6)
rad_shepherd(theta)
#> [1]  1.283185 -3.000000 -1.000000  1.000000  3.000000 -1.283185
```
