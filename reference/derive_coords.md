# Derive polar and reference-relative coordinates from unit-circle position

Given a unit-circle Cartesian position (\`trans_x\`, \`trans_y\`) and a
reference direction, computes the dependent coordinate columns: the
radius, the absolute angle (unit-circle and clock conventions), and the
reference-relative angle and Cartesian position. This is the single
source of the unit-circle -\> polar/relative transformation used across
the package.

## Usage

``` r
derive_coords(trans_x, trans_y, reference = 0)
```

## Arguments

- trans_x, trans_y:

  Numeric vectors of unit-circle Cartesian coordinates (same length).

- reference:

  Reference direction in unit-circle radians; a scalar applied to all
  points, or a vector recycled per element. Default \`0\` (relative
  frame equals absolute frame).

## Value

A data frame with \`trans_rho\`, \`abs_theta_clock\`,
\`abs_theta_unit\`, \`rel_theta_unit\`, \`rel_x\`, \`rel_y\`.

## See also

\[set_reference()\], \[reference()\]

## Examples

``` r
derive_coords(c(0.5, -0.3), c(0.2, 0.4), reference = pi / 2)
#>   trans_rho abs_theta_clock abs_theta_unit rel_theta_unit rel_x rel_y
#> 1 0.5385165        1.190290      0.3805064      5.0928954   0.2  -0.5
#> 2 0.5000000        5.639684      2.2142974      0.6435011   0.4   0.3
```
