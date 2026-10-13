# Circular summary statistics for trajectories

Computes per-trial or global circular statistics (mean direction,
resultant length, concentration) from the step-angle column of a
\`Tracks\`.

## Usage

``` r
circ_summary(x, w = NULL, by = c("id", "global"), axial = FALSE)

# S4 method for class 'Tracks'
circ_summary(x, w = NULL, by = c("id", "global"), axial = FALSE)
```

## Arguments

- x:

  A \[\`Tracks\`\] object.

- w:

  Character. Name of a weight column in \`x@data\`. When \`NULL\`
  (default), all steps are weighted equally.

- by:

  Character. \`"id"\` (default) returns one row per trial; \`"global"\`
  pools all observations into a single summary row.

- axial:

  Logical. Treat the angles as axial (bidirectional, mod-pi) data:
  statistics are computed via the angle-doubling method and the mean is
  reported as an axis in \`\[0, pi)\` radians / \`\[0, 180)\` degrees.
  Default \`FALSE\` (ordinary directional data).

## Value

A \`data.frame\` with columns \`id\`, \`n\`, \`t_start\`, \`t_end\`,
\`mean_dir\` (radians, unit-circle convention, 0 to 2pi),
\`resultant_R\` (0–1), and \`kappa\` (von Mises concentration: the
maximum-likelihood estimate from the mean resultant length, weighted
when weights are used; \`NA\` for fewer than three valid angles, \`Inf\`
when all angles coincide). No small-sample bias correction is applied,
so `kappa` is biased upward for small `n`:
`circular::mle.vonmises(bias = TRUE)` gives a corrected estimate and
[`boot_kappa_ci()`](https://johnkirwan.github.io/radiatR/reference/boot_kappa_ci.md)
reports the bias.

## Examples

``` r
data(cpunctatus)
head(circ_summary(cpunctatus, by = "id"))
#>        id   n t_start t_end  mean_dir resultant_R      kappa
#> 1 10_10_1 299       1   299 2.2781642   0.1548308  0.3134475
#> 2 10_11_1  65      14    78 0.2543320   0.6671992  1.8194711
#> 3 10_12_1  48       2    49 2.3069985   0.9037793  5.4856988
#> 4 10_13_1  80      22   101 0.9651068   0.5374843  1.2768013
#> 5 10_14_1  30       1    30 4.7633599   0.9718670 18.0335307
#> 6 10_15_1  54       2    55 3.5207738   0.9370359  8.2159697
circ_summary(cpunctatus, by = "global")
#>       id     n t_start t_end   mean_dir resultant_R     kappa
#> 1 global 44331       1   299 0.01906991   0.1247348 0.2514355
```
