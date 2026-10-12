# Circular summaries per trajectory

Computes per-trial or global circular statistics (mean direction,
resultant length, concentration) from the step-angle column of a
\`Tracks\`.

## Usage

``` r
circ_summary(x, w = NULL, by = c("id", "global"), axial = FALSE)

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

data.frame(id, n, t_start, t_end, mean_dir, resultant_R, kappa)

A \`data.frame\` with columns \`id\`, \`n\`, \`t_start\`, \`t_end\`,
\`mean_dir\` (radians, unit-circle convention, 0 to 2pi),
\`resultant_R\` (0–1), and \`kappa\` (von Mises concentration; \`NA\`
when estimation fails).

## Examples

``` r
data(cpunctatus)
head(circ_summary(cpunctatus, by = "id"))
#>        id   n t_start t_end  mean_dir resultant_R kappa
#> 1 10_10_1 299       1   299 2.2781642   0.1548308    NA
#> 2 10_11_1  65      14    78 0.2543320   0.6671992    NA
#> 3 10_12_1  48       2    49 2.3069985   0.9037793    NA
#> 4 10_13_1  80      22   101 0.9651068   0.5374843    NA
#> 5 10_14_1  30       1    30 4.7633599   0.9718670    NA
#> 6 10_15_1  54       2    55 3.5207738   0.9370359    NA
circ_summary(cpunctatus, by = "global")
#>       id     n t_start t_end   mean_dir resultant_R kappa
#> 1 global 44331       1   299 0.01906991   0.1247348    NA
```
