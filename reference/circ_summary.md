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
if (FALSE) { # \dontrun{
data(cpunctatus)
circ_summary(cpunctatus, by = "id")
circ_summary(cpunctatus, by = "global")
} # }
```
