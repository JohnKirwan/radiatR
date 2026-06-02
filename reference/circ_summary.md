# Circular summaries per trajectory

Computes per-trial or global circular statistics (mean direction,
resultant length, concentration) from the step-angle column of a
\`TrajSet\`.

## Usage

``` r
circ_summary(
  x,
  w = NULL,
  by = c("id", "global"),
  angle_convention = c("unit_circle", "clock")
)

circ_summary(
  x,
  w = NULL,
  by = c("id", "global"),
  angle_convention = c("unit_circle", "clock")
)

# S4 method for class 'TrajSet'
circ_summary(
  x,
  w = NULL,
  by = c("id", "global"),
  angle_convention = c("unit_circle", "clock")
)
```

## Arguments

- x:

  A \[\`TrajSet\`\] object.

- w:

  Character. Name of a weight column in \`x@data\`. When \`NULL\`
  (default), all steps are weighted equally.

- by:

  Character. \`"id"\` (default) returns one row per trial; \`"global"\`
  pools all observations into a single summary row.

- angle_convention:

  Character. Output convention for \`mean_dir\`: \`"unit_circle"\`
  (default; 0 = East, counterclockwise) or \`"clock"\` (0 = North,
  clockwise).

## Value

data.frame(id, n, t_start, t_end, mean_dir, resultant_R, kappa)

A \`data.frame\` with columns \`id\`, \`n\`, \`t_start\`, \`t_end\`,
\`mean_dir\` (radians, 0 to 2pi), \`resultant_R\` (0–1), and \`kappa\`
(von Mises concentration; \`NA\` when estimation fails).

## Examples

``` r
if (FALSE) { # \dontrun{
data(cpunctatus)
circ_summary(cpunctatus, by = "id")
circ_summary(cpunctatus, by = "global")
} # }
```
