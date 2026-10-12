# Dwell-time proportions across quadrant x ring zones

Classifies each trajectory observation into one of N quadrant sectors
and M annular rings, then returns per-trial frame counts and
proportions. Applicable to any circular-field analysis where spatial
dwell time is of interest (e.g. water maze, open-field, Drosophila
preference assay).

## Usage

``` r
zone_dwell(
  x,
  target_angle,
  target_radius = 1,
  ring_breaks = c(0, 0.5, 0.8, 1),
  coords = c("absolute", "relative")
)
```

## Arguments

- x:

  A \[\`Tracks\`\] object with x/y (or rel_x/rel_y) columns registered.

- target_angle:

  Numeric. Radians. Direction of the target zone from the origin. Q1
  spans +/-45degrees around this angle.

- target_radius:

  Numeric. Accepted for API symmetry with \[count_goal_entries()\] but
  not used in zone assignment. Default \`1\`.

- ring_breaks:

  Numeric vector. Annular ring boundaries, must start at \`0\`. Default
  \`c(0, 0.5, 0.8, 1)\` gives three rings: inner / middle / outer
  (thigmotaxis).

- coords:

  Character. \`"absolute"\` (default) uses \`@cols\$x\`/\`@cols\$y\`;
  \`"relative"\` uses \`@cols\$rel_x\`/\`@cols\$rel_y\`.

## Value

A \`data.frame\` with one row per observed (id x quadrant x ring)
combination, with columns \`id\`, \`quadrant\` (integer, 1 = target),
\`ring\` (integer, 1 = innermost), \`zone\` (e.g. \`"Q1.R3"\`),
\`n_frames\` (integer), and \`proportion\` (numeric). Combinations with
zero observations are omitted.

## Details

The target quadrant (Q1) is centred on \`target_angle\`; Q2–Q4 follow
counter-clockwise. Observations outside \`max(ring_breaks)\` are
excluded from both counts and the proportion denominator.

## See also

\[count_goal_entries()\]

## Examples

``` r
# Treat 45 degrees (NE) as the target, as for a water maze probe trial
data(cpunctatus)
dwell <- zone_dwell(cpunctatus, target_angle = pi / 4,
                    ring_breaks = c(0, 0.5, 0.8, 1))
# Q1 proportion > 0.25 indicates above-chance target preference
head(dwell)
#>                id quadrant ring  zone n_frames proportion
#> 10_10_1.1 10_10_1        1    1 Q1.R1       50 0.16722408
#> 10_10_1.2 10_10_1        2    1 Q2.R1       52 0.17391304
#> 10_10_1.3 10_10_1        3    1 Q3.R1       32 0.10702341
#> 10_10_1.4 10_10_1        4    1 Q4.R1       57 0.19063545
#> 10_10_1.5 10_10_1        1    2 Q1.R2       22 0.07357860
#> 10_10_1.6 10_10_1        2    2 Q2.R2        3 0.01003344
```
