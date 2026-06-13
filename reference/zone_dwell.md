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

  A \[\`TrajSet\`\] object with x/y (or rel_x/rel_y) columns registered.

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
if (FALSE) { # \dontrun{
# Water maze probe trial: platform was at 45 degrees (NE)
dwell <- zone_dwell(ts, target_angle = pi / 4,
                    ring_breaks = c(0, 0.5, 0.8, 1))
# Q1 proportion > 0.25 indicates above-chance target preference
} # }
```
