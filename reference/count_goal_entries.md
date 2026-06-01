# Count entries into a goal zone for circular arena trajectories

For each trial, counts the number of times the trajectory enters a
circular zone of radius \`crossing_radius\` centred on the goal
location. Applicable to any circular arena experiment with a defined
goal (hidden platform in a water maze, reward zone in an open-field,
etc.).

## Usage

``` r
count_goal_entries(
  x,
  target_angle,
  target_radius = 1,
  crossing_radius = 0.15,
  coords = c("absolute", "relative")
)
```

## Arguments

- x:

  A \[\`TrajSet\`\] object with x/y (or rel_x/rel_y) columns registered.

- target_angle:

  Numeric. Radians. Direction of the goal from the arena centre.

- target_radius:

  Numeric. Distance of the goal from the arena centre. Default \`1\`
  (wall). Together with \`target_angle\` gives the goal position: \`gx =
  target_radius \* cos(target_angle)\`, \`gy = target_radius \*
  sin(target_angle)\`.

- crossing_radius:

  Numeric. Radius of the goal zone in unit-circle coordinates. Default
  \`0.15\` (15% of arena radius; roughly a 10 cm platform in a 60 cm
  pool).

- coords:

  Character. \`"absolute"\` (default) or \`"relative"\`. See
  \[zone_dwell()\].

## Value

A \`data.frame\` with one row per trial: \`id\` (character) and
\`n_entries\` (integer).

## Details

An "entry" is a \`FALSE -\> TRUE\` transition in the \`distance \<
crossing_radius\` sequence (ordered by time). An animal that starts
inside the zone on the first frame counts as one entry.

## See also

\[zone_dwell()\]

## Examples

``` r
if (FALSE) { # \dontrun{
# Water maze probe trial: former platform at 45 degrees (NE), at wall
entries <- count_goal_entries(ts, target_angle = pi / 4,
                              crossing_radius = 0.15)
# n_entries > 1 indicates memory of the platform location
} # }
```
