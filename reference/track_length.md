# Per-trajectory path length for a Tracks

Total distance travelled along each trajectory. With a distance
calibration set (\[set_distance_scale()\]) the result is in physical
units; otherwise in the units of the recorded \`x\`/\`y\` coordinates.

## Usage

``` r
track_length(ts, x_col = ts@cols$x, y_col = ts@cols$y)
```

## Arguments

- ts:

  A \`Tracks\`.

- x_col, y_col:

  Names of the coordinate columns. Default to the \`Tracks\`'s recorded
  x/y columns.

## Value

A \`data.frame\` with one row per trajectory: the id column and a
numeric \`length\` column.

## See also

\[set_distance_scale()\], \[track_speed()\], \[straightness_index()\]
