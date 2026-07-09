# Per-trajectory net velocity for a Tracks

The net (average) velocity vector of each trajectory: its straight-line
displacement divided by its elapsed duration. The magnitude is the net
speed and \`atan2(vy, vx)\` the overall direction of travel (so a path
returning to its start has zero net velocity). Distance-calibrated when
a scale is set.

## Usage

``` r
track_velocity(ts, x_col = ts@cols$x, y_col = ts@cols$y)
```

## Arguments

- ts:

  A \`Tracks\`.

- x_col, y_col:

  Names of the coordinate columns. Default to the \`Tracks\`'s recorded
  x/y columns.

## Value

A \`data.frame\` with one row per trajectory: the id column and numeric
\`vx\`, \`vy\` (\`NA\` for tracks with fewer than two points or zero
duration).

## Details

Numeric (frame) time requires a frame rate (\[set_frame_rate()\]);
POSIXct time is used directly.

## See also

\[velocity_vector()\], \[track_speed()\], \[track_turning()\],
\[set_distance_scale()\]
