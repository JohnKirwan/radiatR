# Per-observation velocity vector for a Tracks

The velocity components (\`vx\`, \`vy\`) at each point, aligned to the
\`Tracks\`'s rows. Each point carries the velocity of the step that ends
at it (step displacement divided by its elapsed time); the first point
of every trajectory is \`NA\`. The magnitude is
\[instantaneous_speed()\] and the direction is \`atan2(vy, vx)\`.

## Usage

``` r
velocity_vector(ts, x_col = ts@cols$x, y_col = ts@cols$y)
```

## Arguments

- ts:

  A \`Tracks\`.

- x_col, y_col:

  Names of the coordinate columns. Default to the \`Tracks\`'s recorded
  x/y columns.

## Value

A \`data.frame\` with columns \`vx\` and \`vy\`, one row per observation
in \`ts@data\` order (\`NA\` at each trajectory's first point).

## Details

Numeric (frame) time requires a frame rate (\[set_frame_rate()\]);
POSIXct time is used directly. With a distance calibration
(\[set_distance_scale()\]) the components are in physical units per
second; otherwise coordinate units per second.

## See also

\[instantaneous_speed()\], \[angular_velocity()\],
\[set_distance_scale()\]
