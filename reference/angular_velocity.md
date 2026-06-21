# Per-observation angular (turning-rate) velocity for a Tracks

The signed rate of change of travel direction at each point – how fast,
and which way, the trajectory is turning. Positive is counter-clockwise
(a left turn). Each interior point's turn (the angle between its
incoming and outgoing step) is divided by the centred time step; the
first and last point of every trajectory are \`NA\`.

## Usage

``` r
angular_velocity(
  ts,
  units = c("radians", "degrees"),
  x_col = ts@cols$x,
  y_col = ts@cols$y
)
```

## Arguments

- ts:

  A \`Tracks\`.

- units:

  \`"radians"\` per second (default) or \`"degrees"\` per second.

- x_col, y_col:

  Names of the coordinate columns. Default to the \`Tracks\`'s recorded
  x/y columns.

## Value

A numeric vector, one value per observation in \`ts@data\` order, \`NA\`
at each trajectory's first and last point.

## Details

Numeric (frame) time requires a frame rate (\[set_frame_rate()\]);
POSIXct time is used directly. The result is scale-free (an angle), so
no distance calibration is applied.

## See also

\[velocity_vector()\], \[instantaneous_speed()\], \[frame_rate()\]
