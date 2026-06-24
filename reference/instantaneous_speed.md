# Per-observation instantaneous speed for a Tracks

Instantaneous speed at each point, aligned to the \`Tracks\`'s rows (the
per-observation sibling of \[elapsed_seconds()\]). Each point carries
the speed of the step that ends at it; the first point of every
trajectory is \`NA\`. Speeds come from \[step_speed()\] using the
track's elapsed time from \[elapsed_seconds()\].

## Usage

``` r
instantaneous_speed(ts, x_col = ts@cols$x, y_col = ts@cols$y)
```

## Arguments

- ts:

  A \`Tracks\`.

- x_col, y_col:

  Names of the coordinate columns. Default to the \`Tracks\`'s recorded
  x/y columns.

## Value

A numeric vector, one value per observation in \`ts@data\` order, \`NA\`
at each trajectory's first point.

## Details

Numeric (frame) time requires a frame rate (\[set_frame_rate()\]);
POSIXct time is used directly. With the default coordinate columns the
unit is radii per second.

## See also

\[step_speed()\], \[track_speed()\], \[elapsed_seconds()\]
