# Per-trajectory speed for a Tracks, in real units

Summarises each trajectory's speed (distance per second). Step speeds
come from \[step_speed()\] using the track's elapsed time from
\[elapsed_seconds()\]; the per-track summary is the chosen \`stat\` of
those step speeds.

## Usage

``` r
track_speed(
  ts,
  stat = c("mean", "max", "median"),
  x_col = ts@cols$x,
  y_col = ts@cols$y
)
```

## Arguments

- ts:

  A \`Tracks\`.

- stat:

  Per-track reduction of the step speeds: \`"mean"\` (default),
  \`"max"\`, or \`"median"\`.

- x_col, y_col:

  Names of the coordinate columns. Default to the \`Tracks\`'s recorded
  x/y columns.

## Value

A \`data.frame\` with one row per trajectory: the id column and a
numeric \`speed\` column (\`NA\` for tracks with fewer than two usable
points).

## Details

Numeric (frame) time requires a frame rate (\[set_frame_rate()\]);
POSIXct time is used directly. With the default coordinate columns the
unit is radii per second, because radiatR normalises trajectories to a
unit circle.

## See also

\[step_speed()\], \[elapsed_seconds()\], \[track_duration()\],
\[straightness_index()\]
