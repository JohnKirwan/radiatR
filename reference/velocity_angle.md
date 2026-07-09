# Per-row movement direction

The heading of the velocity vector at each observation – the direction
of travel from the previous point – as a per-row vector aligned to the
Tracks' rows. The directional sibling of \[instantaneous_speed()\]; the
per-observation counterpart of a velocity-based \[derive_headings()\]
rule (which gives one heading per track).

## Usage

``` r
velocity_angle(
  ts,
  units = c("radians", "degrees"),
  x_col = ts@cols$x,
  y_col = ts@cols$y
)
```

## Arguments

- ts:

  A \[Tracks-class\] object.

- units:

  \`"radians"\` (default, \`\[0, 2\*pi)\`) or \`"degrees"\` (\`\[0,
  360)\`).

- x_col, y_col:

  Position columns. Default the Tracks' \`x\`/\`y\`.

## Value

A numeric vector, one element per row of \`ts\`, \`NA\` at each track's
first row.

## Details

Returned in the package's heading convention: radians in \`\[0, 2\*pi)\`
(or degrees in \`\[0, 360)\`), \`0\` = the positive x-axis, increasing
counterclockwise – the same frame as \[derive_headings()\] output, so it
drops straight into \[circ_summary()\] / \[radiate()\]. Map to a display
orientation with \[circ_display()\] when plotting.

Direction needs a step, so the first row of each track is \`NA\`. The
result is invariant to any \[set_distance_scale()\] (the scale cancels
in the ratio).

## See also

\[velocity_vector()\], \[instantaneous_speed()\],
\[angular_velocity()\], \[circ_summary()\]

## Examples

``` r
ts <- simulate_tracks(conditions = data.frame(n_trials = 3L),
                      n_points = 20, seed = 1, output = "trajset")
ts <- set_frame_rate(ts, 30)
head(velocity_angle(ts))
#> [1]       NA 5.538253 5.704025 5.734100 5.428359 5.310436
```
