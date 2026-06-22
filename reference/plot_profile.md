# Kinematics profile plot for a Tracks

Draws a per-observation kinematics metric against elapsed time – the
non-circular companion to \[radiate()\]. \`metric = "speed"\` plots
\[instantaneous_speed()\] (one line per track); \`metric = "turning"\`
plots \[angular_velocity()\] (one line per track); \`metric =
"direction"\` plots \[velocity_angle()\] (the movement direction) as
points.

## Usage

``` r
plot_profile(
  ts,
  metric = c("speed", "turning", "direction"),
  units = c("radians", "degrees"),
  colour_by = NULL,
  panel_by = NULL
)
```

## Arguments

- ts:

  A \`Tracks\`.

- metric:

  \`"speed"\` (default), \`"turning"\`, or \`"direction"\`.

- units:

  For \`metric = "turning"\` (per second) or \`metric = "direction"\`,
  \`"radians"\` (default) or \`"degrees"\`.

- colour_by, panel_by:

  Optional column names of \`as.data.frame(ts)\` to colour by / facet
  into panels. Default: one neutral series per track.

## Value

A \`ggplot2\` object.

## Details

Speed and turning rate are per-second, so a frame rate is required for
frame-indexed time (\[set_frame_rate()\]); POSIXct time is used
directly. With a distance calibration (\[set_distance_scale()\]) speed
is in physical units.

Direction is circular: it is drawn as points (not a line) because a line
would draw a false vertical bar across the 0/2\*pi seam. A rotating
track shows points spread across the range, and values near 0/2\*pi
(0/360 degrees) appear at both extremes – the honest representation.

## See also

\[instantaneous_speed()\], \[angular_velocity()\], \[velocity_angle()\],
\[elapsed_seconds()\], \[radiate()\], \[set_frame_rate()\]

## Examples

``` r
ts <- set_frame_rate(cpunctatus, 30)
plot_profile(ts, metric = "speed")

plot_profile(ts, metric = "direction", units = "degrees")
```
