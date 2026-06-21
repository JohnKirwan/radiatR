# Kinematics profile plot for a Tracks

Draws a per-observation kinematics metric against elapsed time, one line
per trajectory – the non-circular companion to \[radiate()\]. \`metric =
"speed"\` plots \[instantaneous_speed()\]; \`metric = "turning"\` plots
\[angular_velocity()\].

## Usage

``` r
plot_profile(
  ts,
  metric = c("speed", "turning"),
  units = c("radians", "degrees"),
  colour_by = NULL,
  panel_by = NULL
)
```

## Arguments

- ts:

  A \`Tracks\`.

- metric:

  \`"speed"\` (default) or \`"turning"\`.

- units:

  For \`metric = "turning"\`, \`"radians"\` (default) or \`"degrees"\`
  per second.

- colour_by, panel_by:

  Optional column names of \`as.data.frame(ts)\` to colour the lines by
  / facet into panels. Default: one neutral line per track.

## Value

A \`ggplot2\` object.

## Details

Speed and turning rate are per-second, so a frame rate is required for
frame-indexed time (\[set_frame_rate()\]); POSIXct time is used
directly. With a distance calibration (\[set_distance_scale()\]) speed
is in physical units.

## See also

\[instantaneous_speed()\], \[angular_velocity()\],
\[elapsed_seconds()\], \[radiate()\], \[set_frame_rate()\]

## Examples

``` r
ts <- set_frame_rate(cpunctatus, 30)
plot_profile(ts, metric = "speed")
```
