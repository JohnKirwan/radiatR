# Speed distribution histogram for a Tracks

A pooled histogram of step speeds (\[instantaneous_speed()\]) across all
tracks, annotated with the pooled median and coefficient of variation
(CV = sd / mean, a movement-regularity descriptor). The companion
distribution view to the time/scatter plots of \[plot_profile()\] /
\[plot_speed_direction()\].

## Usage

``` r
plot_speed_histogram(ts, max_speed = NULL, bins = 30)
```

## Arguments

- ts:

  A \`Tracks\`. Needs a frame rate for frame-indexed time
  (\[set_frame_rate()\]); POSIXct time is used directly.

- max_speed:

  Speed-axis cap: \`NULL\` (default, 99.5 number (hard cap), or \`Inf\`
  (no clip).

- bins:

  Number of bars across the (clipped) speed axis. Default \`30\`.

## Value

A \`ggplot2\` object.

## Details

The speed axis is robustly clipped by default (\`max_speed = NULL\`
zooms to the 99.5 bulk of the distribution; a caption reports how many
steps are off-scale. The clip is a view zoom (\`coord_cartesian\`), so
no data are dropped. Set \`max_speed\` to a number for a hard cap, or
\`Inf\` for the full raw range.

## See also

\[instantaneous_speed()\], \[plot_profile()\],
\[plot_speed_direction()\]

## Examples

``` r
ts <- set_frame_rate(cpunctatus, 30)
plot_speed_histogram(ts)
```
