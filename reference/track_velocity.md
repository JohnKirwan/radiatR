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

## Examples

``` r
tv <- track_velocity(cpunctatus)
head(tv)
#>         trial_id            vx            vy
#> 10_1_1    10_1_1  0.0016297844  0.0018342959
#> 10_10_1  10_10_1  0.0000116351 -0.0005803454
#> 10_11_1  10_11_1  0.0004857098  0.0031564992
#> 10_12_1  10_12_1 -0.0027444592  0.0033678666
#> 10_13_1  10_13_1  0.0000000000 -0.0025758280
#> 10_14_1  10_14_1 -0.0030766624  0.0052025950
# net direction of travel of each track
head(atan2(tv$vy, tv$vx))
#> [1]  0.8443675 -1.5507504  1.4181177  2.2545541 -1.5707963  2.1048466
```
