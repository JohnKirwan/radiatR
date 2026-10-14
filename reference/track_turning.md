# Per-trajectory turning-rate summary for a Tracks

Summarises each trajectory's \[angular_velocity()\] (signed turning
rate, counter-clockwise positive) by the chosen statistic.

## Usage

``` r
track_turning(
  ts,
  stat = c("mean_abs", "mean", "max_abs", "median_abs"),
  units = c("radians", "degrees"),
  x_col = ts@cols$x,
  y_col = ts@cols$y
)
```

## Arguments

- ts:

  A \`Tracks\`.

- stat:

  \`"mean_abs"\` (default) the typical turning magnitude; \`"mean"\` the
  signed net bias (opposite turns cancel); \`"max_abs"\` the sharpest
  turn; \`"median_abs"\` a robust turning magnitude.

- units:

  \`"radians"\` per second (default) or \`"degrees"\` per second.

- x_col, y_col:

  Names of the coordinate columns. Default to the \`Tracks\`'s recorded
  x/y columns.

## Value

A \`data.frame\` with one row per trajectory: the id column and a
numeric \`turning\` column (\`NA\` for tracks with fewer than three
points).

## See also

\[angular_velocity()\], \[track_velocity()\], \[track_speed()\]

## Examples

``` r
head(track_turning(cpunctatus, units = "degrees"))
#>         trial_id  turning
#> 10_1_1    10_1_1 7.485018
#> 10_10_1  10_10_1 4.021752
#> 10_11_1  10_11_1 6.795136
#> 10_12_1  10_12_1 5.855719
#> 10_13_1  10_13_1 6.088549
#> 10_14_1  10_14_1 2.343546
```
