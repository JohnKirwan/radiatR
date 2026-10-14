# Per-trajectory path length for a Tracks

Total distance travelled along each trajectory. With a distance
calibration set (\[set_distance_scale()\]) the result is in physical
units; otherwise in the units of the recorded \`x\`/\`y\` coordinates.

## Usage

``` r
track_length(ts, x_col = ts@cols$x, y_col = ts@cols$y)
```

## Arguments

- ts:

  A \`Tracks\`.

- x_col, y_col:

  Names of the coordinate columns. Default to the \`Tracks\`'s recorded
  x/y columns.

## Value

A \`data.frame\` with one row per trajectory: the id column and a
numeric \`length\` column.

## See also

\[set_distance_scale()\], \[track_speed()\], \[straightness_index()\]

## Examples

``` r
# unit-circle coordinates, so lengths are in arena radii
head(track_length(cpunctatus))
#>         trial_id    length
#> 10_1_1    10_1_1 1.3645581
#> 10_10_1  10_10_1 8.7387738
#> 10_11_1  10_11_1 1.2783589
#> 10_12_1  10_12_1 1.2376997
#> 10_13_1  10_13_1 1.5926105
#> 10_14_1  10_14_1 0.9365288
```
