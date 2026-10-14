# Per-trajectory straightness index for a Tracks

Computes \[path_straightness()\] for each trajectory in a \`Tracks\`,
ordering each trajectory's points by its time column when one is
recorded.

## Usage

``` r
straightness_index(ts, x_col = ts@cols$x, y_col = ts@cols$y)
```

## Arguments

- ts:

  A \`Tracks\`.

- x_col, y_col:

  Names of the coordinate columns to use. Default to the \`Tracks\`'s
  recorded x/y columns (the real recorded positions), so the metric
  reflects the physical path rather than any display transform.

## Value

A \`data.frame\` with one row per trajectory: the \`Tracks\`'s id column
and a numeric \`straightness\` column.

## See also

\[path_straightness()\], \[tortuosity_ratio()\]

## Examples

``` r
head(straightness_index(cpunctatus))
#>         trial_id straightness
#> 10_1_1    10_1_1   0.71927750
#> 10_10_1  10_10_1   0.09897137
#> 10_11_1  10_11_1   0.79943750
#> 10_12_1  10_12_1   0.82488090
#> 10_13_1  10_13_1   0.63885806
#> 10_14_1  10_14_1   0.93581201
```
