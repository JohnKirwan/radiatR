# Duration of each track

Total elapsed time of each trajectory (its last point minus its first),
computed via \[elapsed_seconds()\].

## Usage

``` r
track_duration(ts, units = c("seconds", "minutes", "milliseconds"))
```

## Arguments

- ts:

  A \[Tracks\] object.

- units:

  \`"seconds"\` (default), \`"minutes"\`, or \`"milliseconds"\`.

## Value

A data frame with columns \`id\` and \`duration\`.

## See also

\[frame_rate()\], \[elapsed_seconds()\]

## Examples

``` r
head(track_duration(cpunctatus, units = "minutes"))
#>        id  duration
#> 1 10_10_1 24.833333
#> 2 10_11_1  5.333333
#> 3 10_12_1  3.916667
#> 4 10_13_1  6.583333
#> 5 10_14_1  2.416667
#> 6 10_15_1  4.416667
```
