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
