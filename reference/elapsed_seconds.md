# Elapsed time per observation of a Tracks object

Real elapsed time of each point from its own track's start, computed on
demand from the time column and (for frame-indexed time) the
\[frame_rate()\]. POSIXct time is used directly; numeric (frame) time
requires a frame rate.

## Usage

``` r
elapsed_seconds(ts, units = c("seconds", "minutes", "milliseconds"))
```

## Arguments

- ts:

  A \[Tracks\] object.

- units:

  \`"seconds"\` (default), \`"minutes"\`, or \`"milliseconds"\`.

## Value

A numeric vector aligned to the object's observations.

## See also

\[frame_rate()\], \[track_duration()\]
