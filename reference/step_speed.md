# Per-step speed along a trajectory

Speed of each step as straight-line step distance divided by the elapsed
time of that step: \`sqrt(diff(x)^2 + diff(y)^2) / diff(seconds)\`. The
unit is the distance unit of \`x\`/\`y\` per second; for radiatR's
unit-circle coordinates that is radii per second.

## Usage

``` r
step_speed(x, y, seconds)
```

## Arguments

- x, y:

  Numeric vectors of ordered (in time) coordinates for one trajectory.

- seconds:

  Numeric vector, the elapsed time of each point in seconds (same length
  as \`x\`/\`y\`).

## Value

A numeric vector of per-step speeds, length \`length(x) - 1\`. A step is
\`NA\` when either endpoint is non-finite or its time increment is \`\<=
0\`; \`numeric(0)\` when fewer than two points are given.

## See also

\[track_speed()\] for a whole \`Tracks\`; \[elapsed_seconds()\].

## Examples

``` r
step_speed(x = 0:3, y = rep(0, 4), seconds = (0:3) / 30)   # 30 units/s
#> [1] 30 30 30
```
