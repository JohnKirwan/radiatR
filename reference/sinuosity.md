# Per-trajectory sinuosity for a Tracks

Computes \[path_sinuosity()\] (Benhamou 2004) for each trajectory in a
\`Tracks\`, ordering each trajectory's points by its time column when
one is recorded. Distance-calibrated: with a \`distance_scale\` set the
step lengths are in real units, so the reported sinuosity is in
\\1/\sqrt{\mathrm{unit}}\\.

## Usage

``` r
sinuosity(ts, x_col = ts@cols$x, y_col = ts@cols$y)
```

## Arguments

- ts:

  A \`Tracks\`.

- x_col, y_col:

  Names of the coordinate columns to use. Default to the \`Tracks\`'s
  recorded x/y columns (the real recorded positions).

## Value

A \`data.frame\` with one row per trajectory: the \`Tracks\`'s id column
and a numeric \`sinuosity\` column (\`\>= 0\`).

## See also

\[path_sinuosity()\], \[straightness_index()\], \[tortuosity_ratio()\],
\[set_distance_scale()\]
