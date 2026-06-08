# Per-trajectory tortuosity ratio for a TrajSet

Computes \[path_tortuosity()\] for each trajectory in a \`TrajSet\`,
ordering each trajectory's points by its time column when one is
recorded.

## Usage

``` r
tortuosity_ratio(ts, x_col = ts@cols$x, y_col = ts@cols$y)
```

## Arguments

- ts:

  A \`TrajSet\`.

- x_col, y_col:

  Names of the coordinate columns to use. Default to the \`TrajSet\`'s
  recorded x/y columns (the real arena positions), so the metric
  reflects the physical path rather than any display transform.

## Value

A \`data.frame\` with one row per trajectory: the \`TrajSet\`'s id
column and a numeric \`tortuosity\` column (\`\>= 1\`, possibly
\`Inf\`).

## See also

\[path_tortuosity()\], \[straightness_index()\]
