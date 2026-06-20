# Intersection helper using track rows

Convenience wrapper that accepts a data frame (or \`Tracks@data\`)
containing coordinates and evaluates the intersection between two rows.

## Usage

``` r
line_circle_intercept_df(df, row_in, row_out)
```

## Arguments

- df:

  Data frame with \`x\` and \`y\` columns.

- row_in, row_out:

  Row indices (or names) identifying the start and end of the vector.

## Value

Tibble with \`x_int\`/\`y_int\`.
