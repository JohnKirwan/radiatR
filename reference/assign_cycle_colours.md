# Assign cycling colour indices to trajectories

Creates a factor column that assigns each unique trajectory a colour
index in the range \`1:n\`, cycling back to 1 after every \`n\`
trajectories. When \`panel_col\` is supplied the cycle resets
independently within each panel so that trajectory 1 in every panel gets
index 1.

## Usage

``` r
assign_cycle_colours(
  data,
  id_col,
  n,
  panel_col = NULL,
  out_col = "cycle_colour"
)

assign_cycle_colors(data, id_col, n, panel_col = NULL, out_col = "cycle_colour")
```

## Arguments

- data:

  A data frame containing at least \`id_col\` and, if supplied,
  \`panel_col\`.

- id_col:

  Name of the column identifying individual trajectories.

- n:

  Number of colours to cycle through (positive integer), or a character
  vector of colour values whose length determines \`n\`.

- panel_col:

  Optional column name. When set the cycle restarts for each unique
  value of this column.

- out_col:

  Name of the new factor column added to \`data\`. Default
  \`"cycle_colour"\`.

## Value

\`data\` with an additional factor column named \`out_col\` (levels
\`"1"\` through \`"n"\`).

## Details

The resulting column can be passed to \`colour_col\` in \[radiate()\],
\[add_heading_points()\], or \[add_heading_vectors()\] to keep colours
consistent across layers.

## Examples

``` r
df <- data.frame(id = paste0("T", 1:12), panel = rep(c("A","B"), each = 6))
df <- assign_cycle_colours(df, id_col = "id", n = 4, panel_col = "panel")
table(df$panel, df$cycle_colour)
#>    
#>     1 2 3 4
#>   A 2 2 1 1
#>   B 2 2 1 1
```
