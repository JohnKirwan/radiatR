# Per-group circular dispersion statistics for a dense heading series

Computes circular mean direction, mean resultant length *R*, circular
standard deviation, and sample size for each group. Designed for
within-trial summaries from
[`pose_to_headings`](https://johnkirwan.github.io/radiatR/reference/pose_to_headings.md)
or `derive_headings(..., frame_select = "all")`, but accepts any data
frame with an angle column.

## Usage

``` r
circ_dispersion(hd, group_col = NULL, angle_col = "heading")
```

## Arguments

- hd:

  Data frame containing headings in radians.

- group_col:

  Column(s) to group by (e.g. `"id"` for per-trial summaries). `NULL`
  treats the entire data frame as one group.

- angle_col:

  Name of the heading column. Default `"heading"`.

## Value

Data frame with columns `group_col` (if supplied), `mean_dir`,
`resultant_R`, `circ_sd`, `n`. Circular standard deviation is \\\sqrt{-2
\log R}\\.
