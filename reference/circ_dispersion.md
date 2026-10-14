# Per-group circular dispersion statistics for a dense heading series

Computes circular mean direction, mean resultant length *R*, circular
standard deviation, and sample size for each group. Designed for
within-trial summaries from
[`pose_to_headings`](https://johnkirwan.github.io/radiatR/reference/pose_to_headings.md)
or `derive_headings(..., rule = "bodypart_axis", frame_select = "all")`,
but accepts any data frame with an angle column.

## Usage

``` r
circ_dispersion(hd, group_col = NULL, angle_col = "heading", axial = FALSE)
```

## Arguments

- hd:

  Data frame containing headings in radians.

- group_col:

  Column(s) to group by (e.g. `"id"` for per-trial summaries). `NULL`
  treats the entire data frame as one group.

- angle_col:

  Name of the heading column. Default `"heading"`.

- axial:

  Logical. Treat the angles as axial (bidirectional, mod-pi) data:
  statistics are computed via the angle-doubling method and the mean is
  reported as an axis in \`\[0, pi)\` radians / \`\[0, 180)\` degrees.
  Default \`FALSE\` (ordinary directional data).

## Value

Data frame with columns `group_col` (if supplied), `mean_dir`,
`resultant_R`, `circ_sd`, `n`. Circular standard deviation is \\\sqrt{-2
\log R}\\.

## Examples

``` r
hd <- derive_headings(cpunctatus, rule = "distal", carry = "arc")
circ_dispersion(hd, group_col = "arc")
#>   arc  mean_dir resultant_R  circ_sd  n
#> 1  10 1.3766939  0.07055967 2.302736 29
#> 2  15 0.7363627  0.24477975 1.677734 30
#> 3  20 3.8069634  0.07326231 2.286355 27
#> 4  30 5.9212227  0.12936762 2.022423 37
#> 5  40 3.5642725  0.06536414 2.335715 20
#> 6  50 1.5873930  0.15790182 1.921344 26
#> 7   5 2.4205480  0.07181952 2.295038 31
#> 8   0 3.0747577  0.23531457 1.701077 35
```
