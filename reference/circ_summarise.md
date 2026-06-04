# Tidy circular summary of a grouped data frame

Computes circular summary statistics from a data frame column of angles.
Supports grouped tibbles and an explicit `.by` argument, returning one
row per group. Output `mean_dir` is always in radians; `mean_dir_deg` is
the same value converted to degrees.

## Usage

``` r
circ_summarise(
  data,
  col,
  units,
  .by = NULL,
  stats = c("n", "mean_dir", "mean_dir_deg", "resultant_R", "kappa"),
  display = circ_display()
)
```

## Arguments

- data:

  A data frame or grouped tibble.

- col:

  Unquoted or quoted name of the column containing angles.

- units:

  Units of the angle column: `"radians"` or `"degrees"`. No default –
  must be specified explicitly. Values are converted to radians
  internally before computation; output is always in radians
  (`mean_dir`) or degrees (`mean_dir_deg`). A warning is issued when the
  value range appears inconsistent with the declared units (e.g. values
  \> 2pi when `units = "radians"`). Suppress range warnings with
  `options(radiatR.check_units = FALSE)`.

- .by:

  Character vector of grouping column names. Overrides any `group_by()`
  groups on `data`.

- stats:

  Character vector selecting which statistics to compute. Order
  determines column order in the output. Valid values: `"n"`,
  `"mean_dir"`, `"mean_dir_deg"`, `"resultant_R"`, `"kappa"`. Default:
  all five.

- display:

  A \[\`circ_display\`\] object. When supplied, \`mean_dir_deg\` is
  converted using the display convention (clockwise, \`zero\` offset).
  When \`NULL\` (default), \`mean_dir_deg\` is the raw degree equivalent
  of the unit-circle radian angle.

## Value

An ungrouped `tibble` with group columns first followed by requested
stat columns in the order given in `stats`.

## Examples

``` r
hd <- data.frame(heading = c(0, pi/4, pi/2), arc = c("a", "a", "b"))
circ_summarise(hd, heading, units = "radians")
#> # A tibble: 1 × 5
#>       n mean_dir mean_dir_deg resultant_R kappa
#>   <int>    <dbl>        <dbl>       <dbl> <dbl>
#> 1     3    0.785           45       0.805    NA
circ_summarise(hd, heading, units = "radians", .by = "arc")
#> # A tibble: 2 × 6
#>   arc       n mean_dir mean_dir_deg resultant_R kappa
#>   <chr> <int>    <dbl>        <dbl>       <dbl> <dbl>
#> 1 a         2    0.393         67.5       0.924    NA
#> 2 b         1    1.57           0         1        NA
circ_summarise(hd, heading, units = "radians", .by = "arc",
               stats = c("n", "mean_dir"))
#> # A tibble: 2 × 3
#>   arc       n mean_dir
#>   <chr> <int>    <dbl>
#> 1 a         2    0.393
#> 2 b         1    1.57 
```
