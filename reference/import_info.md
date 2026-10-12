# Import landmark coordinates from text files

Import landmark coordinates from text files

## Usage

``` r
import_info(filename, cond_cols = NULL, file_tbl = NULL)
```

## Arguments

- filename:

  Path to the CSV file describing each track/landmark pair.

- cond_cols:

  Optional character vector of column names whose values should be
  concatenated to create a \`cond\` column.

- file_tbl:

  Optional tibble produced by \[import_tracks()\]. When supplied, the
  function verifies that the listed files are present in both sources.

## Value

A data frame containing the parsed metadata (and optional \`cond\`
column).

## Examples

``` r
manifest_path <- system.file("extdata", "millipede_trials.csv",
                             package = "radiatR")
manifest <- import_info(manifest_path, cond_cols = c("type", "arc"))
head(manifest)
#>    file arc id stimulus_period recorded_radian recorded_degree    type obstacle
#> 1 con_1   0  1               0       -1.473503       -84.42550 control     none
#> 2 con_2   0  2               0        1.295963        74.25323 control     none
#> 3 con_3   0  3               0       -0.314510       -18.02010 control     none
#> 4 con_5   0  5               0       -2.070025      -118.60370 control     none
#> 5 con_6   0  6               0       -2.907256      -166.57352 control     none
#> 6 con_7   0  7               0       -1.601138       -91.73845 control     none
#>        cond
#> 1 control_0
#> 2 control_0
#> 3 control_0
#> 4 control_0
#> 5 control_0
#> 6 control_0
```
