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
if (FALSE) { # \dontrun{
manifest <- import_info("trials_list.csv", cond_cols = c("type", "arc"))
} # }
```
