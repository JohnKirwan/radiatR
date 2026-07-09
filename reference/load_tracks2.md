# Flexible metadata join for track tables

Flexible metadata join for track tables

## Usage

``` r
load_tracks2(file_tbl, df, track_dir, colnames)
```

## Arguments

- file_tbl:

  Tibble returned by \[import_tracks()\].

- df:

  Manifest data frame containing a \`file\` column.

- track_dir:

  Directory containing the track files (used for validation).

- colnames:

  Named character vector (or list) mapping the desired column names in
  the output to columns in \`df\`.

## Value

\`file_tbl\` with additional columns appended.
