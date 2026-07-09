# Legacy helper to merge manifest metadata with a track table

Legacy helper to merge manifest metadata with a track table

## Usage

``` r
load_tracks(file_tbl, df, track_dir)
```

## Arguments

- file_tbl:

  Tibble returned by \[import_tracks()\].

- df:

  Data frame containing at least a \`file\` column and, optionally,
  \`arc\`, \`type\`, \`obstacle\`, and \`id\`.

- track_dir:

  Directory containing the track files (used for validation).

## Value

\`file_tbl\` with additional metadata columns bound in.
