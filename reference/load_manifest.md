# Load trajectories listed in a file table into a Tracks

This high-level helper combines the file discovery tibble returned by
\[import_tracks()\] with optional metadata from an experiment manifest,
reads each track file using \[read_tracks()\], and merges the results
into a single \`Tracks\`.

## Usage

``` r
load_manifest(
  file_tbl,
  track_dir,
  manifest = NULL,
  manifest_cols = NULL,
  mapping = NULL,
  angle_unit = c("radians", "degrees", "auto"),
  time_type = c("auto", "posix", "seconds", "frames"),
  tz = "UTC",
  fps = NULL,
  normalize_xy = TRUE,
  dialect = NULL,
  keep = NULL,
  drop = NULL,
  ...
)
```

## Arguments

- file_tbl:

  Tibble returned by \[import_tracks()\], containing at least the
  columns \`basename\` and \`track\`.

- track_dir:

  Directory containing the track files on disk.

- manifest:

  Optional data frame with a \`file\` column that matches
  \`file_tbl\$basename\`.

- manifest_cols:

  Optional named character vector (or list) mapping new column names in
  \`file_tbl\` to column names present in \`manifest\`. When \`NULL\`,
  all columns aside from \`file\` are carried over with the same names.

- mapping:

  Optional explicit column mapping passed to \[read_tracks()\].

- angle_unit, time_type, tz, fps:

  Passed to \[read_tracks()\].

- normalize_xy:

  Logical; normalise x/y to unit circle. Default `TRUE`.

- dialect:

  Optional dialect name; passed to \[read_tracks()\].

- keep, drop:

  Column selection vectors passed to \[read_tracks()\].

- ...:

  Additional arguments forwarded to \[read_tracks()\].

## Value

A \`Tracks\` with metadata columns replicated for each observation.
