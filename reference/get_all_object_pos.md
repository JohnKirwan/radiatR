# Aggregate track positions across all videos in a manifest.

Iterates over the rows of \`file_tbl\`, reading the paired landmark and
track files for each video before computing trial limits and normalised
coordinates. The per-trial outputs are combined into a single tibble,
and the augmented trial limits are returned alongside it. Supply
\`manifest\` to join per-file metadata (matched on \`file\`) onto every
trial.

## Usage

``` r
get_all_object_pos(
  landmarks = NULL,
  track = NULL,
  file_tbl,
  track_dir,
  manifest = NULL,
  manifest_cols = NULL
)
```

## Arguments

- landmarks:

  Optional data frame or \`Tracks\` for the first entry. Retained for
  backwards compatibility; values are overwritten internally.

- track:

  Optional data frame or \`Tracks\` for the first entry.

- file_tbl:

  Tibble produced by \[import_tracks()\].

- track_dir:

  Directory containing the landmark and track text files.

- manifest:

  Optional data frame (e.g. from \[import_info()\]) with a \`file\`
  column matching \`file_tbl\$basename\`; its other columns are joined
  onto \`file_tbl\` and carried onto every trial.

- manifest_cols:

  Optional named character vector (or list) mapping new column names to
  columns of \`manifest\`. When \`NULL\`, all columns aside from
  \`file\` are carried over with the same names.

## Value

A \`Tracks\` combining the normalised observations for all valid trials
in the manifest. The aggregated trial limits are available via
\`meta\$trial_limits\`. Each landmark and track file is tab-separated
with frame, x and y in the first three fields of every line (further
fields are not read); a cell that is not a number stops with a
\`radiatR_malformed_values\` error, and a line with fewer than three
fields or an odd number of landmark rows with a
\`radiatR_malformed_file\` error, each naming the file. When no file
holds a valid trial the call stops with a \`radiatR_no_valid_trials\`
error.
