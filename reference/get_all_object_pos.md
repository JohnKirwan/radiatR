# Aggregate track positions across all videos in a manifest.

Iterates over the rows of \`file_tbl\`, reading the paired landmark and
track files for each video before computing trial limits and normalised
coordinates. The per-trial outputs are combined into a single tibble,
and the augmented trial limits are returned alongside it.

## Usage

``` r
get_all_object_pos(landmarks = NULL, track = NULL, file_tbl, track_dir)
```

## Arguments

- landmarks:

  Optional data frame or \`Tracks\` for the first entry. Retained for
  backwards compatibility; values are overwritten internally.

- track:

  Optional data frame or \`Tracks\` for the first entry.

- file_tbl:

  Tibble produced by \[import_tracks()\], optionally enriched via
  \[load_tracks()\] or \[load_tracks2()\].

- track_dir:

  Directory containing the landmark and track text files.

## Value

A \`Tracks\` combining the normalised observations for all valid trials
in the manifest. The aggregated trial limits are available via
\`meta\$trial_limits\`.
