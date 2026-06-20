# Summarise per-trial metadata for a single video.

Uses paired landmark coordinates to determine the temporal bounds of
each trial, the origin, and the reference heading. Additional metadata
from \`file_tbl\` is merged into the result. Accepts landmark and track
data either as data frames or as \`Tracks\` objects.

## Usage

``` r
get_trial_limits(landmarks, track, file_tbl, vid_num)
```

## Arguments

- landmarks:

  Data frame or \`Tracks\` (two rows per trial) containing frame numbers
  and landmark coordinates.

- track:

  Data frame or \`Tracks\` of Cartesian coordinates for all frames in
  the video.

- file_tbl:

  Tibble returned by \[import_tracks()\] (optionally enriched by
  \[load_tracks()\] or \[load_tracks2()\]).

- vid_num:

  Index of the current video within \`file_tbl\`.

## Value

A tibble with one row per trial containing trial limits and reference
metadata.
