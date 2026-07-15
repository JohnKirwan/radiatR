# Derive trial-level track positions in polar coordinates.

Using the trial limits returned by \[get_trial_limits()\], this helper
extracts the corresponding rows from a track data frame or \`Tracks\`,
centres and scales the coordinates, and computes angles in both absolute
and reference-relative frames. The function optionally controls how
inner/outer radius crossings are selected.

## Usage

``` r
get_tracked_object_pos(
  trial_limits,
  track,
  circ0 = 0.1,
  circ1 = 0.2,
  radius_criterion = c("first_past", "closest"),
  .report_oob = TRUE
)
```

## Arguments

- trial_limits:

  Data frame produced by \[get_trial_limits()\].

- track:

  Data frame or \`Tracks\` of Cartesian coordinates for the entire
  video.

- circ0:

  Inner radius threshold (default \`0.1\`).

- circ1:

  Outer radius threshold (default \`0.2\`).

- radius_criterion:

  Strategy for choosing the radius landmarks. \`"first_past"\` selects
  the first point beyond each threshold, while \`"closest"\` chooses the
  closest sample to the specified radius.

- .report_oob:

  Internal. When \`TRUE\` (default), emit a single message summarising
  any track points that fall outside the unit circle. Callers that loop
  over many files (e.g. \[get_all_object_pos()\]) set this to \`FALSE\`
  and read the tally from \`meta\$oob_points\`/\`meta\$oob_trials\` to
  report once.

## Value

A \`Tracks\` containing all valid trial observations. The corresponding
trial limits (including \`valid_track\` flags) are stored in
\`meta\$trial_limits\`.
