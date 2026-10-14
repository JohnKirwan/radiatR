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

## Examples

``` r
untar(system.file("extdata", "tracks.tar.gz", package = "radiatR"),
      exdir = tempdir())
track_dir <- file.path(tempdir(), "tracks")
file_tbl <- import_tracks(track_dir)[1:3, ]
manifest <- import_info(system.file("extdata", "millipede_trials.csv",
                                    package = "radiatR"))
manifest <- manifest[manifest$file %in% file_tbl$basename, ]
ts <- get_all_object_pos(file_tbl = file_tbl, track_dir = track_dir,
                         manifest = manifest)
#> 5 points across 2 trials exceeded the unit circle boundary (radius > 1); coordinates left unscaled.
ts
#> Tracks: 3 trajectories, 412 observations
#> Columns: id='trial_id', time='frame', angle='rel_theta' (radians), x='trans_x', y='trans_y', rel_x='rel_x', rel_y='rel_y'
#> Transform steps: unit_circle_mapping 
#> # A tibble: 6 × 10
#>   trial_id frame     x     y trans_x trans_y rel_theta video order vid_ord
#>   <chr>    <dbl> <dbl> <dbl>   <dbl>   <dbl>     <dbl> <chr> <chr> <chr>  
#> 1 10_10_1      1  470.  379.  0.0371 -0.0149      2.71 10_10 1     10_10_1
#> 2 10_10_1      2  478.  387.  0.0619 -0.0371      2.55 10_10 1     10_10_1
#> 3 10_10_1      3  479.  389.  0.0669 -0.0446      2.50 10_10 1     10_10_1
#> 4 10_10_1      4  482.  401.  0.0768 -0.0818      2.27 10_10 1     10_10_1
#> 5 10_10_1      5  485.  406.  0.0842 -0.0991      2.22 10_10 1     10_10_1
#> 6 10_10_1      6  484.  417.  0.0818 -0.134       2.06 10_10 1     10_10_1
ts@meta$trial_limits[, c("video", "arc", "type", "ref_theta")]
#> # A tibble: 3 × 4
#>   video   arc type     ref_theta
#>   <chr> <int> <chr>        <dbl>
#> 1 10_10    10 stimulus    3.20  
#> 2 10_11    10 stimulus    1.55  
#> 3 10_12    10 stimulus    0.0772
```
