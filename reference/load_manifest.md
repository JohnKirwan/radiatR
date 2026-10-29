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
  normalize_xy = FALSE,
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

  Logical; normalise x/y to unit circle. Default `FALSE`. See
  \[tracks()\] for why `TRUE` is shape-preserving only, not calibration.

- dialect:

  Optional dialect name; passed to \[read_tracks()\].

- keep, drop:

  Column selection vectors passed to \[read_tracks()\].

- ...:

  Additional arguments forwarded to \[read_tracks()\].

## Value

A \`Tracks\` with metadata columns replicated for each observation.

## Examples

``` r
untar(system.file("extdata", "tracks.tar.gz", package = "radiatR"),
      exdir = tempdir())
track_dir <- file.path(tempdir(), "tracks")
# dtrack writes headerless, tab-separated frame, x, y (and a flag) columns
dtrack_loader <- function(x) {
  d <- utils::read.delim(x, header = FALSE, colClasses = "numeric")
  data.frame(id = tools::file_path_sans_ext(basename(x)),
             frame = d[[1]], x = d[[2]], y = d[[3]])
}
register_loader_dialect("dtrack", dtrack_loader, overwrite = TRUE)
file_tbl <- import_tracks(track_dir)[1:3, ]
manifest <- import_info(system.file("extdata", "millipede_trials.csv",
                                    package = "radiatR"))
manifest <- manifest[manifest$file %in% file_tbl$basename, ]
ts <- load_manifest(file_tbl, track_dir, manifest = manifest,
                    manifest_cols = c(arc = "arc", type = "type"),
                    dialect = "dtrack")
ts
#> Tracks: 3 trajectories, 412 observations
#> Columns: id='id', time='frame', angle='angle' (radians), x='x', y='y'
#>              id frame      x      y     angle basename          landmark
#> 1 10_10_point02     1 469.83 379.47 0.6794034    10_10 10_10_point01.txt
#> 2 10_10_point02     2 477.73 386.58 0.6803267    10_10 10_10_point01.txt
#> 3 10_10_point02     3 479.31 388.95 0.6817012    10_10 10_10_point01.txt
#> 4 10_10_point02     4 482.48 400.81 0.6931992    10_10 10_10_point01.txt
#> 5 10_10_point02     5 484.85 406.34 0.6975293    10_10 10_10_point01.txt
#> 6 10_10_point02     6 484.06 417.40 0.7115858    10_10 10_10_point01.txt
#>               track arc     type                              source_file
#> 1 10_10_point02.txt  10 stimulus /tmp/RtmpveuKAL/tracks/10_10_point02.txt
#> 2 10_10_point02.txt  10 stimulus /tmp/RtmpveuKAL/tracks/10_10_point02.txt
#> 3 10_10_point02.txt  10 stimulus /tmp/RtmpveuKAL/tracks/10_10_point02.txt
#> 4 10_10_point02.txt  10 stimulus /tmp/RtmpveuKAL/tracks/10_10_point02.txt
#> 5 10_10_point02.txt  10 stimulus /tmp/RtmpveuKAL/tracks/10_10_point02.txt
#> 6 10_10_point02.txt  10 stimulus /tmp/RtmpveuKAL/tracks/10_10_point02.txt
unique(ts@data[, c("id", "arc", "type")])
#>                id arc     type
#> 1   10_10_point02  10 stimulus
#> 300 10_11_point02  10 stimulus
#> 365 10_12_point02  10 stimulus
```
