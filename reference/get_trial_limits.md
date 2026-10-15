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

  Tibble returned by \[import_tracks()\]. Any columns beyond
  \`basename\`, \`landmark\` and \`track\` are carried onto the trial
  table.

- vid_num:

  Index of the current video within \`file_tbl\`.

## Value

A tibble with one row per trial containing trial limits and reference
metadata.

## Examples

``` r
untar(system.file("extdata", "tracks.tar.gz", package = "radiatR"),
      exdir = tempdir())
track_dir <- file.path(tempdir(), "tracks")
file_tbl <- import_tracks(track_dir)[1, ]
read_xy <- function(f) {
  d <- utils::read.delim(file.path(track_dir, f), header = FALSE)[1:3]
  stats::setNames(d, c("frame", "x", "y"))
}
landmarks <- read_xy(file_tbl$landmark)  # origin, then reference, per trial
track <- read_xy(file_tbl$track)
limits <- get_trial_limits(landmarks, track, file_tbl, vid_num = 1)
limits
#> # A tibble: 1 × 18
#>   first_f orig_x orig_y ref_x ref_y last_f ref_x_0 ref_y_0 ref_theta  r_px
#>     <dbl>  <dbl>  <dbl> <dbl> <dbl>  <dbl>   <dbl>   <dbl>     <dbl> <dbl>
#> 1       1   458.   375.  139.  392.    299  -0.999 -0.0545      3.20  319.
#> # ℹ 8 more variables: quadrant <chr>, video <chr>, order <chr>, vid_ord <chr>,
#> #   x0 <dbl>, y0 <dbl>, x1 <dbl>, y1 <dbl>
```
