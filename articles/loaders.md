# Working with Loaders

## Overview

`radiatR` provides a flexible loader framework that turns tracking
exports from a wide variety of systems into `Tracks` objects. The
helpers cover three typical tasks:

1.  Reading experiment manifests
    ([`import_info()`](https://johnkirwan.github.io/radiatR/reference/import_info.md)
    and
    [`import_tracks()`](https://johnkirwan.github.io/radiatR/reference/import_tracks.md)).
2.  Ingesting trajectory tables with
    [`read_tracks()`](https://johnkirwan.github.io/radiatR/reference/read_tracks.md)
    or
    [`read_tracks_dir()`](https://johnkirwan.github.io/radiatR/reference/read_tracks_dir.md).
3.  Extending the loader registry with
    [`register_loader_dialect()`](https://johnkirwan.github.io/radiatR/reference/register_loader_dialect.md)
    for custom formats.

This vignette walks through each step using the bundled millipede
example data and finishes with a user-defined loader for a custom
format.

## Loading a Real Experiment

The package ships the trials from a *Cylindroiulus punctatus*
(millipede) visual orientation experiment. Tracks are stored as paired
tab-separated text files: `_point01.txt` holds two landmark rows (the
origin + target location on the circumference); `_point02.txt` holds the
full per-frame xy trajectory.

### Step 1 — discover files

[`import_tracks()`](https://johnkirwan.github.io/radiatR/reference/import_tracks.md)
scans a directory for `_point01.txt` / `_point02.txt` pairs and returns
a tibble of basenames.

``` r

track_dir <- system.file("extdata", "tracks", package = "radiatR")
file_tbl  <- import_tracks(track_dir)
head(file_tbl)
#> # A tibble: 6 × 3
#>   basename landmark          track            
#>   <chr>    <chr>             <chr>            
#> 1 10_1     10_1_point01.txt  10_1_point02.txt 
#> 2 10_10    10_10_point01.txt 10_10_point02.txt
#> 3 10_11    10_11_point01.txt 10_11_point02.txt
#> 4 10_12    10_12_point01.txt 10_12_point02.txt
#> 5 10_13    10_13_point01.txt 10_13_point02.txt
#> 6 10_14    10_14_point01.txt 10_14_point02.txt
```

### Step 2 — read the manifest

[`import_info()`](https://johnkirwan.github.io/radiatR/reference/import_info.md)
parses the trial manifest CSV. Use `cond_cols` to generate a compound
condition column from multiple metadata fields.

``` r

manifest_path <- system.file("extdata", "millipede_trials.csv", package = "radiatR")
manifest <- import_info(manifest_path, cond_cols = c("type", "arc"))
head(manifest)
#>    file arc id stimulus_period recorded_radian recorded_degree    type obstacle
#> 1 con_1   0  1               0       -1.473503       -84.42550 control     none
#> 2 con_2   0  2               0        1.295963        74.25323 control     none
#> 3 con_3   0  3               0       -0.314510       -18.02010 control     none
#> 4 con_5   0  5               0       -2.070025      -118.60370 control     none
#> 5 con_6   0  6               0       -2.907256      -166.57352 control     none
#> 6 con_7   0  7               0       -1.601138       -91.73845 control     none
#>        cond
#> 1 control_0
#> 2 control_0
#> 3 control_0
#> 4 control_0
#> 5 control_0
#> 6 control_0
```

### Step 3 — extract and normalise trajectories

[`get_all_object_pos()`](https://johnkirwan.github.io/radiatR/reference/get_all_object_pos.md)
reads each file pair, uses the landmark rows to establish the
unit-circle geometry, and returns a `Tracks` with normalised unit-circle
coordinates. Passing `manifest` joins its columns (arc, type, obstacle,
id, …) onto every trial, matching on the `file` column; use
`manifest_cols` to rename or restrict them.

``` r

ts <- suppressWarnings(get_all_object_pos(file_tbl = file_tbl, track_dir = track_dir,
                                          manifest = manifest))
#> 478 points across 106 trials exceeded the unit circle boundary (radius > 1); coordinates left unscaled.
ts
#> Tracks: 235 trajectories, 44331 observations
#> Columns: id='trial_id', time='frame', angle='rel_theta' (radians), x='trans_x', y='trans_y', rel_x='rel_x', rel_y='rel_y'
#> Transform steps: unit_circle_mapping 
#> # A tibble: 6 × 10
#>   trial_id frame     x     y trans_x  trans_y rel_theta video order vid_ord
#>   <chr>    <int> <dbl> <dbl>   <dbl>    <dbl>     <dbl> <chr> <chr> <chr>  
#> 1 10_1_1       1  456.  372. 0.00511  0           6.21  10_1  1     10_1_1 
#> 2 10_1_1       2  458.  370. 0.0128   0.00770     0.464 10_1  1     10_1_1 
#> 3 10_1_1       3  456.  370. 0.00511  0.00770     0.908 10_1  1     10_1_1 
#> 4 10_1_1       4  454.  370. 0        0.00770     1.49  10_1  1     10_1_1 
#> 5 10_1_1       5  456.  377. 0.00511 -0.0153      4.96  10_1  1     10_1_1 
#> 6 10_1_1       6  459.  382. 0.0154  -0.0307      5.10  10_1  1     10_1_1
head(ts@meta$trial_limits[, c("video", "arc", "type", "obstacle")])
#> # A tibble: 6 × 4
#>   video   arc type     obstacle
#>   <chr> <int> <chr>    <chr>   
#> 1 10_1     10 stimulus none    
#> 2 10_10    10 stimulus none    
#> 3 10_11    10 stimulus none    
#> 4 10_12    10 stimulus none    
#> 5 10_13    10 stimulus none    
#> 6 10_14    10 stimulus none
```

## dtrack format

The bundled example data was tracked with **dtrack**
(<https://bitbucket.org/jochensmolka/dtrack>), a desktop tracking tool
for video recordings of freely-moving animals. The data are from:

> Kirwan J.D. & Nilsson D.-E. (2019). *A millipede compound eye
> mediating low-resolution vision.* Vision Research 165, 36–44.
> <https://doi.org/10.1016/j.visres.2019.09.003>

dtrack exports headerless, tab-separated text files with columns
`frame`, `x`, `y`, and a fourth confidence/flag column. `radiatR` has no
built-in dtrack dialect: the *Registering a Custom Dialect* section
below writes one in a few lines and uses it to read these files.

The `_point01` / `_point02` role split used in the bundled data —
landmarks in one file, trajectory in the other — is specific to this
experiment and is not a general dtrack convention.

## Tracking-tool format support

`radiatR` reads tabular (CSV/TSV) exports from many tracking tools
through named *dialects*. Each dialect maps a tool’s column conventions
onto the `Tracks` model of one position per individual per frame. Pass
the dialect name to `read_tracks(path, dialect = "name")`; tool-specific
options go through `dialect_args`.

The table below summarises what is and is not currently handled. Two
limitations are general:

- **Tabular text only.** The built-in dialects read CSV/TSV (and, where
  the optional packages are installed, Excel and Parquet). Native binary
  formats — such as NumPy `.npz` or MATLAB `.mat` — are not read.
- **Position, not posture.** `radiatR` models a single position per
  individual per frame. Per-frame *posture* data (variable-length
  midline or outline polylines) is outside this model. Where a tool
  tracks discrete body keypoints, those can be loaded as extra columns
  and turned into a body-axis heading with the `bodypart_axis` rule or
  [`pose_to_headings()`](https://johnkirwan.github.io/radiatR/reference/pose_to_headings.md);
  dense posture polylines are not ingested.

| Dialect | Handled | Not handled |
|----|----|----|
| `trex` | Positional CSV: plain `X`/`Y`, the `#wcentroid` / `#centroid` / `#pcentroid` centroid variants, and TRex’s `(cm)` / `(px)` unit annotations in headers. Per-individual `_fishN` files (id from filename) and aggregated files with an id column. | Native `.npz` export; posture export (`midline_points`, `outline_points`). |
| `deeplabcut` / `deeplabcut_multiheader` | Multi-bodypart CSV; single bodypart, or a likelihood-weighted centroid of several; three-row scorer/bodypart/coord header. Bodypart columns are preserved for `bodypart_axis`. | Native HDF5 (`.h5`) export. |
| `sleap` | Analysis CSV: `<node>.x` / `.y` / `.score`; multi-node centroid; `track` and `frame_idx`. | Native `.slp` / HDF5 export. |
| `ethovision` | Centre position, and multi-zone exports (nose, tail, etc.); zone centroid or single-zone selection. | Native `.xlsx` (export to CSV first). |
| `anymaze` | Centre position, optional nose/tail zones, units row. | — |
| `trackmate` | `TRACK_ID` / `FRAME` / `POSITION_X` / `POSITION_Y` CSV. | Native TrackMate `.xml`. |
| `idtrackerai_wide`, `toxtrac` | Their standard CSV/TSV column layouts. | Tool-specific binary or session formats. |

Built-in dialects are kept only for tracking software that is widely
used and still developed. When a tool’s export is not directly
supported, two escape hatches remain: convert to a tidy CSV and use
[`read_tracks()`](https://johnkirwan.github.io/radiatR/reference/read_tracks.md)
with an explicit `mapping`, or register a custom dialect (see
*Registering a Custom Dialect* below).

## Bundled single-file examples

The package ships small real exports from two tools so the dialects can
be tried without any external data. Each loads with one
[`read_tracks()`](https://johnkirwan.github.io/radiatR/reference/read_tracks.md)
call, pointing the `dialect` argument at the matching tool.

**DeepLabCut** — a three-row scorer/bodypart/coord header; by default
the position is the likelihood-weighted centroid of all bodyparts, and
each bodypart’s coordinates are retained for use with the
`bodypart_axis` rule.

``` r

dlc_path <- system.file("extdata", "dlc_CollectedData_Pranav.csv",
                        package = "radiatR")
ts_dlc <- read_tracks(dlc_path, dialect = "deeplabcut_multiheader")
ts_dlc
#> Tracks: 1 trajectories, 116 observations
#> Columns: id='id', time='time', angle='angle' (radians), x='x', y='y'
#>   id time        x        y snout_x snout_y leftear_x leftear_y rightear_x
#> 1  1    1 40.60850 233.5307  21.521 265.428    33.819   265.941     19.984
#> 2  1    2 34.58775 272.0897  10.248 288.487    19.984   297.198     12.298
#> 3  1    3 46.37325 325.3802  24.596 354.075    38.431   354.075     23.058
#> 4  1    4 79.16725 343.1865  73.787 374.572    78.911   366.373     57.390
#> 5  1    5 65.07625 317.9502  38.431 333.066    50.729   341.777     39.968
#> 6  1    6 53.41900 326.7892  23.571 327.430    30.745   339.215     28.183
#>   rightear_y tailbase_x tailbase_y    angle
#> 1    250.056     87.110    152.698 1.398629
#> 2    281.313     95.821    221.361 1.444356
#> 3    337.166     99.408    256.205 1.429230
#> 4    361.761    106.581    270.040 1.344079
#> 5    323.331    131.177    273.627 1.368910
#> 6    321.793    131.177    318.719 1.408763
```

**SLEAP** — an analysis CSV with `<node>.x` / `.y` / `.score` columns,
`track` as the individual and `frame_idx` as time.

``` r

sleap_path <- system.file("extdata", "sleap_example.csv", package = "radiatR")
ts_sleap <- read_tracks(sleap_path, dialect = "sleap")
ts_sleap
#> Tracks: 1 trajectories, 1 observations
#> Columns: id='id', time='time', angle='angle' (radians), x='x', y='y'
#>   id time        x        y      a_x      a_y      b_x      b_y     angle
#> 1  1    0 242.2826 195.6278 205.9301 187.8896 278.6352 203.3659 0.6792588
```

The bundled dtrack example is larger and is shown in full in the
*Loading a Real Experiment* section above.

## Reading Tabular Trajectories

For data already in a data frame or CSV,
[`read_tracks()`](https://johnkirwan.github.io/radiatR/reference/read_tracks.md)
is the central entry point.

``` r

example_df <- data.frame(
  id   = rep(c("A", "B"), each = 4),
  time = rep(seq(0, 3), times = 2),
  x    = c(1, 1.2, 1.4, 1.5, -0.2, -0.1, 0, 0.15),
  y    = c(0, 0.1, 0.3, 0.4, 1.0, 1.1, 1.3, 1.5)
)

ts_df <- read_tracks(example_df, mapping = list(id = "id", time = "time", x = "x", y = "y"))
ts_df
#> Tracks: 2 trajectories, 8 observations
#> Columns: id='id', time='time', angle='angle' (radians), x='x', y='y'
#>   id time    x   y      angle
#> 1  A    0  1.0 0.0 0.00000000
#> 2  A    1  1.2 0.1 0.08314123
#> 3  A    2  1.4 0.3 0.21109333
#> 4  A    3  1.5 0.4 0.26060239
#> 5  B    0 -0.2 1.0 1.76819189
#> 6  B    1 -0.1 1.1 1.66145621
```

To batch a directory of CSV files,
[`read_tracks_dir()`](https://johnkirwan.github.io/radiatR/reference/read_tracks_dir.md)
calls
[`read_tracks()`](https://johnkirwan.github.io/radiatR/reference/read_tracks.md)
for each match and row-binds the result.

``` r

csv_dir <- tempfile()
dir.create(csv_dir)

write.csv(example_df, file.path(csv_dir, "trial01.csv"), row.names = FALSE)
write.csv(transform(example_df, id = paste0(id, "_2")),
          file.path(csv_dir, "trial02.csv"), row.names = FALSE)

dset <- read_tracks_dir(csv_dir, pattern = "\\.csv$")
length(dset)
#> [1] 4

unlink(csv_dir, recursive = TRUE)
```

## Manifest Helpers (Synthetic Example)

For a quick demonstration without real files,
[`import_info()`](https://johnkirwan.github.io/radiatR/reference/import_info.md)
works on any manifest CSV, and
[`import_tracks()`](https://johnkirwan.github.io/radiatR/reference/import_tracks.md)
works on any directory following the `_point01` / `_point02` naming
convention.

``` r

manifest_path <- tempfile(fileext = ".csv")
writeLines(
  c("file,type, arc", "trial01.csv, baseline,90", "trial02.csv,stimulus,135"),
  manifest_path
)
import_info(manifest_path, cond_cols = c("type", "arc"))
#>          file      type arc         cond
#> 1 trial01.csv  baseline  90  baseline_90
#> 2 trial02.csv  stimulus 135 stimulus_135
```

## Registering a Custom Dialect

When no built-in dialect fits your exporter, register your own loader: a
function that receives what was passed to
[`read_tracks()`](https://johnkirwan.github.io/radiatR/reference/read_tracks.md)
(a file path or a data frame) and returns a data frame with `x` and `y`
(or `angle`), and normally `id` and `time`. The dialect is the only
reader of the file, so it can handle layouts the generic reader cannot:
no header row, several header rows, or a format that is not a table at
all.

The bundled millipede tracks were made with dtrack, which writes a
headerless, tab-separated file of `frame`, `x`, `y` and a flag column. A
loader for it:

``` r

dtrack_loader <- function(x) {
  # colClasses = "numeric" makes a malformed cell an error, not a silent NA
  d <- utils::read.delim(x, header = FALSE, colClasses = "numeric")
  if (ncol(d) < 3L)
    stop("dtrack: expected at least 3 columns (frame, x, y) in ", basename(x))
  data.frame(
    id    = tools::file_path_sans_ext(basename(x)),  # one file, one track
    frame = d[[1]],
    x     = d[[2]],
    y     = d[[3]]
  )
}
register_loader_dialect("dtrack", dtrack_loader)
```

After registration the dialect is used by name, like a built-in one:

``` r

track_file <- system.file(
  "extdata", "tracks", "10_10_point02.txt",
  package = "radiatR"
)
ts_raw <- read_tracks(track_file, dialect = "dtrack")
head(ts_raw@data[, c("id", "frame", "x", "y")])
#>              id frame      x      y
#> 1 10_10_point02     1 469.83 379.47
#> 2 10_10_point02     2 477.73 386.58
#> 3 10_10_point02     3 479.31 388.95
#> 4 10_10_point02     4 482.48 400.81
#> 5 10_10_point02     5 484.85 406.34
#> 6 10_10_point02     6 484.06 417.40
```

Given several files, the loader is called once per file and the results
are combined. Here `pattern` picks one session’s trajectory files and
leaves out the `_point01` landmark files, which share the same layout:

``` r

track_dir <- system.file("extdata", "tracks", package = "radiatR")
ts_session <- read_tracks_dir(track_dir, pattern = "^10_.*_point02\\.txt$",
                              dialect = "dtrack")
ts_session
#> Tracks: 29 trajectories, 4802 observations
#> Columns: id='id', time='frame', angle='angle' (radians), x='x', y='y'
#>             id frame      x      y    ..source_file     angle
#> 1 10_1_point02     1 455.60 372.36 10_1_point02.txt 0.6851986
#> 2 10_1_point02     2 457.98 369.98 10_1_point02.txt 0.6795104
#> 3 10_1_point02     3 455.60 369.98 10_1_point02.txt 0.6820587
#> 4 10_1_point02     4 454.02 369.98 10_1_point02.txt 0.6837593
#> 5 10_1_point02     5 455.60 377.10 10_1_point02.txt 0.6914043
#> 6 10_1_point02     6 458.77 381.84 10_1_point02.txt 0.6941355
```

Dialects live in-memory for the current session. To persist them across
projects, place the registration call in a package or in an `.Rprofile`
that you source before using `radiatR`.
