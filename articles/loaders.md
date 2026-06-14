# Working with Loaders

## Overview

`radiatR` provides a flexible loader framework that turns tracking
exports from a wide variety of systems into `TrajSet` objects. The
helpers cover three typical tasks:

1.  Reading experiment manifests
    ([`import_info()`](https://johnkirwan.github.io/radiatR/reference/import_info.md)
    and
    [`import_tracks()`](https://johnkirwan.github.io/radiatR/reference/import_tracks.md)).
2.  Ingesting trajectory tables with
    [`TrajSet_read()`](https://johnkirwan.github.io/radiatR/reference/TrajSet_read.md)
    or
    [`TrajSet_read_dir()`](https://johnkirwan.github.io/radiatR/reference/TrajSet_read_dir.md).
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

### Step 3 — join metadata

[`load_tracks()`](https://johnkirwan.github.io/radiatR/reference/load_tracks.md)
merges the manifest into `file_tbl`, matching on the `file` column. The
resulting tibble carries arc, type, obstacle, and id alongside each file
path.

``` r

file_tbl <- load_tracks(file_tbl, manifest, track_dir)
#> Warning in .augment_with_manifest(file_tbl, df, NULL): Entries in `file_tbl`
#> with no matching metadata: con_19
#> Warning in .augment_with_manifest(file_tbl, df, NULL): Rows in `manifest` with
#> no corresponding track: con_101, con_102, con_104, con_105, con_108, con_109,
#> con_110, con_112, con_116, con_117, con_119, con_120, con_121, 5_101, 5_102,
#> 5_103, 5_104, 5_108, 5_110, 5_117, 5_118, 5_119, 5_121, 10_101, 10_102, 10_105,
#> 10_107, 10_108, 10_109, 10_110, 10_111, 10_112, 10_113, 10_114, 10_116, 10_117,
#> 10_119, 10_121, 15_101, 15_102, 15_104, 15_105, 15_107, 15_109, 15_110, 15_112,
#> 15_116, 15_119, 15_120, 15_121, 20_101, 20_102, 20_103, 20_104, 20_105, 20_106,
#> 20_107, 20_108, 20_109, 20_110, 20_112, 20_116, 20_117, 20_119, 20_121, 30_101,
#> 30_102, 30_104, 30_105, 30_106, 30_107, 30_108, 30_109, 30_113, 30_114, 30_115,
#> 30_116, 30_117, 30_119, 30_121, 40_101, 40_102, 40_103, 40_104, 40_105, 40_107,
#> 40_108, 40_109, 40_110, 40_112, 40_118, 40_119, 40_121, 50_12, 50_34, 50_101,
#> 50_104, 50_105, 50_107, 50_108, 50_109, 50_110, 50_112, 50_113, 50_119, 50_121
head(file_tbl[, c("basename", "arc", "type", "obstacle", "id")])
#> # A tibble: 6 × 5
#>   basename   arc type     obstacle    id
#>   <chr>    <int> <chr>    <chr>    <int>
#> 1 10_1        10 stimulus none         1
#> 2 10_10       10 stimulus none        10
#> 3 10_11       10 stimulus none        11
#> 4 10_12       10 stimulus none        12
#> 5 10_13       10 stimulus none        13
#> 6 10_14       10 stimulus none        14
```

### Step 4 — extract and normalise trajectories

[`get_all_object_pos()`](https://johnkirwan.github.io/radiatR/reference/get_all_object_pos.md)
reads each file pair, uses the landmark rows to establish the
unit-circle geometry, and returns a `TrajSet` with normalised
unit-circle coordinates.

``` r

ts <- suppressWarnings(get_all_object_pos(file_tbl = file_tbl, track_dir = track_dir))
#> 1 point across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 4 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 2 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 4 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 2 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 4 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 11 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 2 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 2 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 1 point across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 3 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 4 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 3 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 6 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 4 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 6 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 3 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 2 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 6 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 5 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 2 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 2 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 5 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 2 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 4 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 1 point across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 9 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 6 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 10 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 3 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 9 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 2 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 3 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 8 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 6 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 3 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 2 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 3 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 3 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 4 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 6 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 2 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 3 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 5 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 3 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 6 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 1 point across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 8 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 2 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 2 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 2 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 2 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 2 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 8 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 5 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 2 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 2 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 2 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 4 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 8 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 2 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 4 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 4 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 6 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 2 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 7 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 23 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 11 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 3 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 6 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 2 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 8 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 3 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 7 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 4 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 4 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 2 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 1 point across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 10 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 1 point across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 4 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 1 point across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 3 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 2 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 4 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 6 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 2 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 3 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 2 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 7 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 11 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 6 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 6 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 19 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 2 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 6 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 9 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 4 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 3 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 6 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 2 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 3 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 6 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 3 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 9 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
#> 2 points across 1 trial exceeded the arena boundary (radius > 1); coordinates left unscaled.
ts
#> TrajSet: 235 trajectories, 44331 observations
#> Columns: id='trial_id', time='frame', angle='rel_theta' (radians), x='trans_x', y='trans_y', rel_x='rel_x', rel_y='rel_y'
#> Transform steps: unit_circle_mapping 
#> # A tibble: 6 × 15
#>   trial_id frame     x     y trans_x  trans_y trans_rho abs_theta rel_theta
#>   <chr>    <int> <dbl> <dbl>   <dbl>    <dbl>     <dbl>     <dbl>     <dbl>
#> 1 10_1_1       1  456.  372. 0.00511  0         0.00511     0         6.21 
#> 2 10_1_1       2  458.  370. 0.0128   0.00770   0.0149      0.541     0.464
#> 3 10_1_1       3  456.  370. 0.00511  0.00770   0.00924     0.985     0.908
#> 4 10_1_1       4  454.  370. 0        0.00770   0.00770     1.57      1.49 
#> 5 10_1_1       5  456.  377. 0.00511 -0.0153    0.0162      5.03      4.96 
#> 6 10_1_1       6  459.  382. 0.0154  -0.0307    0.0343      5.18      5.10 
#> # ℹ 6 more variables: rel_x <dbl>, rel_y <dbl>, video <chr>, order <chr>,
#> #   vid_ord <chr>, radius <dbl>
```

## dtrack format

The bundled example data was tracked with **dtrack**
(<https://bitbucket.org/jochensmolka/dtrack>), a desktop tracking tool
for video recordings of freely-moving animals. The data are from:

> Kirwan J.D. & Nilsson D.-E. (2019). *A millipede compound eye
> mediating low-resolution vision.* Vision Research 165, 36–44.
> <https://doi.org/10.1016/j.visres.2019.09.003>

dtrack exports tab-separated text files with columns `frame`, `x`, `y`,
and a fourth confidence/flag column. Use
[`dtrack_read()`](https://johnkirwan.github.io/radiatR/reference/dtrack_read.md)
to load a single trajectory file directly:

``` r

track_file <- system.file(
  "extdata", "tracks", "10_10_point02.txt",
  package = "radiatR"
)
ts_raw <- dtrack_read(track_file)
head(ts_raw@data[, c("id", "frame", "x", "y")])
#>              id frame      x      y
#> 1 10_10_point02     1 469.83 379.47
#> 2 10_10_point02     2 477.73 386.58
#> 3 10_10_point02     3 479.31 388.95
#> 4 10_10_point02     4 482.48 400.81
#> 5 10_10_point02     5 484.85 406.34
#> 6 10_10_point02     6 484.06 417.40
```

The `_point01` / `_point02` role split used in the bundled data —
landmarks in one file, trajectory in the other — is specific to this
experiment and is not a general dtrack convention.

## Tracking-tool format support

`radiatR` reads tabular (CSV/TSV) exports from many tracking tools
through named *dialects*. Each dialect maps a tool’s column conventions
onto the `TrajSet` model of one position per individual per frame. Pass
the dialect name to `TrajSet_read(path, dialect = "name")`;
tool-specific options go through `dialect_args`.

The table below summarises what is and is not currently handled. Two
limitations are general:

- **Tabular text only.** Dialects read CSV/TSV (and, where the optional
  packages are installed, JSON/XML/Parquet). Native binary formats —
  such as NumPy `.npz` or MATLAB `.mat` — are not read except where
  noted (`ctrax` reads `.mat` via the `R.matlab` package).
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
| `ctrax` | `.mat` file with the `trx` struct (centroid + ellipse `theta`/`a`/`b`); needs `R.matlab`. | — |
| `idtrackerai_wide`, `toxtrac`, `boris_xy`, `tracktor`, `dtrack` | Their standard CSV/TSV column layouts. | Tool-specific binary or session formats. |

When a tool’s export is not directly supported, two escape hatches
remain: convert to a tidy CSV and use
[`TrajSet_read()`](https://johnkirwan.github.io/radiatR/reference/TrajSet_read.md)
with an explicit `mapping`, or register a custom dialect (see
*Registering a Custom Dialect* below).

## Bundled single-file examples

The package ships small real exports from three tools so the dialects
can be tried without any external data. Each loads with one
[`TrajSet_read()`](https://johnkirwan.github.io/radiatR/reference/TrajSet_read.md)
call, pointing the `dialect` argument at the matching tool.

**DeepLabCut** — a three-row scorer/bodypart/coord header; by default
the position is the likelihood-weighted centroid of all bodyparts, and
each bodypart’s coordinates are retained for use with the
`bodypart_axis` rule.

``` r

dlc_path <- system.file("extdata", "dlc_CollectedData_Pranav.csv",
                        package = "radiatR")
ts_dlc <- TrajSet_read(dlc_path, dialect = "deeplabcut_multiheader")
#> New names:
#> • `Pranav` -> `Pranav...2`
#> • `Pranav` -> `Pranav...3`
#> • `Pranav` -> `Pranav...4`
#> • `Pranav` -> `Pranav...5`
#> • `Pranav` -> `Pranav...6`
#> • `Pranav` -> `Pranav...7`
#> • `Pranav` -> `Pranav...8`
#> • `Pranav` -> `Pranav...9`
ts_dlc
#> TrajSet: 1 trajectories, 116 observations
#> Columns: id='id', time='time', angle='angle' (radians), x='x', y='y', raw_x='x_raw', raw_y='y_raw'
#>   id time          x           y snout_x snout_y leftear_x leftear_y rightear_x
#> 1  1    1 -0.3997793 -0.12643666  21.521 265.428    33.819   265.941     19.984
#> 2  1    2 -0.4308106  0.07229848  10.248 288.487    19.984   297.198     12.298
#> 3  1    3 -0.3700675  0.34696056  24.596 354.075    38.431   354.075     23.058
#> 4  1    4 -0.2010454  0.43873493  73.787 374.572    78.911   366.373     57.390
#> 5  1    5 -0.2736712  0.30866595  38.431 333.066    50.729   341.777     39.968
#> 6  1    6 -0.3337533  0.35422263  23.571 327.430    30.745   339.215     28.183
#>   rightear_y tailbase_x tailbase_y    x_raw    y_raw       rho    angle
#> 1    250.056     87.110    152.698 40.60850 233.5307 0.4192967 3.447905
#> 2    281.313     95.821    221.361 34.58775 272.0897 0.4368350 2.975322
#> 3    337.166     99.408    256.205 46.37325 325.3802 0.5072786 2.388409
#> 4    361.761    106.581    270.040 79.16725 343.1865 0.4826050 2.000481
#> 5    323.331    131.177    273.627 65.07625 317.9502 0.4125174 2.296173
#> 6    321.793    131.177    318.719 53.41900 326.7892 0.4866877 2.326450
```

**SLEAP** — an analysis CSV with `<node>.x` / `.y` / `.score` columns,
`track` as the individual and `frame_idx` as time.

``` r

sleap_path <- system.file("extdata", "sleap_example.csv", package = "radiatR")
ts_sleap <- TrajSet_read(sleap_path, dialect = "sleap")
ts_sleap
#> TrajSet: 1 trajectories, 1 observations
#> Columns: id='id', time='time', angle='angle' (radians), x='x', y='y', raw_x='x_raw', raw_y='y_raw'
#>     id time x y      a_x      a_y      b_x      b_y    x_raw    y_raw rho angle
#> 1 <NA>    0 0 0 205.9301 187.8896 278.6352 203.3659 242.2826 195.6278   0     0
```

**Tracktor** — a tidy `frame` / `pos_x` / `pos_y` CSV (one individual
here).

``` r

tracktor_path <- system.file("extdata", "tracktor_example.csv",
                            package = "radiatR")
ts_tracktor <- TrajSet_read(tracktor_path, dialect = "tracktor")
#> New names:
#> New names:
#> • `` -> `...1`
ts_tracktor
#> TrajSet: 1 trajectories, 547 observations
#> Columns: id='id', time='time', angle='angle' (radians), x='x', y='y', raw_x='x_raw', raw_y='y_raw'
#>   id time         x          y    x_raw    y_raw       rho    angle
#> 1  1    3 0.8793455 -0.4750115 1528.267 330.3940 0.9994421 5.787907
#> 2  1    4 0.8790113 -0.4745446 1528.014 330.7465 0.9989261 5.788159
#> 3  1    5 0.8783770 -0.4737024 1527.535 331.3825 0.9979680 5.788600
#> 4  1    6 0.8783105 -0.4734975 1527.485 331.5372 0.9978122 5.788749
#> 5  1    7 0.8781524 -0.4735463 1527.366 331.5003 0.9976962 5.788631
#> 6  1    8 0.8783783 -0.4734931 1527.536 331.5405 0.9978698 5.788785
```

The bundled dtrack example is larger and is shown in full in the
*Loading a Real Experiment* section above.

## Reading Tabular Trajectories

For data already in a data frame or CSV,
[`TrajSet_read()`](https://johnkirwan.github.io/radiatR/reference/TrajSet_read.md)
is the central entry point.

``` r

example_df <- data.frame(
  id   = rep(c("A", "B"), each = 4),
  time = rep(seq(0, 3), times = 2),
  x    = c(1, 1.2, 1.4, 1.5, -0.2, -0.1, 0, 0.15),
  y    = c(0, 0.1, 0.3, 0.4, 1.0, 1.1, 1.3, 1.5)
)

ts_df <- TrajSet_read(example_df, mapping = list(id = "id", time = "time", x = "x", y = "y"))
ts_df
#> TrajSet: 2 trajectories, 8 observations
#> Columns: id='id', time='time', angle='angle' (radians), x='x', y='y', raw_x='x_raw', raw_y='y_raw'
#>   id time          x          y x_raw y_raw       rho     angle
#> 1  A    0 -0.7808688 -0.6246950   1.0   0.0 1.0000000 3.8163336
#> 2  A    1 -0.1561738 -0.3123475   1.2   0.1 0.3492151 4.2487414
#> 3  A    2  0.4685213  0.3123475   1.4   0.3 0.5630925 0.5880026
#> 4  A    3  0.7808688  0.6246950   1.5   0.4 1.0000000 0.6747409
#> 5  B    0 -0.5734623 -0.8192319  -0.2   1.0 1.0000000 4.1016630
#> 6  B    1 -0.2457696 -0.4915392  -0.1   1.1 0.5495575 4.2487414
```

To batch a directory of CSV files,
[`TrajSet_read_dir()`](https://johnkirwan.github.io/radiatR/reference/TrajSet_read_dir.md)
calls
[`TrajSet_read()`](https://johnkirwan.github.io/radiatR/reference/TrajSet_read.md)
for each match and row-binds the result.

``` r

csv_dir <- tempfile()
dir.create(csv_dir)

write.csv(example_df, file.path(csv_dir, "trial01.csv"), row.names = FALSE)
write.csv(transform(example_df, id = paste0(id, "_2")),
          file.path(csv_dir, "trial02.csv"), row.names = FALSE)

dset <- TrajSet_read_dir(csv_dir, pattern = "\\.csv$")
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

When your exporter uses a non-standard format, register a custom
function. A dialect receives either a data frame or a file path and must
return a data frame with at least `id`, `time`, and either `angle` or
`(x, y)`.

Below we define a loader for a hypothetical JSON file where each track
is stored under an ID key with arrays of `timestamp`, `px`, and `py`.

``` r

fake_json <- tempfile(fileext = ".json")
jsonlite::write_json(
  list(
    tracks = list(
      A = list(timestamp = c(0, 1, 2), px = c(0, 1, 2), py = c(0, 0.2, 0.5)),
      B = list(timestamp = c(0, 1, 2), px = c(1, 1.3, 1.5), py = c(0, -0.1, -0.2))
    )
  ),
  fake_json,
  auto_unbox = TRUE
)

json_tracks_fn <- function(x) {
  dat <- if (is.character(x) && file.exists(x)) jsonlite::fromJSON(x) else x
  entries <- purrr::imap(dat$tracks, function(track, id) {
    data.frame(
      id   = id,
      time = track$timestamp,
      x    = track$px,
      y    = track$py,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, entries)
}
register_loader_dialect("json_tracks", json_tracks_fn)
```

After registration, call the dialect function directly on the file to
get a tidy data frame, then pass it to
[`TrajSet_read()`](https://johnkirwan.github.io/radiatR/reference/TrajSet_read.md).
(For standard tabular formats that R’s CSV/TSV readers handle natively,
`TrajSet_read(path, dialect = "name")` works directly—but non-tabular
formats such as JSON require this two-step approach.)

``` r

ts_json <- TrajSet_read(json_tracks_fn(fake_json))
ts_json
#> TrajSet: 2 trajectories, 6 observations
#> Columns: id='id', time='time', angle='angle' (radians), x='x', y='y', raw_x='x_raw', raw_y='y_raw'
#>     id time          x           y x_raw y_raw        rho     angle
#> A.1  A    0 -0.9701425 -0.24253563   0.0   0.0 1.00000000 3.3865713
#> A.2  A    1  0.0000000 -0.04850713   1.0   0.2 0.04850713 4.7123890
#> A.3  A    2  0.9701425  0.24253563   2.0   0.5 1.00000000 0.2449787
#> B.1  B    0 -0.9284767  0.37139068   1.0   0.0 1.00000000 2.7610863
#> B.2  B    1  0.1856953  0.00000000   1.3  -0.1 0.18569534 0.0000000
#> B.3  B    2  0.9284767 -0.37139068   1.5  -0.2 1.00000000 5.9026789
```

Dialects live in-memory for the current session. To persist them across
projects, place the registration call in a package or in an `.Rprofile`
that you source before using `radiatR`.
