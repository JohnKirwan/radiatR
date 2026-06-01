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

This vignette walks through each step using the bundled *P. lividus*
example data and finishes with a user-defined loader for a custom
format.

## Loading a Real Experiment

The package ships five baseline trials from a *Paracentrotus lividus*
visual acuity experiment. Tracks are stored as paired tab-separated text
files: `_point01.txt` holds two landmark rows (arena centre + reference
point); `_point02.txt` holds the full per-frame xy trajectory.

### Step 1 — discover files

[`import_tracks()`](https://johnkirwan.github.io/radiatR/reference/import_tracks.md)
scans a directory for `_point01.txt` / `_point02.txt` pairs and returns
a tibble of basenames.

``` r

track_dir <- system.file("extdata", "tracks", package = "radiatR")
file_tbl  <- import_tracks(track_dir)
file_tbl
#> # A tibble: 5 × 3
#>   basename                                 landmark                        track
#>   <chr>                                    <chr>                           <chr>
#> 1 G1D_0_obstacle/WIN_20210201_11_24_19_Pro G1D_0_obstacle/WIN_20210201_11… G1D_…
#> 2 G1D_0_obstacle/WIN_20210201_11_32_56_Pro G1D_0_obstacle/WIN_20210201_11… G1D_…
#> 3 G1D_0_obstacle/WIN_20210204_16_55_13_Pro G1D_0_obstacle/WIN_20210204_16… G1D_…
#> 4 G1D_0_obstacle/WIN_20210204_17_12_38_Pro G1D_0_obstacle/WIN_20210204_17… G1D_…
#> 5 G1D_0_obstacle/WIN_20210204_17_30_19_Pro G1D_0_obstacle/WIN_20210204_17… G1D_…
```

### Step 2 — read the manifest

[`import_info()`](https://johnkirwan.github.io/radiatR/reference/import_info.md)
parses the trial manifest CSV. Use `cond_cols` to generate a compound
condition column from multiple metadata fields.

``` r

manifest_path <- system.file("extdata", "P_lividus_trials.csv", package = "radiatR")
manifest <- import_info(manifest_path, cond_cols = c("type", "arc"))
manifest
#>   trial type arc id stim_loc       date     time obstacle
#> 1     1 Herm   0  4      -32 01/02/2021 11:24:00  marbles
#> 2     2 Herm   0  6      148 04/02/2021 16:55:00  marbles
#> 3     3 Herm   0  9       49 01/02/2021 11:32:00  marbles
#> 4     4 Herm   0 32      266 04/02/2021 17:12:00  marbles
#> 5     5 Herm   0 20       75 04/02/2021 17:30:00  marbles
#>                                       file   cond
#> 1 G1D_0_obstacle/WIN_20210201_11_24_19_Pro Herm_0
#> 2 G1D_0_obstacle/WIN_20210201_11_32_56_Pro Herm_0
#> 3 G1D_0_obstacle/WIN_20210204_16_55_13_Pro Herm_0
#> 4 G1D_0_obstacle/WIN_20210204_17_12_38_Pro Herm_0
#> 5 G1D_0_obstacle/WIN_20210204_17_30_19_Pro Herm_0
```

### Step 3 — join metadata

[`load_tracks()`](https://johnkirwan.github.io/radiatR/reference/load_tracks.md)
merges the manifest into `file_tbl`, matching on the `file` column. The
resulting tibble carries arc, type, obstacle, and id alongside each file
path.

``` r

file_tbl <- load_tracks(file_tbl, manifest, track_dir)
file_tbl[, c("basename", "arc", "type", "obstacle", "id")]
#> # A tibble: 5 × 5
#>   basename                                   arc type  obstacle    id
#>   <chr>                                    <int> <chr> <chr>    <int>
#> 1 G1D_0_obstacle/WIN_20210201_11_24_19_Pro     0 Herm  marbles      4
#> 2 G1D_0_obstacle/WIN_20210201_11_32_56_Pro     0 Herm  marbles      6
#> 3 G1D_0_obstacle/WIN_20210204_16_55_13_Pro     0 Herm  marbles      9
#> 4 G1D_0_obstacle/WIN_20210204_17_12_38_Pro     0 Herm  marbles     32
#> 5 G1D_0_obstacle/WIN_20210204_17_30_19_Pro     0 Herm  marbles     20
```

### Step 4 — extract and normalise trajectories

[`get_all_object_pos()`](https://johnkirwan.github.io/radiatR/reference/get_all_object_pos.md)
reads each file pair, uses the landmark rows to establish the arena
geometry, and returns a `TrajSet` with normalised unit-circle
coordinates.

``` r

ts <- get_all_object_pos(file_tbl = file_tbl, track_dir = track_dir)
ts
#> TrajSet: 5 trajectories, 2049 observations
#> Columns: id='trial_id', time='frame', angle='rel_theta' (radians), x='trans_x', y='trans_y'
#> Transform steps: unit_circle_mapping 
#> # A tibble: 6 × 15
#>   trial_id       frame     x     y trans_x trans_y trans_rho abs_theta rel_theta
#>   <chr>          <int> <dbl> <dbl>   <dbl>   <dbl>     <dbl>     <dbl>     <dbl>
#> 1 G1D_0_obstacl…     1 1045.  559.   0.181 -0.0395     0.186      6.07      3.93
#> 2 G1D_0_obstacl…     2 1045.  559.   0.181 -0.0395     0.186      6.07      3.93
#> 3 G1D_0_obstacl…     3 1050.  560.   0.191 -0.0407     0.196      6.07      3.93
#> 4 G1D_0_obstacl…     4 1050.  560.   0.191 -0.0407     0.196      6.07      3.93
#> 5 G1D_0_obstacl…     5 1052.  560.   0.197 -0.0407     0.202      6.08      3.94
#> 6 G1D_0_obstacl…     6 1053.  560.   0.199 -0.0407     0.203      6.08      3.94
#> # ℹ 6 more variables: rel_x <dbl>, rel_y <dbl>, video <chr>, order <chr>,
#> #   vid_ord <chr>, radius <dbl>
```

## dtrack format

The bundled example data was tracked with **dtrack**
(<https://bitbucket.org/jochensmolka/dtrack>), a desktop tracking tool
for video recordings of freely-moving animals. The data are from:

> Kirwan J.D., Li T., Ullrich-Lüter J., La Camera G., Nilsson D.-E.,
> Arnone M.I. (2024). *The sea urchin Paracentrotus lividus orients to
> visual stimuli.* bioRxiv. <https://doi.org/10.1101/2024.01.05.574409>

dtrack exports tab-separated text files with columns `frame`, `x`, `y`,
and a fourth confidence/flag column. Use
[`dtrack_read()`](https://johnkirwan.github.io/radiatR/reference/dtrack_read.md)
to load a single trajectory file directly:

``` r

track_file <- system.file(
  "extdata", "tracks", "G1D_0_obstacle",
  "WIN_20210201_11_24_19_Pro_point02.txt",
  package = "radiatR"
)
ts_raw <- dtrack_read(track_file)
head(ts_raw@data[, c("id", "frame", "x", "y")])
#>                                  id frame      x      y
#> 1 WIN_20210201_11_24_19_Pro_point02     1 1044.9 559.09
#> 2 WIN_20210201_11_24_19_Pro_point02     2 1044.9 559.09
#> 3 WIN_20210201_11_24_19_Pro_point02     3 1049.5 559.66
#> 4 WIN_20210201_11_24_19_Pro_point02     4 1049.5 559.66
#> 5 WIN_20210201_11_24_19_Pro_point02     5 1052.3 559.66
#> 6 WIN_20210201_11_24_19_Pro_point02     6 1052.9 559.66
```

The `_point01` / `_point02` role split used in the bundled data —
landmarks in one file, trajectory in the other — is specific to this
experiment and is not a general dtrack convention.

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
#>   id time           x          y x_raw y_raw rho      angle
#> 1  A    0  1.00000000 0.00000000   1.0   0.0   1 0.00000000
#> 2  A    1  0.99654576 0.08304548   1.2   0.1   1 0.08314123
#> 3  A    2  0.97780241 0.20952909   1.4   0.3   1 0.21109333
#> 4  A    3  0.96623494 0.25766265   1.5   0.4   1 0.26060239
#> 5  B    0 -0.19611614 0.98058068  -0.2   1.0   1 1.76819189
#> 6  B    1 -0.09053575 0.99589321  -0.1   1.1   1 1.66145621
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
#>     id time         x          y x_raw y_raw rho     angle
#> A.1  A    0 0.0000000  0.0000000   0.0   0.0  NA 0.0000000
#> A.2  A    1 0.9805807  0.1961161   1.0   0.2   1 0.1973956
#> A.3  A    2 0.9701425  0.2425356   2.0   0.5   1 0.2449787
#> B.1  B    0 1.0000000  0.0000000   1.0   0.0   1 0.0000000
#> B.2  B    1 0.9970545 -0.0766965   1.3  -0.1   1 6.2064134
#> B.3  B    2 0.9912279 -0.1321637   1.5  -0.2   1 6.1506338
```

Dialects live in-memory for the current session. To persist them across
projects, place the registration call in a package or in an `.Rprofile`
that you source before using `radiatR`.
