# Read all matching files from a directory and bind into a Tracks

Read all matching files from a directory and bind into a Tracks

## Usage

``` r
read_tracks_dir(
  dir,
  pattern = "\\.(csv|tsv|txt|parquet|feather)$",
  recursive = FALSE,
  ...
)
```

## Arguments

- dir:

  Directory to scan for files

- pattern:

  Regex passed to \`list.files()\` to select files

- recursive:

  Recurse into subdirectories when TRUE

- ...:

  Additional arguments passed to \`read_tracks()\`

## Value

A single \`Tracks\` object combining every matching file.

## Examples

``` r
df <- data.frame(
  id   = rep(c("A", "B"), each = 4),
  time = rep(0:3, times = 2),
  x    = c(1, 1.2, 1.4, 1.5, -0.2, -0.1, 0, 0.15),
  y    = c(0, 0.1, 0.3, 0.4, 1.0, 1.1, 1.3, 1.5)
)
csv_dir <- file.path(tempdir(), "csv_tracks")
dir.create(csv_dir)
write.csv(df, file.path(csv_dir, "day1.csv"), row.names = FALSE)
write.csv(transform(df, id = paste0(id, "2")), file.path(csv_dir, "day2.csv"),
          row.names = FALSE)
read_tracks_dir(csv_dir, pattern = "\\.csv$")
#> Tracks: 4 trajectories, 16 observations
#> Columns: id='id', time='time', angle='angle' (radians), x='x', y='y'
#> # A tibble: 6 × 6
#>   id     time     x     y ..source_file  angle
#>   <chr> <dbl> <dbl> <dbl> <chr>          <dbl>
#> 1 A         0   1     0   day1.csv      0     
#> 2 A         1   1.2   0.1 day1.csv      0.0831
#> 3 A         2   1.4   0.3 day1.csv      0.211 
#> 4 A         3   1.5   0.4 day1.csv      0.261 
#> 5 A2        0   1     0   day2.csv      0     
#> 6 A2        1   1.2   0.1 day2.csv      0.0831
unlink(csv_dir, recursive = TRUE)
```
