# Discover dtrack (or compatible) landmark/track file pairs in a directory

Scans `dir` for paired files matching `landmark_suffix` and
`track_suffix` and returns a tibble of basenames and paths.

## Usage

``` r
import_tracks(dir, landmark_suffix = NULL, track_suffix = NULL)
```

## Arguments

- dir:

  Directory to scan. Defaults to the current working directory.

- landmark_suffix:

  Suffix identifying landmark files. Default `"_point01.txt"`.

- track_suffix:

  Suffix identifying trajectory files. Default `"_point02.txt"`.

## Value

A tibble with columns `basename`, `landmark`, and `track`.

## Details

The default suffixes match the export naming convention used by dtrack
(<https://bitbucket.org/jochensmolka/dtrack>). In the bundled millipede
example data, `_point01` files contain two landmark rows per trial (the
origin and stimulus edge on the circumference) and `_point02` files
contain the per-frame animal trajectory. This two-file role split is
specific to that experiment and is not a general dtrack convention.
\`vignette("loaders")\` registers a custom loader that reads an
individual trajectory file.

## Examples

``` r
untar(system.file("extdata", "tracks.tar.gz", package = "radiatR"),
      exdir = tempdir())
track_dir <- file.path(tempdir(), "tracks")
file_tbl <- import_tracks(track_dir)
head(file_tbl)
#> # A tibble: 6 × 3
#>   basename landmark          track            
#>   <chr>    <chr>             <chr>            
#> 1 10_10    10_10_point01.txt 10_10_point02.txt
#> 2 10_11    10_11_point01.txt 10_11_point02.txt
#> 3 10_12    10_12_point01.txt 10_12_point02.txt
#> 4 10_13    10_13_point01.txt 10_13_point02.txt
#> 5 10_14    10_14_point01.txt 10_14_point02.txt
#> 6 10_15    10_15_point01.txt 10_15_point02.txt
```
