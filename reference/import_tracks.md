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
(<https://bitbucket.org/jochensmolka/dtrack>). In the bundled *P.
lividus* example data, `_point01` files contain two landmark rows per
trial (arena centre and stimulus edge on the arena wall) and `_point02`
files contain the per-frame animal trajectory. This two-file role split
is specific to that experiment and is not a general dtrack convention.
Use \[dtrack_read()\] to read an individual trajectory file.
