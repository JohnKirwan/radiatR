# Read a dtrack trajectory file into a TrajSet

Reads a tab-separated, headerless file produced by dtrack
(<https://bitbucket.org/jochensmolka/dtrack>). The file is expected to
have at least three columns: frame number, x coordinate, y coordinate. A
fourth confidence/flag column (always `1` in practice) is silently
dropped.

## Usage

``` r
dtrack_read(path, normalize_xy = FALSE, ...)
```

## Arguments

- path:

  Path to a dtrack `_point02.txt` trajectory file.

- normalize_xy:

  Logical; passed to \[TrajSet_read()\]. Default `FALSE` because dtrack
  files are in pixel space.

- ...:

  Additional arguments passed to \[TrajSet_read()\].

## Value

A `TrajSet`.

## See also

\[import_tracks()\] for discovering dtrack file pairs in a directory.
