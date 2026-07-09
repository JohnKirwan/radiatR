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
