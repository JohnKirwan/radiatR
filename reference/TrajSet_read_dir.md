# Read all matching files from a directory and bind into a TrajSet

Read all matching files from a directory and bind into a TrajSet

## Usage

``` r
TrajSet_read_dir(
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

  Additional arguments passed to \`TrajSet_read()\`
