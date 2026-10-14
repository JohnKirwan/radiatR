# Register a declarative loader \*format\* (list or YAML/JSON file)

The spec maps cleanly onto read_tracks() args and supports regex-based
column finding.

## Usage

``` r
register_loader_format(name, spec, overwrite = FALSE)
```

## Arguments

- name:

  A unique name

- spec:

  A named list, or a path to a YAML/JSON file defining the spec

- overwrite:

  Overwrite an existing format of the same name

## Value

\`name\`, invisibly. Called for its side effect of registering the
format for \[read_tracks_format()\].

## Examples

``` r
# A tracker whose columns are found by pattern; time is in seconds
spec <- list(
  regex_mapping = list(id = "^animal", time = "^sec",
                       x = "^pos_?x", y = "^pos_?y"),
  time = list(type = "seconds")
)
register_loader_format("my_tracker", spec, overwrite = TRUE)
list_loader_formats()
#> [1] "my_tracker"
```
