# Register a declarative loader \*format\* (list or YAML/JSON file) The spec maps cleanly onto TrajSet_read() args and supports regex-based column finding.

Register a declarative loader \*format\* (list or YAML/JSON file) The
spec maps cleanly onto TrajSet_read() args and supports regex-based
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
