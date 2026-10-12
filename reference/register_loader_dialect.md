# Register a custom loader dialect

The function must accept (x, ...) and return a data.frame in long form
with columns at least id,time and one of (angle) or (x,y)

## Usage

``` r
register_loader_dialect(name, fun, overwrite = FALSE)
```

## Arguments

- name:

  Unique dialect name

- fun:

  Function that accepts (x, ...) and returns a data.frame with
  id/time/angle or id/time/x/y

- overwrite:

  Replace an existing dialect registered with the same name

## Value

\`name\`, invisibly. Called for its side effect of registering the
dialect for \`read_tracks(dialect = )\`.

## Details

\`x\` is whatever was passed to \[read_tracks()\]: a data frame, or one
file path. The dialect is the only reader of that file, and is called
once per file when \`read_tracks()\` is given several paths. A table
returned without an id column is one trajectory, named after its file in
a multi-file import. Numeric columns returned as text are coerced
strictly (see "Malformed values" in \[read_tracks()\]) using the decimal
mark sniffed from the file; a dialect that knows better can set
\`attr(out, "decimal")\`.
