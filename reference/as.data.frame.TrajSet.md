# Coerce a TrajSet to a data frame

Returns the long-form trajectory table held in the TrajSet's \`data\`
slot.

## Usage

``` r
# S3 method for class 'TrajSet'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)
```

## Arguments

- x:

  A \[TrajSet-class\] object.

- row.names, optional:

  Accepted for S3 generic consistency; ignored.

- ...:

  Ignored.

## Value

A data frame: the TrajSet's \`data\` slot.

## Details

Registered as an S3 method so that \`as.data.frame(ts)\` dispatches
correctly from any caller. (A bare S4 method on the base S3 generic is
only reached from within the package's own namespace, not from user code
using the installed package.)
