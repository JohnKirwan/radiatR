# Guess the role of each column in a track table

Inspects a data frame's column names and returns the best guess for each
\`TrajSet\` role (\`id\`, \`time\`, \`x\`, \`y\`, \`angle\`,
\`weight\`), honouring any explicit \`mapping\` overrides. Matching is
case-insensitive; \`x\`/\`y\` also match separator-suffixed names such
as \`Track1_X\`. A role with no match is \`NULL\`. This is the same
logic \[TrajSet_read()\] uses internally; call it to see or pre-fill a
column mapping. It does not synthesize missing \`id\`/\`time\` columns
(a \`NULL\` signals that \`TrajSet_read()\` will apply its single-track
/ row-order fallback).

## Usage

``` r
guess_columns(data, mapping = list())
```

## Arguments

- data:

  A data frame (or anything with \`names()\`).

- mapping:

  Optional named list of explicit role -\> column overrides.

## Value

A named list with elements \`id\`, \`time\`, \`x\`, \`y\`, \`angle\`,
\`weight\` (each a column name or \`NULL\`).

## Examples

``` r
guess_columns(data.frame(Frame = 1:2, Track1_X = 0:1, Track1_Y = 0:1))
#> $id
#> NULL
#> 
#> $time
#> [1] "Frame"
#> 
#> $angle
#> NULL
#> 
#> $x
#> [1] "Track1_X"
#> 
#> $y
#> [1] "Track1_Y"
#> 
#> $weight
#> NULL
#> 
```
