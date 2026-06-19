# Low-level headings_frame constructor

Wraps a data frame as a \`headings_frame\` (a tibble subclass) carrying
the canonical display/heading metadata. Most users call
\[headings_frame()\] (which validates and normalises angles) or get one
from \[derive_headings()\].

## Usage

``` r
new_headings_frame(
  data,
  display = circ_display(),
  heading_col = "heading",
  colour_col = NULL,
  coords = "absolute"
)
```

## Arguments

- data:

  A data frame / tibble with the heading column already in unit-circle
  radians.

- display:

  A \[circ_display()\] object (orientation convention).

- heading_col:

  Name of the heading column. Default \`"heading"\`.

- colour_col:

  Optional grouping/colour column name, or \`NULL\`.

- coords:

  \`"absolute"\` or \`"relative"\`.

## Value

A \`headings_frame\`.
