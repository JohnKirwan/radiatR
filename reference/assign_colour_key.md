# Assign a shared colour-key column to a Tracks or data frame

Writes a colour-key column (\`into\`, default \`".colour"\`) keyed on
\`by\`, so the tracks and any overlays drawn on top share one colour
scale. Two modes are chosen automatically by cardinality: \*
\*\*Cycled\*\* – \`by = "trajectory"\` (the trajectory id column), or
any column with more than \`n\` levels: the key is a cycled \`1:n\`
index (\[cycle_colours()\]), so a high-cardinality key stays legible. \*
\*\*Distinct\*\* – a column with \`n\` or fewer levels: the key holds
the column's raw values (as a factor), so a legend is meaningful.

## Usage

``` r
assign_colour_key(x, by, n = 20, reference = NULL, into = ".colour")

assign_color_key(x, by, n = 20, reference = NULL, into = ".colour")
```

## Arguments

- x:

  A \`Tracks\` or data frame to annotate.

- by:

  \`"trajectory"\` (the trajectory id column) or a grouping column name.

- n:

  Colour cap / cycle length (positive integer). Default 20.

- reference:

  Optional \`Tracks\`/frame whose key order to reuse. Default \`NULL\`
  uses \`x\` itself.

- into:

  Name of the key column to add. Default \`".colour"\`.

## Value

\`x\` with the \`into\` column added.

## Details

Pass \`reference\` (another Tracks or frame sharing the key) to borrow
its level order, so a given key value gets the same colour in both –
e.g. tracks and their heading markers. If \`by\` names a column absent
from \`x\` but present on \`reference\`, it is borrowed by matching
trajectory id.

## American spellings

Every \`colour...\` argument and the \`assign_colour\_\*\` /
\`cycle_colours\` / \`hf_colour_col\` functions accept the American
\`color...\` spelling as an alias (e.g. \`color\`, \`color_col\`,
\`track_color\`). British spelling is canonical; supplying both
spellings of a pair is an error.

## See also

\[cycle_colours()\]

## Examples

``` r
ts <- simulate_tracks(n_points = 10, output = "trajset")
ts <- assign_colour_key(ts, by = "trajectory")
```
