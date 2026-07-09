# Circular statistics over derived headings

Derives one heading per trial via \`derive_headings()\`, then computes
circular summary statistics (mean direction, resultant length,
concentration) optionally grouped by one or more metadata columns.

## Usage

``` r
circ_summary_headings(
  x,
  rule = c("crossing", "distal", "straight", "origin_mean", "net", "velocity_mean"),
  group_by = "id",
  ...
)
```

## Arguments

- x:

  A \[\`Tracks\`\] object.

- rule:

  Character. Heading derivation rule passed to \[derive_headings()\].
  One of \`"crossing"\`, \`"distal"\`, \`"straight"\`,
  \`"origin_mean"\`, \`"net"\`, or \`"velocity_mean"\`.

- group_by:

  Character vector of column names used to group headings before
  summarising. Default \`"id"\` returns one row per trial. Use \`NULL\`
  for a single global summary row. Any column carried through by
  \[derive_headings()\] (e.g. \`"arc"\`) is valid.

- ...:

  Additional arguments forwarded to \[derive_headings()\], such as
  \`circ0\`, \`circ1\`, \`return_coords\`, or \`coords\`.

## Value

A \`data.frame\` with grouping columns followed by \`mean_dir\`
(radians, unit-circle convention, 0 to 2π), \`resultant_R\` (0–1),
\`kappa\` (von Mises concentration), and \`n\` (number of valid headings
in the group).

## Examples

``` r
if (FALSE) { # \dontrun{
data(cpunctatus)
# per-trial headings (default)
circ_summary_headings(cpunctatus, rule = "crossing", circ0 = 0.2, circ1 = 0.4)

# per-condition summary (requires an "arc" column carried through)
circ_summary_headings(cpunctatus, rule = "crossing",
                      circ0 = 0.2, circ1 = 0.4,
                      group_by = "arc")
} # }
```
