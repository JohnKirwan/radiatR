# Circular statistics over derived headings

Derives one heading per trial via \`derive_headings()\`, then computes
circular summary statistics (mean direction, resultant length,
concentration) optionally grouped by one or more metadata columns.

## Usage

``` r
circ_summary_headings(x, rule, group_by = "id", ...)
```

## Arguments

- x:

  A \[\`Tracks\`\] object.

- rule:

  Required. Heading derivation rule passed to \[derive_headings()\]. One
  of \`"crossing"\`, \`"distal"\`, \`"straight"\`, \`"origin_mean"\`,
  \`"net"\`, or \`"velocity_mean"\`.

- group_by:

  Character vector of column names used to group headings before
  summarising. Default \`"id"\` returns one row per trial. Use \`NULL\`
  for a single global summary row. Other columns must first be carried
  onto the heading table with \`carry\` (e.g. \`carry = "arc"\`).

- ...:

  Additional arguments forwarded to \[derive_headings()\], such as
  \`circ0\`, \`circ1\`, \`carry\`, \`return_coords\`, or \`coords\`.

## Value

A \`data.frame\` with grouping columns followed by \`mean_dir\`
(radians, unit-circle convention, 0 to \\2\pi\\), \`resultant_R\` (0–1),
\`kappa\` (von Mises concentration, with no small-sample bias
correction; see
[`circ_summary()`](https://johnkirwan.github.io/radiatR/reference/circ_summary.md)),
and \`n\` (number of valid headings in the group).

## Examples

``` r
data(cpunctatus)
# per-trial headings (default)
head(circ_summary_headings(cpunctatus, rule = "crossing",
                           circ0 = 0.2, circ1 = 0.4))
#> Warning: derive_headings(rule = 'crossing'): 25 of 251 headings (10.0%), from 25 of 235 trials, are missing and are excluded from circular statistics. Rule-based failures are often non-random and can bias results; inspect attr(x, "missing_ids").
#>        id mean_dir resultant_R kappa n
#> 1 10_10_1 6.233783           1   Inf 1
#> 2 10_11_1 1.335350           1   Inf 1
#> 3 10_12_1 2.284647           1   Inf 1
#> 4 10_13_1 5.652135           1   Inf 1
#> 5 10_14_1 1.658976           1   Inf 1
#> 6 10_15_1 4.900618           1   Inf 1

# per-condition summary: carry the "arc" column through to group by it
circ_summary_headings(cpunctatus, rule = "crossing",
                      circ0 = 0.2, circ1 = 0.4,
                      carry = "arc", group_by = "arc")
#> Warning: derive_headings(rule = 'crossing'): 25 of 251 headings (10.0%), from 25 of 235 trials, are missing and are excluded from circular statistics. Rule-based failures are often non-random and can bias results; inspect attr(x, "missing_ids").
#>   arc  mean_dir resultant_R      kappa  n
#> 1   0 2.4441971  0.21852376 0.44789786 33
#> 2   5 6.1710354  0.03518184 0.07040728 30
#> 3  10 1.3122762  0.27298867 0.56758464 35
#> 4  15 6.1535219  0.25745294 0.53291294 27
#> 5  20 3.7683984  0.10968390 0.22070060 24
#> 6  30 0.8051961  0.16285026 0.33011478 34
#> 7  40 3.3258479  0.12904693 0.26027272 19
#> 8  50 1.6405047  0.09148952 0.18375018 24
```
