# Circular boxplot statistics (Tukey-like, for circular and axial data)

Computes the five-number summary and fences of a circular boxplot
following Buttarazzi, Pandolfo & Porzio (2018): observations are
depth-ranked outward from the antimedian, the box spans the central ~50
the median), and the fence multiplier is obtained in closed form from
the von Mises quantiles at the sample's estimated concentration (so ~0.7
observations are flagged as far-out under that reference). Pairs with
\[add_circular_boxplot()\] for rendering.

## Usage

``` r
circ_boxplot_stats(hd, angle_col = "heading", axial = FALSE)
```

## Source

Algorithm reimplemented from the bpDir package.

## Arguments

- hd:

  A data frame with a column of heading angles in radians (unit-circle
  convention), or a numeric vector of angles.

- angle_col:

  Name of the angle column when \`hd\` is a data frame. Default
  \`"heading"\`.

- axial:

  Logical. Treat the angles as axial (bidirectional, mod-pi): angles are
  doubled, the boxplot computed, and locations halved back to \`\[0,
  pi)\`; the fence multiplier is taken on the doubled data. Default
  \`FALSE\`.

## Value

A list with \`median\`, \`antimedian\`, \`hinges\`, \`box_arc\`,
\`constant\`, \`kappa\`, \`fences\`, \`whiskers\`, \`far_out\` (all
radians, unit-circle convention), \`n\`, \`axial\`, \`drawable\`, and
\`reason\`. When \`drawable\` is \`FALSE\` (non-unique median or \`n \<
4\`) the location fields are \`NA\`; \`reason\` may also carry a
non-fatal advisory while \`drawable\` remains \`TRUE\` (near-uniform
data).

## References

Buttarazzi, D., Pandolfo, G. & Porzio, G. C. (2018). A boxplot for
circular data. *Biometrics* 74(4), 1492–1501.
[doi:10.1111/biom.12889](https://doi.org/10.1111/biom.12889)

## See also

\[add_circular_boxplot()\], \[circ_summarise()\], \[vonmises_fit()\]

## Examples

``` r
tr <- simulate_tracks(output = "trajset", seed = 1)
hd <- derive_headings(tr, rule = "distal")
circ_boxplot_stats(hd)
#> $median
#> [1] 6.146777
#> 
#> $antimedian
#> [1] 3.005184
#> 
#> $hinges
#> [1] 5.7343562 0.4583497
#> 
#> $box_arc
#> [1] 5.7343562 0.4583497
#> 
#> $constant
#> [1] 2.149198
#> 
#> $kappa
#> [1] 2.071822
#> 
#> $fences
#> [1] 3.569730 2.622976
#> 
#> $whiskers
#> [1] 4.167899 1.316886
#> 
#> $far_out
#> numeric(0)
#> 
#> $n
#> [1] 60
#> 
#> $axial
#> [1] FALSE
#> 
#> $drawable
#> [1] TRUE
#> 
#> $reason
#> [1] NA
#> 
```
