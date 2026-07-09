# Cycle a bounded set of colour indices over the values of a key

The order-stable primitive behind \[assign_cycle_colours()\] (and so
\[radiate()\]'s \`colour_cycle\`). Maps each value of \`x\` to an index
in \`1:n\`, numbering the distinct values by \`levels\` and wrapping
back to \`1\` after every \`n\`. Passing an explicit \`levels\` lets two
data frames that share a key (for example tracks and an overlay drawn on
top of them) be coloured identically, so a given key value gets the same
colour in both.

## Usage

``` r
cycle_colours(x, n, levels = NULL)

cycle_colors(x, n, levels = NULL)
```

## Arguments

- x:

  A vector of key values (e.g. trajectory ids or a grouping column).

- n:

  Number of colours to cycle through (a positive integer).

- levels:

  Optional ordering of the distinct key values. Defaults to their order
  of first appearance in \`x\`. Supply a shared ordering to colour two
  frames consistently.

## Value

A factor the same length as \`x\` with levels \`"1"\`..\`"n"\` giving
the cycled colour index. \`NA\` in \`x\` is preserved as \`NA\`.

## See also

\[assign_cycle_colours()\], \[radiate()\]

## Examples

``` r
cycle_colours(c("a", "b", "c", "a"), n = 2)
#> [1] 1 2 1 1
#> Levels: 1 2
```
