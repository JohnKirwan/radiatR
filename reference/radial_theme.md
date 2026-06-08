# Themes for radial track plots, named for the ggplot2 base themes.

Applies one of the standard ggplot2 themes as the look of a
\`radiate()\` plot, so the panel background, grid lines, and border
match the familiar \`ggplot2::theme\_\*()\` appearance. The Cartesian
axis text and ticks are not meaningful on a unit-circle plot and are
removed by \`radiate()\` itself, so this wrapper keeps only the
panel-level styling that distinguishes the themes from one another.

## Usage

``` r
radial_theme(name = "void", base_size = 11)
```

## Arguments

- name:

  One of \`"void"\`, \`"minimal"\`, \`"classic"\`, \`"bw"\`, \`"grey"\`
  (or \`"gray"\`), \`"light"\`, \`"dark"\`, \`"linedraw"\` –
  corresponding to the matching \`ggplot2::theme\_\*()\`. Default
  \`"void"\`.

- base_size:

  Base font size, passed to the underlying theme. Default \`11\`.

## Value

A ggplot2 theme object.

## Examples

``` r
library(ggplot2)
ggplot() + coord_fixed() + add_circ() + radial_theme("bw")

```
