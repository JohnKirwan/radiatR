# Snap angles to fixed-width circular bin centres

Bins angles (in radians) into fixed-width sectors and returns each angle
snapped to its bin's centre. Snapping coincident-binned angles to a
common value is the standard precursor to a stacked dot plot: feed the
result to
[`stack_headings`](https://johnkirwan.github.io/radiatR/reference/stack_headings.md)
(with the default `tol = NULL`) to build clean radial columns.

## Usage

``` r
bin_angles(angles, width, phase = 0)
```

## Arguments

- angles:

  Numeric vector of angles in radians. `NA` is preserved.

- width:

  Bin width in radians; must be a single positive number. For a 5-degree
  bin use `pi / 36`.

- phase:

  Radian location of a bin *centre*. The default `0` places bin centres
  at `0, width, 2 * width, ...`, so the reference direction sits on a
  column rather than on a bin boundary. Set `phase = width / 2` to
  reproduce the edge-aligned bins of
  [`circular::plot.circular`](https://rdrr.io/pkg/circular/man/plot.circular.html)
  (centres at `width / 2`, `3 * width / 2`, ...). Any phase is allowed –
  e.g. with `width = pi / 2, phase = pi / 4` the bin boundaries fall on
  the axes, binning by quadrant.

## Value

A numeric vector the same length as `angles`, each value snapped to its
bin centre and wrapped to `[0, 2 * pi)`.

## See also

[`stack_headings`](https://johnkirwan.github.io/radiatR/reference/stack_headings.md),
[`add_stacked_headings`](https://johnkirwan.github.io/radiatR/reference/add_stacked_headings.md)

## Examples

``` r
# 5-degree bins centred on the reference direction
bin_angles(c(0.01, 0.10, 0.11), width = pi / 36)
#> [1] 0.00000000 0.08726646 0.08726646
# circular-package style (edge-aligned) bins
bin_angles(c(0.01, 0.10, 0.11), width = pi / 36, phase = pi / 72)
#> [1] 0.04363323 0.13089969 0.13089969
```
