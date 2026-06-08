# Path straightness index for a single trajectory

The straightness index is the net (start-to-end) displacement divided by
the total path length travelled. It ranges from 0 (a maximally
convoluted path that returns to its starting point) to 1 (a perfectly
straight path), and is the reciprocal of the tortuosity ratio
(\[path_tortuosity()\]). Unlike that ratio it is bounded and does not
blow up when the net displacement is small.

## Usage

``` r
path_straightness(x, y)
```

## Arguments

- x, y:

  Numeric vectors of ordered (in time) coordinates for one trajectory.

## Value

A single straightness value in \`\[0, 1\]\`, or \`NA_real\_\` when fewer
than two finite points are available or the path has zero length.

## Details

The index is scale-invariant: multiplying all coordinates by a constant
leaves it unchanged, so absolute or relative coordinates give the same
value (provided the transformation is a similarity, i.e. uniform in x
and y).

## See also

\[straightness_index()\] for a whole \`TrajSet\`; \[path_tortuosity()\].

## Examples

``` r
path_straightness(x = c(0, 1, 2), y = c(0, 0, 0))   # straight -> 1
#> [1] 1
path_straightness(x = c(0, 1, 0), y = c(0, 1, 0))   # returns to start -> 0
#> [1] 0
```
