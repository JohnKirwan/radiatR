# Sinuosity index for a single trajectory

The sinuosity index of Benhamou (2004), a turning-angle-based measure of
path tortuosity that — unlike the displacement-based
\[path_straightness()\] / \[path_tortuosity()\] — does not rely on net
start-to-end displacement, so it is well-behaved for convoluted or
random-search paths: \$\$S = 2 \left\[ p\\\frac{1+c}{1-c} + b^2
\right\]^{-1/2}\$\$ where \\p\\ is the mean step length, \\c\\ the mean
cosine of the interior turning angles, and \\b\\ the coefficient of
variation of step length. A perfectly straight path gives \`0\`; more
winding paths give larger values.

## Usage

``` r
path_sinuosity(x, y)
```

## Arguments

- x, y:

  Numeric vectors of ordered (in time) coordinates for one trajectory.

## Value

A single sinuosity value \`\>= 0\`, or \`NA_real\_\` when fewer than
three finite points are available or all steps have zero length.

## Details

Unlike the straightness index, sinuosity is \*\*not\*\* scale-invariant:
it has units of \\1/\sqrt{\mathrm{length}}\\. For the most reliable
comparison across trajectories the path should be resampled to a common
step length (rediscretisation); the \\b\\ term mitigates variable step
length but does not fully replace it.

## References

Benhamou, S. (2004). How to reliably estimate the tortuosity of an
animal's path. *Journal of Theoretical Biology* 229(2), 209–220.
[doi:10.1016/j.jtbi.2004.03.016](https://doi.org/10.1016/j.jtbi.2004.03.016)

## See also

\[sinuosity()\] for a whole \`Tracks\`; \[path_straightness()\],
\[path_tortuosity()\].

## Examples

``` r
path_sinuosity(x = 0:5, y = rep(0, 6))              # straight -> 0
#> [1] 0
```
