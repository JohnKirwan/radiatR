# \*Cylindroiulus punctatus\* trajectory tibble

A tidy long-form tibble (44,331 rows) holding the per-frame observations
from the same millipede experiment as
[`cpunctatus`](https://johnkirwan.github.io/radiatR/reference/cpunctatus.md),
with the trial condition metadata (\`arc\`, \`type\`, \`individual\`)
joined onto every frame. Convenient for analyses that prefer a plain
data frame to the \`Tracks\` container. A subset of the published
dataset.

## Usage

``` r
cpunctatus_tracks
```

## Format

A tibble with 18 columns including `trial_id`, `frame`, `trans_x`,
`trans_y`, `rel_x`, `rel_y`, `abs_theta`, `rel_theta`, `arc`, `type`,
and `individual`.

## Source

Same experiment as
[`cpunctatus`](https://johnkirwan.github.io/radiatR/reference/cpunctatus.md).

## References

Kirwan, J. D., & Nilsson, D.-E. (2019). A millipede compound eye
mediating low-resolution vision. \*Vision Research\*, 165, 36–44.
[doi:10.1016/j.visres.2019.09.003](https://doi.org/10.1016/j.visres.2019.09.003)
