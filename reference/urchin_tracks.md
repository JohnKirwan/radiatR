# \*Paracentrotus lividus\* visual acuity trajectory tibble (urchin tracks)

A tidy long-form tibble of 19 505 rows from the same \*P. lividus\*
visual acuity experiment as
[`plividus`](https://johnkirwan.github.io/radiatR/reference/plividus.md),
retained for backwards compatibility. Each row is one frame from one
trial.

## Usage

``` r
urchin_tracks
```

## Format

A tibble with 23 columns including `frame`, `x`, `y`, `trans_x`,
`trans_y`, `rel_x`, `rel_y`, `abs_theta`, `rel_theta`, `arc`, `type`,
`obstacle`, and `id`.

## Source

Same experiment as
[`plividus`](https://johnkirwan.github.io/radiatR/reference/plividus.md).
