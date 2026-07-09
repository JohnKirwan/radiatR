# Test a circular sample for unimodality against bimodality

A parametric-bootstrap likelihood-ratio test of a single von Mises
distribution (unimodal) against an asymmetric two-component von Mises
mixture (bimodal). The null hypothesis is unimodality; a small p-value
is evidence of a second mode.

## Usage

``` r
test_unimodality(hd, group_col = NULL, angle_col = "heading", n_boot = 199L)
```

## Arguments

- hd:

  Data frame with a heading column in radians.

- group_col:

  Column to group by. `NULL` (default) tests the whole data frame as one
  group.

- angle_col:

  Heading column name. Default `"heading"`.

- n_boot:

  Number of parametric-bootstrap replicates. Default `199`.

## Value

Tidy data frame with columns `group_col` (if supplied), `statistic` (the
likelihood-ratio statistic), `p_value`, `n`, and `test`. Groups with
fewer than ten finite angles, or where the von Mises fit fails, yield
`NA` statistics.

## Details

The mixture likelihood-ratio statistic has a non-standard null
distribution (a boundary / non-identifiability problem), so its p-value
cannot be read from a \\\chi^2\\. Instead it is obtained by a parametric
bootstrap: `n_boot` samples are simulated from the fitted von Mises,
both models are refitted to each, and the observed statistic is compared
with the resulting null distribution. This is deliberately *not* a
critical-bandwidth (Silverman) test: the critical bandwidth's null
distribution is not valid for circular data (Fisher & Marron 2001),
whereas this bootstrap is well calibrated.

The test is computationally intensive – every bootstrap replicate refits
the mixture – so it is best run with a modest `n_boot`. Set the RNG seed
with [`set.seed`](https://rdrr.io/r/base/Random.html) for reproducible
p-values. It reuses the fitters behind
[`circ_model_select`](https://johnkirwan.github.io/radiatR/reference/circ_model_select.md),
which offers a complementary information-criterion view of the same
unimodal / bimodal comparison.

## References

Silverman, B.W. (1981). Using kernel density estimates to investigate
multimodality. *JRSS B* 43(1), 97–99. Fisher, N.I. & Marron, J.S.
(2001). Mode testing via the excess mass estimate. *Biometrika* 88(2),
499–517.

## See also

[`circ_model_select`](https://johnkirwan.github.io/radiatR/reference/circ_model_select.md),
[`test_symmetry`](https://johnkirwan.github.io/radiatR/reference/test_symmetry.md)
