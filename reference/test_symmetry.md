# Test a circular distribution for reflective symmetry

Pewsey's (2002) omnibus test of reflective symmetry about an unspecified
mean direction, based on the standardised second central sine moment.
The null hypothesis is that the distribution is symmetric about its mean
direction; a small p-value indicates skewness (asymmetry). Useful as a
precondition check before applying methods that assume symmetry, such as
a von Mises fit or the Watson-Williams test.

## Usage

``` r
test_symmetry(
  hd,
  group_col = NULL,
  angle_col = "heading",
  p_adjust = "none",
  axial = FALSE
)
```

## Arguments

- hd:

  Data frame with a heading column in radians.

- group_col:

  Column to group by. `NULL` (default) tests the whole data frame as one
  group.

- angle_col:

  Heading column name. Default `"heading"`.

- p_adjust:

  Multiple-comparison correction method passed to
  [`p.adjust`](https://rdrr.io/r/stats/p.adjust.html). Default `"none"`.
  Applies only when `group_col` is supplied; a `p_value_adj` column is
  added.

- axial:

  Logical. Treat the angles as axial (bidirectional, mod-pi) data:
  symmetry is tested via the angle-doubling method. Default `FALSE`.

## Value

Tidy data frame with columns `group_col` (if supplied), `statistic` (the
standardised \\\|\bar b_2\|\\), `p_value`, `n`, `test`, and
`p_value_adj` (when `p_adjust != "none"`). Groups with fewer than four
finite angles yield `NA` statistics.

## Details

The test uses a large-sample normal approximation and can be liberal at
small \\n\\ or low concentration; treat borderline results at small
samples with caution.

## References

Pewsey, A. (2002). Testing circular symmetry. *Canadian Journal of
Statistics* 30(4), 591–600.
[doi:10.2307/3316098](https://doi.org/10.2307/3316098) .

## See also

[`test_uniformity`](https://johnkirwan.github.io/radiatR/reference/test_uniformity.md),
[`vonmises_fit`](https://johnkirwan.github.io/radiatR/reference/vonmises_fit.md)
