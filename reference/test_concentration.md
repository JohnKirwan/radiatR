# Test whether groups share the same concentration (dispersion)

Two tests are available:

- `parametric = TRUE` (default):

  Likelihood-ratio test for equal von Mises \\\kappa\\ across groups
  (`equal.kappa.test`). Returns a chi-squared statistic with \\k-1\\
  degrees of freedom.

- `parametric = FALSE`:

  Wallraff's non-parametric test for equal angular dispersions – no
  distributional assumption required.

## Usage

``` r
test_concentration(hd, group_col, angle_col = "heading", parametric = TRUE)
```

## Arguments

- hd:

  Data frame with heading and group columns.

- group_col:

  Column identifying conditions or groups.

- angle_col:

  Heading column in radians. Default `"heading"`.

- parametric:

  Logical. `TRUE` (default) uses `equal.kappa.test`; `FALSE` uses
  `wallraff.test`.

## Value

One-row tidy data frame with `statistic`, `df` (parametric only),
`p_value`, and `test`.
