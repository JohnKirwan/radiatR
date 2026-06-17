# Select among candidate circular models by AICc

Fits three candidate models to a heading sample and ranks them by the
small-sample-corrected Akaike information criterion (AICc): a `uniform`
distribution (no preferred direction), a `unimodal` von Mises (one
preferred direction), and an `axial` (symmetric bimodal) von Mises (a
preferred axis, two equal antipodal modes). Answers whether a sample is
best described as uniform, directionally, or axially oriented.

## Usage

``` r
circ_model_select(hd, angle_col = "heading", group_col = NULL)
```

## Arguments

- hd:

  Data frame with a heading column in radians.

- angle_col:

  Heading column name. Default `"heading"`.

- group_col:

  Column to group by. `NULL` (default) treats the whole data frame as
  one sample.

## Value

Tidy data frame, one row per candidate model (per group when `group_col`
is supplied), sorted by `AICc` ascending (best first; `NA` last).
Columns: `group_col` (if supplied), `model`, `n`, `k` (free parameters),
`logLik`, `AIC`, `AICc`, `BIC`, `dAICc` (AICc minus the group minimum),
and `weight` (Akaike weight). `AICc`/`weight` are `NA` for a model whose
fit failed or when `n - k - 1 <= 0`; remaining weights sum to 1.

## Details

Parameters are estimated with
[`vonmises_fit`](https://johnkirwan.github.io/radiatR/reference/vonmises_fit.md)
(the axial model via its `axial = TRUE` doubled-angle fit) and
likelihoods with the `circular` package densities. The table reports
model comparison only; obtain the fitted parameters of a chosen model
from
[`vonmises_fit`](https://johnkirwan.github.io/radiatR/reference/vonmises_fit.md).

## References

Burnham, K.P. & Anderson, D.R. (2002). Model Selection and Multimodel
Inference, 2nd ed. Springer.

## See also

[`vonmises_fit`](https://johnkirwan.github.io/radiatR/reference/vonmises_fit.md),
[`test_uniformity`](https://johnkirwan.github.io/radiatR/reference/test_uniformity.md)
