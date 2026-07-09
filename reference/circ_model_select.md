# Select among candidate circular models by AICc

Fits five candidate models to a heading sample and ranks them by the
small-sample-corrected Akaike information criterion (AICc):

- uniform:

  no preferred direction (k = 0).

- unimodal:

  one von Mises mode (k = 2).

- axial:

  symmetric bimodal von Mises, two equal antipodal modes (k = 2).

- unimodal_uniform:

  a directed von Mises mode over a uniform background,
  \\p\\vM(\mu,\kappa) + (1-p)\\U\\ (k = 3).

- bimodal:

  an asymmetric two-component von Mises mixture with free means,
  concentrations, and weight (k = 5).

The mixture models (`unimodal_uniform`, `bimodal`) are fitted by
numerical maximum likelihood; the other three are closed-form. This
model set mirrors the Schnute-Groot family implemented by the CircMLE
package (Fitak & Johnsen 2017): `uniform` = M1, `unimodal` = single von
Mises, `axial` = symmetric bimodal, `unimodal_uniform` = a directed
component over a uniform background, and `bimodal` = a free
two-component mixture.

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

Tidy data frame, one row per candidate model (five rows, or five per
group when `group_col` is supplied), sorted by `AICc` ascending (best
first; `NA` last). Columns: `group_col` (if supplied), `model`, `n`, `k`
(free parameters), `logLik`, `AIC`, `AICc`, `BIC`, `dAICc` (AICc minus
the group minimum), and `weight` (Akaike weight). `AICc`/`weight` are
`NA` for a model whose fit failed or when `n - k - 1 <= 0`; remaining
weights sum to 1.

## Details

Parameters for `uniform`, `unimodal`, and `axial` are estimated with
[`vonmises_fit`](https://johnkirwan.github.io/radiatR/reference/vonmises_fit.md)
(the axial model via its `axial = TRUE` doubled-angle fit) and
likelihoods with the `circular` package densities. The table reports
model comparison only; obtain the fitted parameters of a chosen model
from
[`vonmises_fit`](https://johnkirwan.github.io/radiatR/reference/vonmises_fit.md).

## References

Burnham, K.P. & Anderson, D.R. (2002). Model Selection and Multimodel
Inference, 2nd ed. Springer.

Schnute, J.T. & Groot, K. (1992). Statistical analysis of animal
orientation data. Animal Behaviour, 43(1), 15-33.

Fitak, R.R. & Johnsen, S. (2017). Bringing the analysis of animal
orientation data full circle: model-based approaches with maximum
likelihood. Journal of Experimental Biology, 220(21), 3878-3882.

## See also

[`vonmises_fit`](https://johnkirwan.github.io/radiatR/reference/vonmises_fit.md),
[`test_uniformity`](https://johnkirwan.github.io/radiatR/reference/test_uniformity.md)
