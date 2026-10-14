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

## Examples

``` r
hd <- derive_headings(cpunctatus, rule = "distal", carry = "arc",
                      coords = "relative")
# Control (no stimulus) against the widest arc
circ_model_select(hd[hd$arc %in% c("0", "50"), ], group_col = "arc")
#>    arc            model  n k    logLik       AIC      AICc       BIC      dAICc
#> 4   50 unimodal_uniform 26 3 -25.31838  56.63676  57.72767  60.41105  0.0000000
#> 5   50          bimodal 26 5 -25.01426  60.02851  63.02851  66.31900  5.3008401
#> 3   50            axial 26 2 -35.64951  75.29902  75.82076  77.81522 18.0930879
#> 2   50         unimodal 26 2 -39.71644  83.43288  83.95462  85.94908 26.2269499
#> 1   50          uniform 26 0 -47.78480  95.56961  95.56961  95.56961 37.8419340
#> 11   0          uniform 35 0 -64.32570 128.65139 128.65139 128.65139  0.0000000
#> 41   0 unimodal_uniform 35 3 -61.16608 128.33216 129.10635 132.99820  0.4549588
#> 31   0            axial 35 2 -63.36687 130.73373 131.10873 133.84443  2.4573380
#> 21   0         unimodal 35 2 -64.15780 132.31560 132.69060 135.42630  4.0392093
#> 51   0          bimodal 35 5        NA        NA        NA        NA         NA
#>          weight
#> 4  9.339324e-01
#> 5  6.595574e-02
#> 3  1.100149e-04
#> 2  1.884541e-06
#> 1  5.662965e-09
#> 11 4.500595e-01
#> 41 3.584898e-01
#> 31 1.317242e-01
#> 21 5.972646e-02
#> 51           NA
```
