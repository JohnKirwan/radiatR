# Von Mises regression of concentration on covariates

Fits a von Mises model in which the concentration \\\kappa\\ varies with
one or more covariates while the mean direction \\\mu\\ is a single
constant: \\\theta_i \sim \mathrm{vM}(\mu, \kappa_i)\\ with \\\kappa_i =
h(x_i'\gamma)\\ for an inverse link \\h\\. It is the
concentration-regression complement to
[`circ_regression`](https://johnkirwan.github.io/radiatR/reference/circ_regression.md)
(which models the mean with a constant concentration), and the estimator
that pairs with
[`simulate_tracks`](https://johnkirwan.github.io/radiatR/reference/simulate_tracks.md)'s
`concentration_slope`. The model is fitted by maximum likelihood with
[`optim`](https://rdrr.io/r/stats/optim.html).

## Usage

``` r
concentration_regression(
  data,
  formula,
  link = c("log", "identity"),
  init = NULL
)

# S3 method for class 'concentration_regression'
summary(object, conf.level = 0.95, ...)

# S3 method for class 'concentration_regression'
predict(object, newdata = NULL, ...)

# S3 method for class 'concentration_regression'
fitted(object, ...)

# S3 method for class 'concentration_regression'
print(x, ...)
```

## Arguments

- data:

  A data frame containing the response and predictor columns.

- formula:

  A formula `heading ~ x1 + x2`; the LHS is the angle column (radians),
  the RHS the covariate(s) modelling \\\kappa\\. The intercept is
  retained (it is the baseline concentration on the link scale).

- link:

  Link relating \\\kappa\\ to the linear predictor: `"log"` (default;
  \\\kappa = e^{x'\gamma}\\, always positive) or `"identity"` (\\\kappa
  = x'\gamma\\, floored at a small positive value).

- init:

  Optional numeric starting values for the concentration coefficients
  (length = number of design-matrix columns, intercept first).

- object:

  A `concentration_regression` object.

- conf.level:

  Confidence level for the coefficient interval. Default 0.95.

- ...:

  Unused.

- newdata:

  Optional data frame of new covariate values. Default uses the training
  data.

- x:

  A `concentration_regression` object.

## Value

An S3 object of class `"concentration_regression"`. Use
[`summary()`](https://rdrr.io/r/base/summary.html) for a tidy
coefficient data frame (link scale),
[`predict()`](https://rdrr.io/r/stats/predict.html) /
[`fitted()`](https://rdrr.io/r/stats/fitted.values.html) for fitted
\\\kappa\\ (response scale), and
[`print()`](https://rdrr.io/r/base/print.html) for a compact report. On
non-convergence or too few rows, `converged` is `FALSE` and the
coefficients are `NA`.

## References

Mardia, K. V. & Jupp, P. E. (2000). *Directional Statistics*. Wiley.

## See also

[`circ_regression`](https://johnkirwan.github.io/radiatR/reference/circ_regression.md),
[`boot_kappa_ci`](https://johnkirwan.github.io/radiatR/reference/boot_kappa_ci.md),
[`test_concentration`](https://johnkirwan.github.io/radiatR/reference/test_concentration.md),
[`simulate_tracks`](https://johnkirwan.github.io/radiatR/reference/simulate_tracks.md)

## Examples

``` r
# Does orientation sharpen as the stimulus arc widens?
hd <- derive_headings(cpunctatus, rule = "distal", carry = "arc",
                      coords = "relative")
hd$arc_deg <- as.numeric(as.character(hd$arc))
fit <- concentration_regression(hd, heading ~ arc_deg)
fit
#> Von Mises concentration regression (kappa ~ covariates)
#>   link = log, mu = 0.05412, n = 235, converged = TRUE
#>          term    estimate         se statistic      p_value    conf.low
#> 1 (Intercept) -2.61654159 0.72101041 -3.628993 0.0002845292 -4.02969603
#> 2     arc_deg  0.06127567 0.01628527  3.762643 0.0001681271  0.02935712
#>     conf.high
#> 1 -1.20338715
#> 2  0.09319422
summary(fit)
#>          term    estimate         se statistic      p_value    conf.low
#> 1 (Intercept) -2.61654159 0.72101041 -3.628993 0.0002845292 -4.02969603
#> 2     arc_deg  0.06127567 0.01628527  3.762643 0.0001681271  0.02935712
#>     conf.high
#> 1 -1.20338715
#> 2  0.09319422
# Fitted kappa for a narrow, medium and wide arc
predict(fit, newdata = data.frame(arc_deg = c(5, 25, 50)))
#> [1] 0.09924505 0.33802013 1.56399261
```
