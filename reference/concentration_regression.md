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
