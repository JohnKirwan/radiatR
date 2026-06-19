# Circular-linear regression of a heading on linear covariates

Fits the Fisher-Lee circular-linear regression \\\theta_i \sim
\mathrm{vM}(\mu_0 + 2\arctan(x_i'\beta), \kappa)\\ via
[`lm.circular`](https://rdrr.io/pkg/circular/man/lm.circular.html)
(`type = "c-l"`), behind a formula interface. The response is a heading
in radians (unit-circle convention); the right-hand side supplies one or
more linear covariates (factors and interactions are expanded by
`model.matrix`).

## Usage

``` r
circ_regression(data, formula, init = NULL)

# S3 method for class 'circ_regression'
summary(object, conf.level = 0.95, ...)

# S3 method for class 'circ_regression'
predict(object, newdata = NULL, ...)

# S3 method for class 'circ_regression'
fitted(object, ...)

# S3 method for class 'circ_regression'
print(x, ...)
```

## Arguments

- data:

  A data frame containing the response and predictor columns.

- formula:

  A formula \`heading ~ x1 + x2\`; the LHS is the angle column.

- init:

  Optional numeric starting values for the slope coefficients (length =
  number of predictor columns). Default a vector of zeros.

- object:

  A `circ_regression` object.

- conf.level:

  Confidence level for the coefficient interval. Default 0.95.

- ...:

  Unused.

- newdata:

  Optional data frame of new covariate values. Default uses the training
  data.

- x:

  A `circ_regression` object.

## Value

An S3 object of class `"circ_regression"`. Use
[`summary()`](https://rdrr.io/r/base/summary.html) for a tidy
coefficient data frame,
[`predict()`](https://rdrr.io/r/stats/predict.html) /
[`fitted()`](https://rdrr.io/r/stats/fitted.values.html) for fitted mean
angles, and [`print()`](https://rdrr.io/r/base/print.html) for a compact
report. On non-convergence or too few rows, `converged` is `FALSE` and
the coefficients are `NA`.

## References

Fisher, N. I. & Lee, A. J. (1992). Regression models for an angular
response. *Biometrics* 48, 665-677. Mardia, K. V. & Jupp, P. E. (2000).
*Directional Statistics*. Wiley.

## See also

[`circ_cor`](https://johnkirwan.github.io/radiatR/reference/circ_cor.md),
[`vonmises_fit`](https://johnkirwan.github.io/radiatR/reference/vonmises_fit.md),
[`simulate_tracks`](https://johnkirwan.github.io/radiatR/reference/simulate_tracks.md)
