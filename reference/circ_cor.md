# Circular correlation between headings and a covariate

Computes the association between a heading (angle) series and either a
continuous linear covariate (`x_type = "linear"`, default) or a second
set of angles (`x_type = "circular"`).

## Usage

``` r
circ_cor(
  hd,
  x_col,
  angle_col = "heading",
  group_col = NULL,
  x_type = c("linear", "circular"),
  test = TRUE
)
```

## Arguments

- hd:

  Data frame containing the heading and covariate columns.

- x_col:

  Name of the covariate column.

- angle_col:

  Heading column in radians. Default `"heading"`.

- group_col:

  Column to group by. `NULL` uses all rows.

- x_type:

  `"linear"` (default) or `"circular"`.

- test:

  Logical; include hypothesis test. Default `TRUE`.

## Value

Tidy data frame with columns `group_col` (if supplied), `r`, `n`,
`type`, and when `test = TRUE` also `statistic`, `df`, `p_value`.

## Details

**Circular-linear** (T-linear association, Mardia \\ Jupp 2000): \$\$r^2
= \frac{r\_{cx}^2 + r\_{cy}^2 - 2r\_{cx}r\_{cy}r\_{xy}}{1 -
r\_{xy}^2}\$\$ where \\r\_{cx} = \text{cor}(\cos\theta, x)\\, \\r\_{cy}
= \text{cor}(\sin\theta, x)\\, \\r\_{xy} = \text{cor}(\cos\theta,
\sin\theta)\\. \\r \in \[0, 1\]\\; the test statistic \\n r^2 \sim
\chi^2_2\\ under \\H_0\\. Note: \\r\\ is unsigned (association strength
only, not direction).

**Circular-circular** (Fisher's \\\rho\\, via
[`cor.circular`](https://rdrr.io/pkg/circular/man/cor.circular.html)):
\\r \in \[-1, 1\]\\.
