# Register a custom heading derivation rule

Adds a named function to the heading rule registry so it can be called
by
[`derive_headings`](https://johnkirwan.github.io/radiatR/reference/derive_headings.md)
via `rule = "name"`. The function must accept `(df, cols, ...)` and
return a data frame with columns `id`, `time`, and `heading` (radians).

## Usage

``` r
register_heading_rule(name, fun, overwrite = FALSE)
```

## Arguments

- name:

  Character; unique rule name.

- fun:

  Function with signature `function(df, cols, ...)`.

- overwrite:

  Logical; replace an existing rule of the same name.

## Value

The rule name, invisibly.

## See also

[`list_heading_rules`](https://johnkirwan.github.io/radiatR/reference/list_heading_rules.md),
[`derive_headings`](https://johnkirwan.github.io/radiatR/reference/derive_headings.md)

## Examples

``` r
last_bearing <- function(df, cols, ...) {
  n <- nrow(df)
  data.frame(id = df[[cols$id]][n], time = df[[cols$time]][n],
             heading = atan2(df[[cols$y]][n], df[[cols$x]][n]) %% (2 * pi))
}
register_heading_rule("last_bearing", last_bearing, overwrite = TRUE)
head(derive_headings(cpunctatus, rule = "last_bearing"))
#> # A tibble: 6 × 3
#>   id       time heading
#>   <chr>   <int>   <dbl>
#> 1 10_10_1   299    4.77
#> 2 10_11_1    78    1.41
#> 3 10_12_1    49    2.25
#> 4 10_13_1   101    4.73
#> 5 10_14_1    30    1.94
#> 6 10_15_1    55    5.03
```
