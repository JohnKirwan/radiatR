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
