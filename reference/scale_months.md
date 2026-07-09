# Circumference scale: months of the year

Circumference scale: months of the year

## Usage

``` r
scale_months(format = c("abbr", "initial", "number"))
```

## Arguments

- format:

  \`"abbr"\` (Jan...Dec, default), \`"initial"\` (J, F, M, ...), or
  \`"number"\` (1...12).

## Value

A circumference-scale list for \[circumference_labs()\].

## See also

\[circumference_labs()\], \[scale_cardinal()\], \[scale_clock()\],
\[scale_seconds()\]

## Examples

``` r
scale_months("initial")
#> $n
#> [1] 12
#> 
#> $at
#>  [1] 1.5707963 1.0471976 0.5235988 0.0000000 5.7595865 5.2359878 4.7123890
#>  [8] 4.1887902 3.6651914 3.1415927 2.6179939 2.0943951
#> 
#> $labels
#>  [1] "J" "F" "M" "A" "M" "J" "J" "A" "S" "O" "N" "D"
#> 
#> $name
#> [1] "months"
#> 
```
