# Circumference scale: cardinal compass directions

Circumference scale: cardinal compass directions

## Usage

``` r
scale_cardinal(points = 4)
```

## Arguments

- points:

  \`4\` (N/E/S/W, default) or \`8\` (adds the intercardinals
  NE/SE/SW/NW).

## Value

A circumference-scale list for \[circumference_labs()\].

## See also

\[circumference_labs()\], \[scale_clock()\], \[scale_months()\],
\[scale_seconds()\]

## Examples

``` r
scale_cardinal(points = 8)
#> $n
#> [1] 8
#> 
#> $at
#> [1] 1.5707963 0.7853982 0.0000000 5.4977871 4.7123890 3.9269908 3.1415927
#> [8] 2.3561945
#> 
#> $labels
#> [1] "N"  "NE" "E"  "SE" "S"  "SW" "W"  "NW"
#> 
#> $name
#> [1] "cardinal"
#> 
```
