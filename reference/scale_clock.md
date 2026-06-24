# Circumference scale: clock hours

Draws a tick at every hour and labels a sparse subset.

## Usage

``` r
scale_clock(hours = 24, every = NULL)
```

## Arguments

- hours:

  \`24\` (default) or \`12\`.

- every:

  Label every \`every\` hours. \`NULL\` (default) auto-picks \`6\` for a
  24-hour dial and \`3\` for a 12-hour dial. Must evenly divide
  \`hours\`.

## Value

A circumference-scale list for \[circumference_labs()\].

## See also

\[circumference_labs()\], \[scale_cardinal()\], \[scale_months()\],
\[scale_seconds()\]

## Examples

``` r
scale_clock(hours = 12)
#> $n
#> [1] 12
#> 
#> $at
#> [1] 1.570796 0.000000 4.712389 3.141593
#> 
#> $labels
#> [1] "0" "3" "6" "9"
#> 
#> $name
#> [1] "clock"
#> 
```
